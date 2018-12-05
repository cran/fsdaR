tclustIC <- function(x, kk, cc, alpha, whichIC=c("ALL", "MIXMIX", "MIXCLA", "CLACLA"),
        nsamps, refsteps,
        control, trace=FALSE, ...)
{
    outclass <- "tclustic"

    whichIC = match.arg(whichIC)
    
    if(missing(control))
        control <- list(whichIC=whichIC)

    if(!missing(kk))
        control$kk <- kk
    if(!missing(cc))
        control$cc <- cc
    if(!missing(alpha))
        control$alpha <- alpha
    if(!missing(nsamps))
        control$nsamps <- nsamps
    if(!missing(refsteps))
        control$refsteps <- refsteps

    ## ES 27.06.2018: parameters that are mandatory to the MATLAB function
    ## cannot be put into the MATLAB function because they have to be supplied
    ## to the function individually and not in (name, value) pairs

    ## Check if the control corresponds to the parameter supplied: monitoring, family and method:
##    defc <- .defaultControl(monitoring=monitoring, family=family, method=method)
##    if(class(defc) != class(control))
##        stop(paste0("Wrong control object provided: '", class(control), "'. Must be ", class(defc), "."))

	if(is.data.frame(x))
	    x <- data.matrix(x)
	else if(!is.matrix(x))
	    x <- matrix(x, length(x), 1,
			dimnames = list(names(x), deparse(substitute(x))))
    if(!is.numeric(x)) stop("x is not a numeric")

    dx <- dim(x)
    xn <- (dnx <- dimnames(x))[[2]]
    xn <- if (!is.null(xn))
        xn
    else if (dx[2] > 1)
        paste("X", 1:dx[2], sep = "")
    else if(dx[2])
        "X"
    dimnames(x) <- list(dnx[[1]], xn)

    n <- nrow(x)
    p <- ncol(x)

    ## ES 27.06.2018: parameters that are mandatory to the MATLAB function
    ## cannot be put into the MATLAB function because they have to be supplied
    ## to the function individually and not in (name, value) pairs.
    ## Mandatory parameters to the MATLAB FSDA function
    parlist = c(.jarray(x, dispatch=TRUE))

    paramNames = names(control)
    if(length(paramNames) > 0)
    {
        for (i in 1:length(paramNames)) {
            paramName = paramNames[i]
            paramValue = control[[i]]

            matlabValue = rType2MatlabType(paramName, paramValue)
            parlist = c(parlist, .jnew("java/lang/String", paramName), matlabValue)
        }
    }

    out <- callFsdaFunction("tclustIC", "[Ljava/lang/Object;", 1, parlist)
    if(is.null(out))
        return(NULL)

    arr1 = .jcast(out[[1]], "com/mathworks/toolbox/javabuilder/MWStructArray")
    arr = .jnew("org/jrc/ipsc/globesec/sitaf/fsda/FsdaMWStructArray", arr1)

    if(trace)
    {
        cat("\nReturning from MATLAB tclust().  Fields returned by MATLAB: \n")
        print(arr$fieldNames())
    }

    CLACLA <- if(as.integer(arr$hasField("CLACLA", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("CLACLA", as.integer(1)), "[[D", simplify = TRUE))
    # M_IDXCLA <- if(as.integer(arr$hasField("IDXCLA", as.integer(1))) != 1) NULL
    #           else arr$get("IDXCLA", as.integer(1))
    IDXCLA <- if(as.integer(arr$hasField("IDXCLA", as.integer(1))) != 1) NULL
              else unwrapComplexNumericCellArray(as.matrix(.jevalArray(arr$get("IDXCLA", as.integer(1)))))
    MIXMIX <- if(as.integer(arr$hasField("MIXMIX", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("MIXMIX", as.integer(1)), "[[D", simplify = TRUE))
    MIXCLA <- if(as.integer(arr$hasField("MIXCLA", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("MIXCLA", as.integer(1)), "[[D", simplify = TRUE))
    MIXCLA <- if(as.integer(arr$hasField("MIXCLA", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("MIXCLA", as.integer(1)), "[[D", simplify = TRUE))
    # M_IDXMIX <- if(as.integer(arr$hasField("IDXMIX", as.integer(1))) != 1) NULL
    #           else arr$get("IDXMIX", as.integer(1))
    IDXMIX <- if(as.integer(arr$hasField("IDXMIX", as.integer(1))) != 1) NULL
              else unwrapComplexNumericCellArray(as.matrix(.jevalArray(arr$get("IDXMIX", as.integer(1)))))
    kk_ret <- as.vector(as.matrix(.jevalArray(arr$get("kk", as.integer(1)), "[[D", simplify = TRUE)))
    cc_ret <- as.vector(as.matrix(.jevalArray(arr$get("cc", as.integer(1)), "[[D", simplify = TRUE)))
    alpha_ret = as.vector(as.matrix(.jevalArray(arr$get("alpha", as.integer(1)), "[[D", simplify = TRUE)))[1]

    Y <- if(as.integer(arr$hasField("Y", as.integer(1))) != 1) NULL
                else as.matrix(.jevalArray(arr$get("Y", as.integer(1)), "[[D", simplify = TRUE))

    ans = list(call=match.call(), CLACLA=CLACLA, IDXCLA=IDXCLA, MIXMIX=MIXMIX, MIXCLA=MIXCLA, IDXMIX=IDXMIX,
            kk=kk_ret, cc=cc_ret, alpha=alpha_ret, whichIC=whichIC, Y=Y)

    freeMatlabResources(out)

    class(ans) <- outclass
    return (ans)
}

tclustICsol <- function(out, trace=FALSE, plot=FALSE, 

##quant=c(0.01, 0.5, 0.99), exact=0, sign=TRUE, mplus1=FALSE, envm,
##    xlim, ylim, xlab, ylab, main,
##    lwdenv, lwd, cex.lab, cex.axis,
##    tag, datatooltip, label, nameX, namey, databrush,
    ...)
{

    outclass <- "tclusticsol"

    ## Perform check on the structure of 'out'
    if(missing(out) | !inherits(out, "tclustic"))
        stop("Function defined only for the output of 'tclistIC()'.")

    ## The needed elements are MAL, Un, Bols, y and X. The R class name is mapped to a Matlab class name
##    outStr <- list(mmdrs=out$mmdrs, BBrs=out$BBrs, Y=out$X, class=getMatlabClass(class(out)))
##    if(is.null(outStr$mmdrs) || is.null(outStr$BBrs) || is.null(outStr$Y))
##        stop("One or more required arguments are missing.")

    outStr <- out

    control = list(...)
    control$plots <- ifelse(plot, 1, 0)

if(FALSE)
{
    control$quant <- quant
    control$exact <- exact
    control$sign <- ifelse(sign, 1, 0)
    control$mplus1 <- ifelse(mplus1, 1, 0)
    if(!missing(envm))
        control$envm <- envm

    if(!missing(xlim))
        control$xlimx <- xlim
    if(!missing(ylim))
        control$ylimy <- ylim
    if(!missing(xlab))
        control$labx <- xlab
    if(!missing(ylab))
        control$laby <- ylab
    if(!missing(main))
        control$titl <- main
    if(!missing(lwdenv))
        control$lwdenv <- lwdenv
    if(!missing(lwd))
        control$lwd <- lwd
    if(!missing(tag))
        control$tag <- as.character(tag)
    if(!missing(datatooltip))
        control$datatooltip <- datatooltip
    if(!missing(label))
        control$label <- label
    if(!missing(nameX))
        control$nameX <- nameX
    if(!missing(namey))
        control$namey <- namey
    if(!missing(databrush))
        control$databrush <- databrush

    if(!missing(cex.axis))
    {
        control$SizeAxesNum <- 10  ## the default
        control$SizeAxesNum <- cex.axis * control$SizeAxesNum
    }
    if(!missing(cex.lab))
    {
        control$FontSize <- 12  ## the default
        control$FontSize <- cex.lab * control$FontSize
    }

}

    # Initialize parlist to the input values. All parameters from the control structure
    # must be added to the R list *before* converting it into a MATLAB struct

    ## VT::16.10.2017: the mandatory input parameters are all contained
    ##  in an S3 class (a list), which is the output of one of the XXXeda() fucntions.
    ##  Convert the S3 class to a Matlab structure and initialize with it the parameter list.
    ##  NOTE: this is identical to the initialization of the parameter list in fsreg()
    ##      with the mandatory X and y.
    ##
    parlist <- list(list2MatlabStruct(outStr))
    paramNames = names(control)

    if (length(paramNames) > 0) {
        for (i in 1:length(paramNames)) {
            paramName = paramNames[i];
            paramValue = control[[i]];
            matlabValue = rType2MatlabType(paramName, paramValue)
            parlist = c(parlist, .jnew("java/lang/String", paramName), matlabValue)
        }
    }

    matlabParams <- parlist
    out <- callFsdaFunction("tclustICsol", "[Ljava/lang/Object;", 1, parlist)

    if(is.null(out))
        return(NULL)

    arr1 = .jcast(out[[1]], "com/mathworks/toolbox/javabuilder/MWStructArray")
    arr = .jnew("org/jrc/ipsc/globesec/sitaf/fsda/FsdaMWStructArray", arr1)

    if(trace)
    {
        cat("\nReturning from MATLAB tclust().  Fields returned by MATLAB: \n")
        print(arr$fieldNames())
    }

    MIXMIXbs <- if(as.integer(arr$hasField("MIXMIXbs", as.integer(1))) != 1) NULL
              else .jevalArray(arr$get("MIXMIXbs", as.integer(1)), "[[D", simplify = TRUE)
    MIXMIXbsari <- if(as.integer(arr$hasField("MIXMIXbsari", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("MIXMIXbsari", as.integer(1)), "[[D", simplify = TRUE))
    ARIMIX <- if(as.integer(arr$hasField("ARIMIX", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("ARIMIX", as.integer(1)), "[[D", simplify = TRUE))

    MIXCLAbs <- if(as.integer(arr$hasField("MIXCLAbs", as.integer(1))) != 1) NULL
              else .jevalArray(arr$get("MIXCLAbs", as.integer(1)), "[[D", simplify = TRUE)
    MIXCLAbsari <- if(as.integer(arr$hasField("MIXCLAbsari", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("MIXCLAbsari", as.integer(1)), "[[D", simplify = TRUE))

    CLACLAbs <- if(as.integer(arr$hasField("CLACLAbs", as.integer(1))) != 1) NULL
              else .jevalArray(arr$get("CLACLAbs", as.integer(1)), "[[D", simplify = TRUE)
    CLACLAbsari <- if(as.integer(arr$hasField("CLACLAbsari", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("CLACLAbsari", as.integer(1)), "[[D", simplify = TRUE))
    ARICLA <- if(as.integer(arr$hasField("ARICLA", as.integer(1))) != 1) NULL
              else as.matrix(.jevalArray(arr$get("ARICLA", as.integer(1)), "[[D", simplify = TRUE))

    kk_ret <- as.vector(as.matrix(.jevalArray(arr$get("kk", as.integer(1)), "[[D", simplify = TRUE)))
    cc_ret <- as.vector(as.matrix(.jevalArray(arr$get("cc", as.integer(1)), "[[D", simplify = TRUE)))

    ans = list(call=match.call(), MIXMIXbs=MIXMIXbs, MIXMIXbsari=MIXMIXbsari, ARIMIX=ARIMIX,
                                  MIXCLAbs=MIXCLAbs, MIXCLAbsari=MIXCLAbsari,
                                  CLACLAbs=CLACLAbs, CLACLAbsari=CLACLAbsari, ARICLA=ARICLA,
                                  kk=kk_ret, cc=cc_ret)


    freeMatlabResources(out)
    class(ans) <- outclass
    return(invisible(ans))
}
