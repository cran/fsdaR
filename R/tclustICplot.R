tclustICplot <- function(out, whichIC=c("ALL", "MIXMIX", "MIXCLA", "CLACLA"),

##quant=c(0.01, 0.5, 0.99), exact=0, sign=TRUE, mplus1=FALSE, envm,
##    xlim, ylim, xlab, ylab, main,
##    lwdenv, lwd, cex.lab, cex.axis,
##    tag, datatooltip, label, nameX, namey, databrush,
    ...)
{

    ## Perform check on the structure of 'out'
    if(missing(out) | !inherits(out, "tclustic"))
        stop("Function defined only for output of tclustIC().")

    whichIC = match.arg(whichIC)
    ## The needed elements are ...
##    outStr <- list(mmdrs=out$mmdrs, BBrs=out$BBrs, Y=out$X, class=getMatlabClass(class(out)))
##    if(is.null(outStr$mmdrs) || is.null(outStr$BBrs) || is.null(outStr$Y))
##        stop("One or more required arguments are missing.")

    # if (!is.null(out$IDXCLA)) 
    #   out$IDXCLA = rewrapComplexNumericCellArray(out$IDXCLA)
    # 
    # if (!is.null(out$IDXMIX)) 
    #   out$IDXCLA = rewrapComplexNumericCellArray(out$IDXMIX)
    
    outStr <- out
    control = list(whichIC=whichIC)

if(FALSE)
{
     if(!missing(envm))
        control$envm <- envm


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
    matlabResult <- callFsdaFunctionNoArgout("tclustICplot", "[Ljava/lang/Object;", matlabParams)
    ans = list()

    freeMatlabResources(out)
    return(invisible(ans))
}

carbikeplot <- function(out,

##quant=c(0.01, 0.5, 0.99), exact=0, sign=TRUE, mplus1=FALSE, envm,
##    xlim, ylim, xlab, ylab, main,
##    lwdenv, lwd, cex.lab, cex.axis,
##    tag, datatooltip, label, nameX, namey, databrush,
    ...)
{

    ## Perform check on the structure of 'out'
    if(missing(out) | !inherits(out, "tclusticsol"))
        stop("Function defined only for output of tclustICsol().")

    ## The needed elements are ...
##    outStr <- list(mmdrs=out$mmdrs, BBrs=out$BBrs, Y=out$X, class=getMatlabClass(class(out)))
##    if(is.null(outStr$mmdrs) || is.null(outStr$BBrs) || is.null(outStr$Y))
##        stop("One or more required arguments are missing.")

    outStr <- out
    control = list()

if(FALSE)
{
     if(!missing(envm))
        control$envm <- envm


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
    out <- callFsdaFunction("carbikeplot", "[Ljava/lang/Object;", 1, parlist)
    ans = list()

    freeMatlabResources(out)
    return(invisible(ans))
}
