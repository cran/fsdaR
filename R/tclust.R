tclustfsda <- function(x, k, alpha, restrfactor=12, monitoring = FALSE, plot=FALSE,
        control, trace=FALSE, ...)
{
    ## If 'control' not supplied, create a default control, according to
    ##  'monitoring', 'family' and 'method'. Use as a switch for calling the Matlab functions
    ##  the 'outclass' element of the control object.
    ##  Remove 'outclass' from the control object - not to disturb the mapping to Java.
    if(missing(control))
        control <- list() #.defaultControl(monitoring, family="mult", method="MM")

    control$plots <- ifelse(plot, 1, 0)
    outclass <- if(monitoring) "tclusteda" else "tclust"
    ##  control$outclass
    ##  control$outclass <- NULL

    if(missing(alpha))
        alpha <- if(!monitoring) 0 else c(0.1, 0.05, 0)

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
    parlist = c(parlist, .jnew("java/lang/Double", as.double(k)))           # k
    if(length(alpha) == 1)
        parlist = c(parlist, .jnew("java/lang/Double", as.double(alpha)))       # alpha
    else
        parlist = c(parlist, .jarray(alpha, dispatch=TRUE))

    parlist = c(parlist, .jnew("java/lang/Double", as.double(restrfactor))) # restrfactor

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

    if(!monitoring)
    {
        out <- callFsdaFunction("tclust", "[Ljava/lang/Object;", 1, parlist)
        if(is.null(out))
            return(NULL)

        arr1 = .jcast(out[[1]], "com/mathworks/toolbox/javabuilder/MWStructArray")
        arr = .jnew("org/jrc/ipsc/globesec/sitaf/fsda/FsdaMWStructArray", arr1)

        if(trace)
        {
            cat("\nReturning from MATLAB tclust().  Fields returned by MATLAB: \n")
            print(arr$fieldNames())
        }

        muopt = as.vector(as.matrix(.jevalArray(arr$get("muopt", as.integer(1)), "[[D", simplify = TRUE)))
        sigmaopt = as.vector(as.matrix(.jevalArray(arr$get("sigmaopt", as.integer(1)), "[[D", simplify = TRUE)))
        idx = as.vector(as.matrix(.jevalArray(arr$get("idx", as.integer(1)), "[[D", simplify = TRUE)))
        size = as.vector(as.matrix(.jevalArray(arr$get("siz", as.integer(1)), "[[D", simplify = TRUE)))

        fullsol <- if(as.integer(arr$hasField("fullsol", as.integer(1))) != 1) NULL
                    else as.matrix(.jevalArray(arr$get("fullsol", as.integer(1)), "[[D", simplify = TRUE))
        Y <- if(as.integer(arr$hasField("Y", as.integer(1))) != 1) NULL
                    else as.matrix(.jevalArray(arr$get("Y", as.integer(1)), "[[D", simplify = TRUE))

        ans = list(call=match.call(), muopt=muopt, sigmaopt=sigmaopt, idx=idx, size=size, fullsol=fullsol, X=Y)

    } else
    {
        out <- callFsdaFunction("tclusteda", "[Ljava/lang/Object;", 1, parlist)
        if(is.null(out))
            return(NULL)

        arr1 = .jcast(out[[1]], "com/mathworks/toolbox/javabuilder/MWStructArray")
        arr = .jnew("org/jrc/ipsc/globesec/sitaf/fsda/FsdaMWStructArray", arr1)

        if(trace)
        {
            cat("\nReturning from MATLAB tclusteda().  Fields returned by MATLAB: \n")
            print(arr$fieldNames())
        }

        MU = as.vector(as.matrix(.jevalArray(arr$get("MU", as.integer(1)), "[[D", simplify = TRUE)))
        SIGMA = as.vector(as.matrix(.jevalArray(arr$get("SIGMA", as.integer(1)), "[[D", simplify = TRUE)))
        IDX = as.vector(as.matrix(.jevalArray(arr$get("IDX", as.integer(1)), "[[D", simplify = TRUE)))
        Amon = as.vector(as.matrix(.jevalArray(arr$get("Amon", as.integer(1)), "[[D", simplify = TRUE)))
        Y <- if(as.integer(arr$hasField("Y", as.integer(1))) != 1) NULL
                    else as.matrix(.jevalArray(arr$get("Y", as.integer(1)), "[[D", simplify = TRUE))

        ans = list(call=match.call(), MU=MU, SIGMA=SIGMA, IDX=IDX, Amon=Amon, X=Y)

    }
    freeMatlabResources(out)

    class(ans) <- outclass
    return (ans)
}
