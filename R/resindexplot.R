resindexplot <- function(out, ...)
{
    ## Perform check on the structure of 'out'
    if(missing(out))
        stop("Input residuals missing.")
    if(is.list(out))
    {
        ## The needed elements are residuals, y and X. The R class name is mapped to a Matlab class name
        outStr <- list(residuals=out$residuals, y=out$y, X=out$X, class=getMatlabClass(class(out)))
        if(is.null(outStr$residuals) || is.null(outStr$y) || is.null(outStr$X))
            stop("One or more required arguments are missing.")
    }else
        outStr <- list(residuals=as.vector(out))

    control = list(...)

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
    matlabResult <- callFsdaFunctionNoArgout("resindexplot", "[Ljava/lang/Object;", matlabParams)
    if(is.null(matlabResult))
        return(NULL)

    ans = list()

    # Libera le risorse MATLAB create a runtime
    freeMatlabResources(out)

    return(invisible(ans))
}
