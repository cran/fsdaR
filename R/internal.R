## Probably not needed ###############################################################


## Controllo numero pari dei parametri opzionali
assertEvenNumberedOptList <- function(optArgList)
{
    ## Controllo numero pari dei parametri opzionali
    optLen = length(optArgList)
    if (optLen %% 2 != 0)
    {
        stop("Optional argument list must have an even number of parameters")
    }
}

## Converte gli oggetti R in oggetti Java
checkOptArg <- function(optlist, paramName, structfnp)
{
    result = list()

    if (paramName %in% optlist)
    {
        idx = match(paramName, optlist, nomatch = 0)
        param = optlist[[idx+1]]

        if (is.numeric(param) && length(param) == 1)
        {# Pure scalar
            paramValue = as.double(param);
            result = list(.jnew("java/lang/String", paramName), .jnew("java/lang/Double", paramValue))
        } else if (is.numeric(param) && length(param) > 1) { # Vector of scalars
            result = list(.jnew("java/lang/String", paramName), .jarray(param, dispatch=TRUE))
        } else if (is.character(param) && length(param) == 1) { # Single string # !is.vector(param)
            result = list(.jnew("java/lang/String", paramName), .jnew("java/lang/String", param))
        } else if (is.list(param)) { # List(complex structure))
            structmap = structfnp(param)
            result = list(.jnew("java/lang/String", paramName), structmap)
        } else if (is.vector(param) && is.character(param)) {
            cellArr = structfnp(param)
            result = list(.jnew("java/lang/String", paramName), cellArr)
        } #else if (is.vector(param) && is.numeric(param)) {
        #result = .jarray(param, dispatch = TRUE) }
        else {
            stop(paste("Param <", paramName, "> is neither a list nor numeric"))
        }
    }
    return (result)
}

##
checkOptArg <- function(optlist, paramName, structfnp)
{
  result = list()

  if (paramName %in% optlist)
  {
    idx = match(paramName, optlist, nomatch = 0)
    param = optlist[[idx+1]]

    if (is.numeric(param) && length(param) == 1)
    {# Pure scalar
      paramValue = as.double(param);
      result = list(.jnew("java/lang/String", paramName), .jnew("java/lang/Double", paramValue))
    } else if (is.numeric(param) && length(param) > 1) { # Vector of scalars
      result = list(.jnew("java/lang/String", paramName), .jarray(param, dispatch=TRUE))
    } else if (is.character(param) && length(param) == 1) { # Single string # !is.vector(param)
      result = list(.jnew("java/lang/String", paramName), .jnew("java/lang/String", param))
    } else if (is.list(param)) { # List(complex structure))
      structmap = structfnp(param)
      result = list(.jnew("java/lang/String", paramName), structmap)
    } else if (is.vector(param) && is.character(param)) {
      cellArr = structfnp(param)
      result = list(.jnew("java/lang/String", paramName), cellArr)
    } #else if (is.vector(param) && is.numeric(param)) {
    #result = .jarray(param, dispatch = TRUE) }
    else {
      stop(paste("Param <", paramName, "> is neither a list nor numeric"))
    }
  }
  return (result)
}

rType2MatlabType <- function(attrName, attrValue, forceStringsToCellArray = FALSE)
{
  val = NULL
  if (is.numeric(attrValue) && length(attrValue) == 1) {         # Pure scalar value
    if (is.double(attrValue)) {
      val = .jnew("java/lang/Double", as.double(attrValue))
    } else if (is.integer(attrValue)) {
      val = .jnew("java/lang/Integer", as.integer(attrValue))
    }
  } else if (is.numeric(attrValue) && length(attrValue) > 1) {   # Vector of scalars
    val = .jarray((attrValue), dispatch=TRUE)
  }else if (is.character(attrValue) && length(attrValue) == 1) { # Single string
    if (forceStringsToCellArray) {
      val = stringVector2CellArray((attrValue), "")
    } else {
      val = .jnew("java/lang/String", attrValue)
    }
  }else if (is.vector(attrValue) && is.character(attrValue)) {   # Vector of strings
    val = stringVector2CellArray((attrValue), "")
  } else if (is.list(attrValue)) {                               # List (nested)
    #          if (length(attrValue) > 0) {
    val = list2MatlabStruct(attrValue, forceStringsToCellArray)
    #          }
  } else if (is.logical(attrValue)) {
    if (attrValue == TRUE) {
      val = .jnew("java/lang/Boolean", "true")
    } else {
      val = .jnew("java/lang/Boolean", "false")
    }
  } else {
    cat(paste("Warning: attribute <", attrName, "> is of unknown type. Skipping...\n"))
  }

  return(val)
}
##
list2MatlabStruct <- function(rList, forceStringsToCellArray = FALSE)
{
  matlabFields = vector()
  matlabValues = vector()

  if (!is.null(rList) && is.list(rList)) {

    if (length(rList) > 0) {
      for (attrName in names(rList)) {
        attrValue = rList[[attrName]];

        if (!is.null(attrValue)) {
          # val = NULL
          # if (is.numeric(attrValue) && length(attrValue) == 1) {          # Pure scalar value
          #   if (is.double(attrValue)) {
          #     val = .jnew("java/lang/Double", as.double(attrValue))
          #   } else if (is.integer(attrValue)) {
          #     val = .jnew("java/lang/Integer", as.integer(attrValue))
          #   }
          # } else if (is.numeric(attrValue) && length(attrValue) > 1) {    # Vector of scalars
          #   val = .jarray((attrValue), dispatch=TRUE)
          # } else if (is.character(attrValue) && length(attrValue) == 1) { # Single string
          #   val = .jnew("java/lang/String", attrValue)
          # } else if (is.vector(attrValue) && is.character(attrValue)) {   # Vector of strings
          #   val = stringVector2CellArray((attrValue), "")
          # } else if (is.list(attrValue)) {                                # List (nested)
          #   #          if (length(attrValue) > 0) {
          #   val = list2MatlabStruct(attrValue)
          #   #          }
          # } else if (is.logical(attrValue)) {
          #   if (attrValue == TRUE) {
          #     val = .jnew("java/lang/Boolean", "true")
          #   } else {
          #     val = .jnew("java/lang/Boolean", "false")
          #   }
          # } else {
          #   cat(paste("Warning: attribute <", attrName, "> is of unknown type. Skipping...\n"))
          # }

          val = rType2MatlabType(attrName, attrValue, forceStringsToCellArray);

          if (!is.null(val)) {
            matlabFields = c(matlabFields, attrName)
            matlabValues = c(matlabValues, val)
          }
        } else { # List field is null
          cat(paste("Warning: attribute <", attrName, "> is null. Skipping...\n"))
        }

      }

      fieldList = .jarray(matlabFields)

      structArray = new(J("com/mathworks/toolbox/javabuilder/MWStructArray"),
                        as.integer(1),
                        as.integer(1),
                        fieldList)

      for (i in 1:length(matlabFields)) {
        fieldName = .jnew("java/lang/String", matlabFields[i])
        fieldValue = .jcast(matlabValues[[i]], "java/lang/Object")
        structArray$set(fieldName, as.integer(1), fieldValue)
      }
    } else { # Empty list => create empty MWStructArray
      structArray = new(J("com/mathworks/toolbox/javabuilder/MWStructArray"))
    }

    return(structArray)
  } else { # Not a list or null
    stop("Function parameter is null or is not a list")
  }
}

pauseEnter <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

## finds all .R and .r files within a folder and sources them
sourceFolder <- function(folder, recursive = FALSE, ...)
{
  files <- list.files(folder, pattern = "[.][rR]$",
                      full.names = TRUE, recursive = recursive)
  if (!length(files))
    stop(simpleError(sprintf('No R files in folder "%s"', folder)))
  src <- invisible(lapply(files, source, ...))
  message(sprintf('%s files sourced from folder "%s"', length(src), folder))
}

##################################################################################
.initFsdaEngine <- function()
{
    ## VT::29.11.2017
    ## Hack for the R CMD check message "Found the following assignments to the global environment:"
    ##  from https://reformatcode.com/code/r/r-cmd-check-quotfound-the-following-assignments-to-the-global-environmentquot
    ##
    ##  - as.environment(pos) where pos=1 is equivalent to .GlobalEnv
    ##
    assign_engine_to_global <- function(pos=1){
        assign("fsdaEngine", fsdaEngine, envir=as.environment(pos))
    }

    ## TODO: vedere se possibile definire la variabile non globale con una funzione
    ## che viene chiamata con  costrutto try catch e con exists tutte le volte che serve

    if(!exists("fsdaEngine"))
    {
        ## Check if the Matlab Runtime is installed and stop if not.
        if(!checkRuntime())
            return(FALSE)

        ## java class generata da MATLAB Compiler con tutti i possibili prototipi di funzione
        fsdaEngine = .jnew("org/jrc/ipsc/globesec/sitaf/fsda4java/Fsda")
##        assign("fsdaEngine", fsdaEngine, envir = .GlobalEnv)
        assign_engine_to_global()
    } else {
      fsdaEngine = get("fsdaEngine", envir = .GlobalEnv)
      if (is.jnull(fsdaEngine)) {
        fsdaEngine = .jnew("org/jrc/ipsc/globesec/sitaf/fsda4java/Fsda")
##        assign("fsdaEngine", fsdaEngine, envir = .GlobalEnv)
        assign_engine_to_global()
      }
    }

    return(TRUE)
}

## Check if the Matlab Runtime module is installed and stop if not.
##
checkRuntimeStop <- function()
{
    if(!checkRuntime()){
      stop("Matlab Runtime not installed!")
    }
}

## Check if the Matlab Runtime module is installed and display
##  a message requesting its installation, if not.
##
checkRuntime <- function()
{
  ## Do the check for installed Matlab runtime
  runtimeVersion = "v90" # R2015b
  ## Three ways to check the host OS in R. Though Linux will be the most used
  ## platform (together with Windows), I would prefer the one returning a
  ## more generic "unix"-type OS rather than Linux. After all, the system
  ## variable to be searched for the particular substring is the same on all
  ## Unix variants, including Linux.
  #
  # > .Platform$OS.type
  # [1] "unix"
  # > version$os ## or R.version$os
  # [1] "linux-gnu"
  # > Sys.info()["sysname"]
  # sysname
  # "Linux"
  ## Do the check for installed Matlab runtime

  hostOs = .Platform$OS.type
  path = ""
  pathsep = ""
  filesep = ""
  searchSubstring = "" # vector(mode="character", length=0)
  if (hostOs == "unix") {
    path = Sys.getenv("LD_LIBRARY_PATH")
    pathsep = ":"
    filesep = "/"
    searchSubstring = paste("/", runtimeVersion, "/runtime/glnxa64", sep = "")
  } else if (hostOs == "windows") {
    path = Sys.getenv("PATH")
    pathsep = ";"
    filesep = "\\"
    searchSubstring = paste("\\", runtimeVersion, "\\runtime\\win64", sep = "")
  }
  else {
    stop("Unknown host operating system type!")
  }

  # rti = (grep(searchSubstring, path,  fixed=TRUE) > 0)
  # rti = FALSE
  # for (ss in searchSubstrings) {
  #   rti = rti || (grep(ss, path,  fixed=TRUE) > 0)
  # }
  rti = grepl(searchSubstring, path,  fixed=TRUE) > 0
  if (rti == TRUE) {
    if (!javabuilderJarIsOnClasspath()) {
      addJavabuilderJar2Classpath(path, pathsep, filesep, runtimeVersion, searchSubstring)
    }
  } else {
    cat("\n!! Your installation does not contain the Matlab Runtime module.\n",
        "\nIn order to enable execution of MATLAB files on systems without",
        "\nan installed version of MATLAB you need to install the Matlab Runtime",
        "\n\nDownload and install the required version of the MATLAB Runtime",
        "\nfrom the Web at http://www.mathworks.com/products/compiler/mcr.\n\n")
  }

  return(rti)
}

addJavabuilderJar2Classpath <- function(path, pathsep, filesep, version, rtSubstring)
{
  pathVector = unlist(strsplit(path, pathsep, fixed = TRUE))

  rtBinFolder = ""
  for (sp in pathVector) {
    if (grepl(rtSubstring, sp,  fixed=TRUE) > 0) {
      rtBinFolder = sp
      break;
    }
  }

  if (rtBinFolder == "") {
    stop("javabuilder.jar not found in system path. Please install the MATLAB Runtime first.")
  }

  rtFolderRoot = gsub(rtSubstring, "", rtBinFolder, fixed = TRUE)
  javabuilderJar = paste(rtFolderRoot, version, "toolbox",
                         "javabuilder", "jar", "javabuilder.jar", sep = filesep)
  .jaddClassPath(javabuilderJar)
}

javabuilderJarIsOnClasspath <- function()
{
  cpath = .jclassPath()
  found = FALSE
  for (sp in cpath) {
    found = found || (grepl("javabuilder.jar", sp,  fixed=TRUE) > 0)
  }

  return (found)
}

callFsdaFunctionNoArgout <- function(fsdaFunction, returnType, parameters)
{
    if(!.initFsdaEngine())
        return(NULL)

    ## Richiamo del riferimento alle librerie di MATLAB compilate in Java
    fsdaEngine = get("fsdaEngine", envir = .GlobalEnv)

    ## Chiamata alla funzione FSDA compilata in MATLAB
    out = .jcall(fsdaEngine,
               returnType,          # Tipo di ritorno del metodo Java (notazione JNI)
               fsdaFunction,        # Funzione FSDA da chiamare (es. LXS, FSR, ecc.)
               .jarray(parameters)) # Lista di parametri in ingresso

    return (out)
}

callFsdaFunction <- function(fsdaFunction, returnType, nargout, parameters)
{

    if(!.initFsdaEngine())
        return(NULL)

    ## Richiamo del riferimento alle librerie di MATLAB compilate in Java

    fsdaEngine <- get("fsdaEngine", envir = .GlobalEnv)

    ## Chiamata alla funzione FSDA compilata in MATLAB
    out = .jcall(fsdaEngine,
               returnType,          # Tipo di ritorno del metodo Java (notazione JNI)
               fsdaFunction,        # Funzione FSDA da chiamare (es. LXS, FSR, ecc.)
               as.integer(nargout), # Numero di output restituiti dalla funzione MATLAB
               .jarray(parameters)) # Lista di parametri in ingresso

    return (out)
}

# Disposes of any native MATLAB resources created by Java code
#
# For a native matlab resource to be freed, it must be an instance of the
# "com.mathworks.toolbox.javabuilder.Disposable" interface
freeMatlabResources <- function(resource, verbose = FALSE)
{

  func = deparse(sys.calls()[[sys.nframe()-1]])

  if (is.list(resource)) {
    for (i in 1:length(resource)) {

      if (!is.null(resource[[i]]) && class(resource[[i]]) == "jobjRef") {
        if (verbose) {
          message(paste(func, ": freeing MATLAB object", i , "of", length(resource)))
        }

        if (resource[[i]] %instanceof% "com.mathworks.toolbox.javabuilder.Disposable") {
          resource[[i]]$dispose()
        }
      }
    }
  }
}

# Converte un vettore di stringhe in un cell array di MATLAB
# Parametri:
# svec: vettore di stringhe (char)
# type: parametro stringa.
#       Se impostato a "col" crea un cell array colonna (n x 1),
#       in tutti gli altri casi crea un cell array riga (1 x n)
#
stringVector2CellArray <- function(svec, type) {

  # type of vector (row or column) provided as input parameter
  if (!identical(type, "")) {
    if (identical(type, "col")) {
      rows = length(svec)
      cols = 1
    } else {
      rows = 1
      cols = length(svec)
    }
  }

  # type of vector inferred from layout. Defaults to row type
  # if dim(vector) is null
  if (is.null(dim(svec))) {
    rows = length(svec)
    cols = 1
  }

  cellArray = .jnew("com/mathworks/toolbox/javabuilder/MWCellArray",
                    as.integer(rows),
                    as.integer(cols))

  for (i in 1:length(svec)) {
    cellContent = .jnew("java/lang/String", svec[i])
    index = .jarray(c(as.integer(i), as.integer(1)), "[I", dispatch = TRUE)
    .jcall(cellArray, "V", "set", index, .jcast(cellContent, "java/lang/Object"))
  }

  return (cellArray)
}
