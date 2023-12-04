# What about findReadDatafuns and methods in freeVariables.R
#   .expression, .list, .environment, .character.

# Repeating the same pattern for  Input, Output and GraphicsOutputs.
# The only difference is the list of function info but have methods for the different types
# character, ListOfCalls, expression.
# But we can have 3 functions each of which take any source of code
# and have a default that gets the specific list of function-parameter details
# and then call a single helper function which can be generic.

# Note issue with dput of getInputFiles() result and call.

if(FALSE) {
    stopifnot(identical(getInputFiles("inst/sampleCode/inputFiles.R"),
                        list("../foo.csv", "sub/lines.txt", quote(sprintf("Data_%s.csv", Sys.Date())), "results.rds")))
    
    stopifnot(identical(getOutputFiles("inst/sampleCode/inputFiles.R"),
                        c("today.rds", NA, "foo.csv", "myMatrix.mat")))

    stopifnot(identical(getGraphicsOutputFiles("inst/sampleCode/inputFiles.R"),
                        c("foo.pdf", "foo.jpg", "foo.png")))

}

##############

PrimitiveGraphicDeviceIOInfo = list("png" = "filename",
                                    "pdf"= "file",
                                    "jpeg" = "filename",
                                    "svg" = "filename",
                                    #                               "cairo" = "", # XXX
                                    "quartz" = "file",
                                    "pictex" = "file",
                                    "cairo_pdf" = "filename",  # can create multiple files
                                    "cairo_ps" = "filename",
                                    "bitmap" = "file")



PrimitiveSaveIOInfo = list("saveRDS" = "file",
                           "save.image" = "file",
                           "save" = "file",
                           "serialize" = "connection",
                           "write.table" = "file",
                           "write.csv" = "file",
                           "write.csv2" = "file",
                           "write" = "file",
                           "MASS::write.matrix" = "file")
# cat? but only with a file = ... argument
#

PrimitiveReadIOInfo = list("readLines" = "con",
                           "read.csv" = "file",
                           "read.table" = "file",
                           "read.fwf" = "file",
                           "read_excel" = "path",                          
                           "excel_sheets" = "path",
                           "scan" = "file",
                           "data.table" = "...",
                           "readRDS" = "file",
                           "load" = "file")


#XXX  Make a function that mirrors the findReadDataFuns .

getGraphicsDevFuns =
function(..., .els = list(...))
    mkFunNameList(.els, PrimitiveGraphicDeviceIOInfo, rmDups = FALSE)

getReadDataFuns =
function(..., .els = list(...))
    mkFunNameList(.els, PrimitiveReadIOInfo, rmDups = FALSE)

getOutputDataFuns =
function(..., .els = list(...))
    mkFunNameList(.els, PrimitiveSaveIOInfo, rmDups = FALSE)

##########

getInputFiles =
function(x, ..., .readFunNames = getReadDataFuns())
    getIOArgs(x, .funNames = .readFunNames)

getOutputFiles =
function(x, ..., .readFunNames = getOutputDataFuns())
    getIOArgs(x, .funNames = .readFunNames)

getGraphicsOutputFiles =
function(x, ..., .readFunNames = getGraphicsDevFuns())
    getIOArgs(x, .funNames = .readFunNames)


############

generalCharacterMethod2 =
    #
    # fun is a generic function such as getInputFiles, getOutputFiles, getGraphicsOutputFiles
    #
    # If x is a collection of file names, apply 
    #
function(x, fun, ..., .funNames = character())
{
    if(length(x) == 1) {
        if(!file.info(x)$isdir) {
            code = parse(x)
        } else
            return(generalCharacterMethod2(getRFiles(x), fun, ..., .funNames = .funNames))
    } else { 
        code = unlist(lapply(x, parse), recursive = FALSE)
    }

    calls = findCallsTo(code, names(.funNames))
    sapply(calls, matchArgInCall, .funNames)
}

matchArgInCall =
function(call, funArgs, envir = globalenv())
{
    fn = call[[1]]
    if(isCallTo(fn, c("::", ":::")))
        def = eval(fn, envir)
    else
        def = get(as.character(call[[1]]), envir, mode = "function")

    fn = deparse(fn)
    i = match(fn,  names(funArgs))
    if(is.na(i))
        i = match(gsub(".*::", "", fn), names(funArgs))

    if(any(is.na(i)) || length(i) > 1)
        stop(paste0("problems matching", fn, " in ", paste(names(funArgs), collapse = ", ")))
    
    argName = funArgs[[i]]    
    # For write.csv, write.csv2, etc. that are defined as function(...) and then an NSE call
    # to utils::write.table or some other function, can't match.
    # FIX
    if(!(argName %in% names(formals(def))))
        return(NA)
    
    m = match.call(def, call, expand.dots = TRUE)
    # funArgs  could be a list with more than one parameter name in a function
    # if more than one parameter is of interest.
    m[[argName]]
}



#XXX Pass in the names of the graphics functions.
if(FALSE)
getGraphicsOutputFiles =
function(x, ...)
    UseMethod("getGraphicsOutputFiles")

if(FALSE) {
getGraphicsOutputFiles.character =
function(x, ...)
{
    if(length(x) > 1)
        return(unlist(lapply(x, getGraphicsOutputFiles, ...)))

    if(file.info(x)$isdir)
        return(getGraphicsOutputFiles(getRFiles(x), ...))

    getGraphicsOutputFiles(parse(x), x, ...)
}
}


if(FALSE) {
getGraphicsOutputFiles.character =
function(x, ...)
    generalCharacterMethod2(x, getGraphicsOutputFiles, ..., .funNames = getGraphicsDevFuns())

getGraphicsOutputFiles.expression =
function(x, filename, ...)
{
    calls = getGraphicsDeviceCalls(x)
    ans = sapply(calls, getCallParam, 1L)
    names(ans) = rep(filename, length(ans))
    ans
}
}




if(FALSE) {
    i1 = getInputFiles("code")
    i2 = getInputFiles(getAllCalls("code"))
    i3 = getInputFiles(parse("code/getPOWER.R"))
    i4 = getInputFiles(getAllCalls("code/getPOWER.R"))    
}

getIOArgs =
function(x, .funNames, ...)
  UseMethod("getIOArgs")

getIOArgs.character =
function(x, .funNames, ...)
    generalCharacterMethod2(x, getInputFiles, ..., .funNames = .funNames)

getIOArgs.expression =
    #
    # Could actually call findCallsTo(x, readFunNames)
    #
function(x, .funNames, filename = NA, ...)
{
    k = structure(findCallsTo(x, .funNames), class = "ListOfCalls")
    # was k = getAllCalls(x) but now filtering as we walk the code to only keep the calls
    # that are calls to functions in readFunNames.
    # Could make subsequent methods faster by knowing these are pre-filtered, but leave for now.
    getIOArgs(k, filename = filename, readFunNames = .funNames, ...)
}

getIOArgs.ListOfCalls =
function(x, .funNames, filename = NA, ...)
{
    ans = findCallsToFunctions(x, .funNames, 1L, ...)
    if(length(ans))
        names(ans) = rep(filename, length(ans))
    ans
}

getIOArgs.DirectoryCalls =
function(x, .funNames, ...)
{
    #    ans = mapply(getIOArgs, x, names(x), MoreArgs = list(.funNames = .funNames, ...), SIMPLIFY = FALSE)
    ans = lapply(x, getIOArgs, .funNames = .funNames, ...)
    structure(unlist(ans), names = rep(names(x), sapply(ans, length)))
}



# These are from dependFuns.R
# Were previously S4 methods.
# But we no longer have a generic for getInputFiles.
# Perhaps merge with getIOArgs, but specific to input.

if(FALSE) {
getInputFiles.character = 
function(x, num = NA, ...)
     getInputFiles(readScript(x), num = NA, ...)
}

getInputFiles.ScriptInfo = 
function(x, num = NA, ...) {
    tmp = lapply(seq_along(x), function(i) getInputFiles(x[[i]], i, ...))
    do.call(rbind, tmp)
}

getInputFiles.ScriptNodeInfo = 
function(x, num = NA, ...) {
    file = x@files
    file = file[file != "" ]
    if(length(file) == 0)
        return(NULL)
    op = names(x@functions)
    
    data.frame(filename = file, operation = op,  expressionNum = num, stringsAsFactors = FALSE)
}


if(FALSE) {
getOutputFiles =
function(x, ...)    
  UseMethod("getOutputFiles")

getOutputFiles.character =
function(x, ...)
    generalCharacterMethod2(x, getOutputFiles, ..., .funNames = listWriteDataFuns())

getOutputFiles.expression =
function(x, filename = NA, writeFunNames = findWriteDataFuns(x), ...)
{
    ans = findCallsToFunctions(getAllCalls(x), writeFunNames, "file", ...)
    if(length(ans) > 0)
        names(ans) = rep(filename, length(ans))
    ans
}
}
