# TODO for VarietyTrial.xml
#
# To remove from use in VarietyTrial.xml as too low-level
#   ifFunAssign?
#   isIfFalse
#   rlangType
#
#   findReadDataFuns? findWriteDataFuns?
#
###############################
#
#  function for finding functions that call graphics device functions - mirroring findReadDataFuns()
#  in getGraphics
#  functions for finding the files for read and write functions calls.
#  call graphs for functions.  We have the code for this.
#
#  [easy] find where the free variables are defined as suggestion for related script.
#
#  exports
#  man pages
#
# √ getSourceInfo
#
# √ load and package for freeVariables
#
# √ Constant propagation - worked around for now.
#
# √ move the code to find reading data into a function.
#
# √ find code that writes results to files - saveRDS(), save.image(), save(), write.csv()
#
# √  URLs - default values of parameters in CodeDepends.  Worked around it here.




# Need to identify variables that are updated before defined.
# Handle source().
# See updateAvgTemp.R and all_vt_comp which is updated before defined.
# 


updateInputs =
    #
    # Given a ScriptInfo object, find the elements that predicate identifies as relevant
    #  and for each of these call the fun() which
    # is expected to modify the corresponding ScriptNodeInfo.
    #
function(inputs, predicate, update, ...)    
{
    w = sapply(inputs, predicate)
    inputs[w] = lapply(inputs[w], update, ...)
    inputs
}

updateLoads =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # load() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
    #
function(inputs, fun, ...)    
    updateInputs(inputs, function(x) isCallTo(x, "load"), fun, ...)

updatePackages =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # library/require() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
    #
function(inputs, fun, ...)    
    updateInputs(inputs, function(x) isCallTo(x, c("require", "library")), fun, ...)

    

substituteSource =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # source() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
    #
function(inputs, fun, ...)    
    updateInputs(inputs, isSourceCall, fun, ...)


updateSourceOutputs =
function(x, baseFile) {
    x@outputs = getSourceDefinedVars(x@code, baseFile)
    x
}

updateLoad =
function(x, baseFile)
{
    file = relativeFile(x@code[[2]], baseFile)
    if(!file.exists(file)) {
        warning("could not load ", file, ": proceeding without reading it")
        return(x)
    }

    env = new.env()
    load(file, env)
    
    x@outputs = c(x@outputs, ls(env, all = TRUE))
    x
}

updatePackage =
function(x)    
{

    pkg = as.character(x@code[[2]])
    if(system.file("DESCRIPTION", package = pkg) == "") {
        warning("package ", pkg, " is not installed or can't be found. Skipping.")
        return(x)
    }
    x@outputs = c(x@outputs, ls(getNamespace(pkg), all = TRUE))
    x
}


getSourceInfo =
function(x, ...)    
    UseMethod("getSourceInfo")


getSourceInfo.character =
function(x, ...)    
{
    if(length(x) > 1)
        return(do.call(rbind, lapply(x, getSourceInfo)))

    if(file.info(x)$isdir)
        return(getSourceInfo(getRFiles(x)))

    getSourceInfo(parse(x), x)
}

getSourceInfo.expression =
function(x, filename, ...)    
{
    w = sapply(x, isSourceCall)
    if(any(w)) 
        ans = cbind(rep(filename, sum(w)), sapply(x[w], getCallParam, 1L))
    else
        ans = matrix(NA, 0, 2)

    colnames(ans) = c("from", "sourced")
    ans
        
}


isSourceCall =
function(x)
   isCallTo(x, "source")

isCallTo =
function(x, funName)
{
    if(is(x, "ScriptNodeInfo"))
        x = x@code
    
    if(is(x, "R6"))
        is(x, "Call") && is_symbol(x$fn) && x$fn$value %in% funName
    else 
        is(x, "call") && is.name(x[[1]]) && as.character(x[[1]]) %in% funName
}


insertSource =
    #
    # works on a file and the regular R language objects from parse()
    # not the CodeDepends Script/ScriptNodeInfo or the rstatic AST objects.
    #
function(file, code = parse(file), done = character(), asScript = TRUE)
{
    w = sapply(code, isSourceCall)
    if(!any(w))
        return(code)

    done = c(done, file)
    # Need to get relative path.
    #  Need to avoid infinite loops
    tmp = lapply(code[w], function(x) {
                                fi = as.character(x[[2]])
                                fi = relativeFile(fi, file)
                                if(!file.exists(fi)) {
                                    warning("the file ", fi, "does not exist. Skipping the source() substitute.")
                                    return(x)
                                }
                                insertSource(fi, parse(fi), done)
                            })

    code[w] = tmp

    # This doesn't work - leaves the same.
    #    unlist(code, recursive = FALSE)
    # So we use
    ans = unlist(lapply(code, function(x) x), recursive = FALSE)

    if(asScript) # May want to indicate that this script has had the source expressions substituted/inserted. New subclass of Script.
        new("Script", ans, location = file)
    else
        ans
}

relativeFile =
function(name, base)
{
    # temporary
    name =  path.expand(name)
    if(grepl("^/", name))
        return(name)

    file.path( dirname(base), name)
}


################



freeVariables =
    #
    #  load - TRUE/FALSE for whether to follow load() and readRDS commands and replace the outputs there with the names of the variables.
    #  packages - TRUE/FALSE (not names of the packages) so that we check for variables in those packages identified by library/require.
    #
    #
    #  fr = freeVariables("code/calc_weather_var.R")
    #
function(sc, load = TRUE, packages = TRUE, includeSource = TRUE,
         exclude = getSearchPathVariables(), inputs = getInputs(sc))
{
    scriptName = NA
    if(is.character(sc)) {
        scriptName = sc
        sc = readScript(sc)
    }

    if(includeSource)
       sc = insertSource(scriptName, sc)

#    inputs = substituteSource(inputs, updateSourceOutputs, sc@location)

    if(load) 
        inputs = updateLoads(inputs, updateLoad, sc@location)
    
    if(packages)  
        inputs = updatePackages(inputs, updatePackage)

    
    defs = lapply(inputs, slot, "outputs")
    when = rep(seq(along.with = defs), sapply(defs, length))
    names(when) = unlist(defs)

    # XXX Have to handle forward references to variables used in default value of a function parameter.
    # These will be inputs to the function definition but not actually used until the function is first called and maybe not even
    # then.
    free = lapply(seq(along.with = inputs), function(i) notDefBefore(c(inputs[[i]]@inputs, inputs[[i]]@updates), i, when, inputs[[i]]@code, exclude))
    ans = unique(unlist(free))

    if(length(ans)) {
        # Check the variables that are identified as not yet defined but may be being used as default values in functions.
        spurious = sapply(ans, isSpuriousFreeVar, inputs, when)
        # now check if the spurious ones are defined.
        spurious = spurious & ans %in% names(when)
        ans = ans[!spurious]
    }

    
#    if(length(exclude))
#        ans = setdiff(ans, exclude)
    ans
}

isSpuriousFreeVar =
function(var, inputs, whenDef)
{
    
    u = sapply(inputs, function(x) var %in% x@inputs)
    any(sapply(inputs[ u ], isVarInDefaultValue, var))
}

isVarInDefaultValue =
    # Can be anywhere in the function, not just a default value.
function(use, var)
{
    return(isFunAssign(use@code))

#    calls = find_nodes(to_ast(use@code), is_symbol, var)
#    if(var == "calc_vars") browser()
    return(all(sapply(calls, isApplyFunArg, var)))

# Don't need this as not just looking at default argumemts.    
    u = use@code
    
    if(!isFunAssign(u))
        return(FALSE)

    any(sapply(u[[3]][[2]], function(x) (is.name(x) && as.character(x) == var) || length(find_nodes(to_ast(x), is_symbol, var))))
}


notDefBefore =
function(vars, num, defWhen, code = NULL, exclude = character())
{
    exists = names(defWhen)[ defWhen < num]
    ans = vars[!(vars %in% c(exists, exclude))]

    # Check if the code is defining a function and the symbols are in the default
    # values
#    if(length(ans) > 0 && length(code) && isFunAssign(code)) {
#        browser()
#    }

    ans
}

isFunAssign =
function(x)
   class(x) %in% c("=", "<-") && is.call(x[[3]]) && is.name(x[[3]][[1]]) && x[[3]][[1]] == "function" # add the following ?? && is.name(x[[2]])

getSearchPathVariables =
    #
    # Get the names of all the variables on the search path, by default, excluding
    # the global environment.
    #  The caller can specify the packages of interest rather than changing the
    # search path, e.g,
    #    setdiff(search(), c(".GlobalEnv", "Autoloads", "package:base"))
function(pkgs = search()[-1])
{
  unique(unlist(lapply(pkgs, ls, all = TRUE)))
}


getSourceDefinedVars =
function(call, baseFile)
{
    f = XML::getRelativeURL(call[[2]], baseFile)
    if(!file.exists(f)) {
        warning("cannot find ", f, " referenced in source() call")
        return(character())
    }
    
    sc = readScript(f)
    info = getInputs(sc)
    
    unique(unlist(lapply(info, slot, "outputs")))
}

################################

dropNotRunCode =
    #
    # remove code of the form if(FALSE) from an rstatic AST.
    #
function(x, ...)
   UseMethod("dropNotRunCode")

dropNotRunCode.R6 =
function(x, ...)    
{
    nodes = find_nodes(x, isIfFalse)
    if(length(nodes)) 
           lapply(nodes, function(x) children(x$parent) = children(x$parent)[ - where_is(x) ])

    x
}

dropNotRunCode.expression = dropNotRunCode.ScriptInfo = dropNotRunCode.Script =
    #
    # Example of how we can use the same code for these three different data
    # structures because the predicate function isIfFalse is "generic"
    # polymorphic.
    #
    #
function(x, ...)
{
    w = sapply(x, isIfFalse)
    x[!w]
}



isIfFalse =
    #
    # tests whether the call (x) is an if(FALSE) statement with no else.
    #
function(x)
{
    if(is(x, "ScriptNodeInfo"))
        x = x@code
    
    if(is(x, "if")) {
      as.character(x[[1]]) == "if" && is.logical(x[[2]]) && x[[2]] == FALSE && length(x) == 3
    } else if(is(x, "If"))
        is(x$condition, "Literal") && x$condition$value == FALSE && length(x$false$contents) == 0
    else
        FALSE

}



##############

GraphicDeviceFuns = c("png", "pdf", "jpeg", "svg", "cairo", "quartz", "pictex", "cairo_pdf", "cairo_ps", "bitmap")
#XXX  Make a function that mirrors the findReadDataFuns .

getGraphicsDeviceCalls =
   # Find calls to (known) graphics devices.    
function(f, omitNotRunCode = FALSE, graphicDeviceFuns = GraphicDeviceFuns)
{
    if(is.character(f))
        f = parse(f)
    
    e = to_ast(f)
    if(omitNotRunCode)
        e = dropNotRunCode(e)
    
    find_nodes(e, function(x) is(x, "Call") && is(x$fn, "Symbol") &&
                                   x$fn$value %in% graphicDeviceFuns)
}




generalCharacterMethod =
function(x, fun, ...)
{
    if(length(x) > 1)
        return(unlist(lapply(x, fun, ...)))

    if(file.info(x)$isdir)
        return(fun(getRFiles(x), ...))

    fun(parse(x), x, ...)
}
    


#XXX Pass in the names of the graphics functions.
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

getGraphicsOutputFiles.character =
function(x, ...)
    generalCharacterMethod(x, getGraphicsOutputFiles, ...)

getGraphicsOutputFiles.expression =
function(x, filename, ...)
{
    calls = getGraphicsDeviceCalls(x)
    ans = sapply(calls, getCallParam, 1L)
    names(ans) = rep(filename, length(ans))
    ans
}
    
##################
getCallParam =
    #
    # Given a call, gets the idx argument in the call after match.call.
    # The function must be available on the search path.
    #
function(call, idx = 1, definitions = globalenv())
{
    rlang = is(call, "call")
    fnName = if(rlang)
                as.character(call[[1]])
             else
                 call$fn$value

    env = globalenv()
    if(is.environment(definitions))
        env = definitions
    
    fun = if(is.list(definitions) && fnName %in% names(definitions))
             definitions[[ fnName ]]
          else
             get(fnName, env, mode = "function")

    ans = if(rlang) 
             match.call(fun, call)[if(is.numeric(idx) idx + 1 else idx]
          else 
             lapply(match_call(call, fun)$args$contents[idx], paramValue)

    if(length(idx) == 1)
        ans[[1]]
    else
        ans
}


paramValue =
function(v)
{
    if(is(v, "Literal"))
        as_language(v)
    else
        findLiteralValue(v)
}



###################
propConstant =
function(code, info)
{
    isLit = sapply(code, isLiteralAssign)
    info

}
isLiteralAssign =
function(x)
{
    class(x) %in% c("=", "<-") && is.name(x[[2]]) && class(x[[3]]) %in% c("logical", "integer", "numeric", "character")
}


# Instead of propagating the constants before analysis, for now we will
# find a Symbol and walk back through the script to see if we have a literal value
# This is for an rstatic AST object.

findLiteralValue =
function(sym)
{
        # Assume an argument in an ArgumentList so get to the call.
   call = sym$parent$parent

   idx = where_is(asToplevelExpr(call))
   
   script = asScript(call)
   before = script$contents[rev(seq_len(idx - 1))]
   lit = sapply(before, function(x) is(x, "Assignment") && x$write == sym && is(x$read, "Literal"))
   if(any(lit))
       as_language(before[[ which(lit)[1] ]]$read)
   else
       sym
}

asToplevelExpr =
function(x)
{
    if(is.null(x$parent))
        return(x)
    
    while(!is.null(x$parent$parent))
        x = x$parent
    x
}




###################################

PrimitiveReadDataFuns = c("readLines", "read_excel", "read.csv", "read.table", "read.fwf", "excel_sheets")

findReadDataFuns =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    UseMethod("findReadDataFuns")
}


findReadDataFuns.list =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
{
    # DEBUGGING EG.  Leave primtiveFuns as ReadDataFuns. or PrimitiveReadDataFuns.
    # Also shows that not using global variables but parameter with default value which is global variable is better.
    xtra = names(funs)[sapply(funs, function(f) any(getGlobals(f)$functions %in% primitiveFuns))] 
    unique(c(primitiveFuns, xtra))
}

findReadDataFuns.environment =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
    findReadDataFuns(as.list(funs), primtiveFuns = primitiveFuns, ...)

findReadDataFuns.character =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
   findReadDataFuns(getFunctionDefs(funs), primitiveFuns = primitiveFuns)


findReadDataFuns.expression =
    # a parsed file
function(funs,..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
    findReadDataFuns(as.list(funs), primtiveFuns = primitiveFuns, ...)


getInputFiles =
function(x, ...)    
  UseMethod("getInputFiles")

getInputFiles.character =
function(x, ...)
    generalCharacterMethod(x, getInputFiles, ...)

getInputFiles.expression =
function(x, filename = NA, readFunNames = findReadDataFuns(x), ...)
{
    ans = findCallsToFunctions(getAllCalls(x), readFunNames, 1L, ...)
    names(ans) = rep(filename, length(ans))
    ans
}

getOutputFiles =
function(x, ...)    
  UseMethod("getOutputFiles")

getOutputFiles.character =
function(x, ...)
    generalCharacterMethod(x, getOutputFiles, ...)

getOutputFiles.expression =
function(x, filename = NA, writeFunNames = findWriteDataFuns(x), ...)
{
    ans = findCallsToFunctions(getAllCalls(x), writeFunNames, "file", ...)
    if(length(ans) > 0)
        names(ans) = rep(filename, length(ans))
    ans
}
    


PrimitiveSaveDataFuns = c("saveRDS", "save.image", "save", "serialize", "write.table", "write.csv")
# cat? but with a file = ...
#

findWriteDataFuns =
    #
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findWriteDataFuns(ff)
    #
    #
function(funs, ..., primitiveFuns = c(PrimitiveSaveDataFuns, ...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    findReadDataFuns(funs, ..., primitiveFuns = primitiveFuns)
}

####################

getFunctionDefs =
    # Read a file, an environment, a parse tree and find the top-level
    # function definitions
function(x, ...)
 UseMethod("getFunctionDefs")

getFunctionDefs.character =
function(x, ...)
{
    if(file.exists(x)) {

        info = file.info(x)
        if(info$isdir[1]) {
            files = getRFiles(x)
            tmp = lapply(files, getFunctionDefs)
            tt = table(unlist(lapply(tmp, names)))
            if(any(tt > 1))
                warning("multiple definitions for functions ", paste(names(tt)[tt > 1], collapse = ", "))
            return(unlist(tmp))
        }
    }
    
   e =  if(file.exists(x))
            parse(x)
        else
            parse(text = x)

  getFunctionDefs(e)
}

getFunctionDefs.expression =
function(x, ...)
{
    w = sapply(x, isFunAssign)
    env = new.env()
    lapply(x[w], eval, env)
    as.list(env)
}


#################################


findCallsToFunctions =
    #
    #  allCalls = getAllCalls("code")
    # a = findCallsToFunctions(allCalls, findSaveDataFuns(), definitions = funs, argIndices = "file")
    #
    # a = findCallsToFunctions("code", findSaveDataFuns(), definitions = funs, argIndices = "file")
    #
function(allCalls, funNames, argIndices = integer(), definitions = NULL)
{
    if(is.character(allCalls)) 
        allCalls = getAllCalls(allCalls)
    
    rcalls = lapply(allCalls, function(calls) {
                                  if(is(calls, "ListOfCalls"))
                                      calls[sapply(calls, function(x) x$fn$value) %in% funNames]
                                  else if(calls$fn$value %in% funNames)
                                      calls
                              })

    rcalls = unlist(rcalls)
    if(length(argIndices))
        unlist(lapply(rcalls, getCallParam, argIndices, definitions = definitions))
    else
        rcalls
}


getAllCalls =
function(x, ...)
    UseMethod("getAllCalls")

getAllCalls.character =
    #
    #
    # Note that we separate directory and getting list.files
    #  and then processing the vector so that the caller can specify
    # a collection of files not just the directory.
    # This allows them to filter some out.
    #
    #
    #
function(x, ...)
{
    if(file.info(x)$isdir) 
        x = getRFiles(x)

    if(length(x) > 1)
        return(structure(lapply(x, getAllCalls), names = basename(x)))

    getAllCalls(parse(x), ...)
}

getAllCalls.expression =
function(x, ...)
   structure( find_nodes(to_ast(x), is, "Call"), class = "ListOfCalls" )




rlangType =
function(x)
{
    if(inherits(x, c("logical", "integer", "numeric", "character")))
        class(x)
    else
        class(rstatic::as_language(x))  
}




###############

getURLs =
    #
    # This is a slow way of doing this if we have already done some of the computations.
    # So we need to allow this to be called in different ways.
    # However, we still have to make up for the "error" in CodeDepends not processing the default values of parameters.
    #
function(dir)    
{
    if(!file.exists(dir))
        stop("no such file ", dir)

    if(length(dir) > 1)
        return(unique(unlist(lapply(dir, getURLs))))
    
    if(file.info(dir)$isdir) 
        ff = getRFiles(dir)
    else
        ff = dir
    
    sc = lapply(ff, function(f) getInputs(readScript(f)))
    strings = unlist(lapply(sc, function(f) lapply(f, function(x) x@strings)))

    # Because CodeDepends doesn't handle strings properly in default arguments of parameters
    funs = unlist(getFunctionDefs(dir))
    tmp = unlist(lapply(funs, function(x) find_nodes(to_ast(x), function(x) is(x, "Character") && grepl("^(http|ftp)", x$value))))
    xtra = unlist(sapply(tmp, function(x) x$value))

    grep("^(http|ftp)", unique(c(strings, xtra)), value = TRUE)
}



getRFiles =
function(dir, pattern = '\\.[RrSsQq]$')
   list.files(dir, pattern, full.names = TRUE)



