# TODO for VarietyTrial.xml
#
#  load and package for freeVariables
#  [easy] find where the free variables are defined as suggestion for related script.
#
#  Constant propagation
#
#  move the code to find reading data into a function.
#
#  find code that writes results to files - saveRDS(), save.image(), save(), write.csv()
#
#  call graphs for functions.  We have the code for this.
#
#  URLs - default values of parameters in CodeDepends.




# Need to identify variables that are updated before defined.
# Handle source().
# See updateAvgTemp.R and all_vt_comp which is updated before defined.
# 


updateInputs =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # source() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
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
    # source() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
    #
function(inputs, fun, ...)    
    updateInputs(inputs, function(x) isCallTo(x, "load"), fun, ...)

updatePackages =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # source() calls and for each of these call the fun() which
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

    # Check the variables that are identified as not yet defined but may be being used as default values in functions.
    spurious = sapply(ans, isSpuriousFreeVar, inputs, when)
    # now check if the spurious ones are defined.
    spurious = spurious & ans %in% names(when)
    ans = ans[!spurious]

    
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
   class(x) %in% c("=", "<-") && is.call(x[[3]]) && is.name(x[[3]][[1]]) && x[[3]][[1]] == "function"

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

getGraphicsDeviceCalls =
   # Find calls to (known) graphics devices.    
function(f, omitNotRunCode = FALSE, graphicDeviceFuns = GraphicDeviceFuns)
{
    e = to_ast(parse(f))
    if(omitNotRunCode)
        e = dropNotRunCode(e)
    
    find_nodes(e, function(x) is(x, "Call") && is(x$fn, "Symbol") &&
                                   x$fn$value %in% graphicDeviceFuns)
}


##################
getCallParam =
    #
    # Given a call, gets the idx argument in the call after match.call.
    # The function must be available on the search path.
    #
function(call, idx = 1, definitions = globalenv())
{
    fnName = if(is(call, "call"))
                as.character(call[[1]])
             else
                 call$fn$value

    fun = if(is.list(definitions) && fnName %in% names(definitions))
             definitions[[ fnName ]]
          else
             get(fnName, mode = "function")

    if(is(call, "call"))
        match.call(fun, call)[[idx + 1]]
    else
        as_language(match_call(call, fun)$args$contents[[idx]])
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
    xtra = names(funs)[sapply(funs, function(f) any(getGlobals(f)$functions %in% primitiveFuns))] # DEBUGGING EG.  Leave primtiveFuns as ReadDataFuns. or PrimitiveReadDataFuns. Also shows that not using global variables but parameter with default value which is global variable is better.
    unique(c(primitiveFuns, xtra))
}

findReadDataFuns.environment =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
    findReadDataFuns(as.list(funs), primitiveFuns, ...)

findReadDataFuns.character =
function(funs, ..., primitiveFuns = c(PrimitiveReadDataFuns, ...))
{
   findReadDataFuns(getFunctionDefs(funs), primitiveFuns = primitiveFuns)
}


PrimitiveSaveDataFuns = c("saveRDS", "save.image", "save", "serialize")

findSaveDataFuns =
    #
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findSaveDataFuns(ff)
    #
    #
function(funs, ..., primitiveFuns = PrimitiveSaveDataFuns)
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
            files = list.files(x, pattern = "\\.[RrSsQq]$", full.names = TRUE)
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
function(allCalls, funNames, argIndices = 1L, definitions = NULL)
{    
  rcalls = lapply(allCalls, function(calls)
                               calls[sapply(calls, function(x) x$fn$value) %in% funNames])

  unlist(lapply(unlist(rcalls), getCallParam, definitions = definitions))
}
