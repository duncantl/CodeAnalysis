# TODO for VarietyTrial.xml
#
# To remove from use in VarietyTrial.xml as too low-level
#   ifFunAssign?
#   isIfFalse
#   rlangType
#
#   listReadDataFuns? listWriteDataFuns?
#
###############################
#
#  in getGraphics ?????
#  call graphs for functions.  We have the code for this.
#
#  [easy] find where the free variables are defined as suggestion for related script.
#
#  exports
#  man pages
#
#  get{Input,Output,Graphic}Files
#     + take the scripts or √ parsed documents.
#     + allow to either include/ignore calls inside if(FALSE)
#     + √ take all of the Call objects - getAllCalls()
#     + √ when a directory, and missing readDataFuns argument, √ call findWriteDataFuns().
#
# √ function for finding functions that call graphics device functions - mirroring listReadDataFuns() =  findGraphicsDevFuns
#
# √ functions for finding the files in read and write functions calls.
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
# √ URLs - default values of parameters in CodeDepends.  Worked around it here.




# Need to identify variables that are updated before defined.
# Handle source(). ??
# Wrong:  See updateAvgTemp.R and all_vt_comp which is updated before defined.  But actually defined in prep_stan_data.R which is source()d.
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
    
    x@outputs = c(x@outputs, ls(env, all.names = TRUE))
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
    x@outputs = c(x@outputs, ls(getNamespace(pkg), all.names = TRUE))
    x
}


getSourceInfo =
    # Recursive.
function(x, recursive = TRUE, ...)    
{
    ans = getSourceInfox(x)
    if(recursive) {
#        browser()        
        done = unique(ans$from)
        while(TRUE) {
            xtra = unique(getRelativeFiles(ans$sourced, ans$from))
            w = !(xtra %in% done)
            if(any(w)) {
                new = getSourceInfox(xtra[w])
                if(nrow(new) == 0)
                    break
                
                ans = rbind(ans, new)
                done = unique(c(done, new$from))
            }
        }
    }
    
    ans    
}

getRelativeFiles =
    #
    # Is there a function in R that does this? e.g., file.path
    #  No for file.path - file.path("A/B", "~/foo.R")
    #
    # for each of element of a character vector of `files` that were
    # processed relative each element of `rel`
    # compute the relative path of file[i] relative to rel[i]
    #
    # e.g., files = foo/bar.R and rel = ../A/B/C/abc.R
    # The result should be ../A/B/C/foo/bar.R (check)
    #
    # But  files = ~/foo/bar.R and rel = ../A/B/C/abc.R
    # should be ~/foo/bar.R or path.expand.
    #
    # getRelativeFiles("~/foo.R", "A/B/foo.R")
    # 
function(files, rel)
{
    dir = dirname(rel)
    w = grepl("^(~|/)", files)
    ans = file.path(dir, files)

    if(any(w))
        ans[w] = files
    ans
}



# Call this getSourceInfox rather than getSourceInfo() so that
# we can use the latter to call this and then do the recursive
# step.


getSourceInfox =
function(x, recursive = TRUE, ...)    
  UseMethod("getSourceInfox")


getSourceInfox.character =
function(x, ...)    
{
    if(length(x) > 1)
        return(do.call(rbind, lapply(x, getSourceInfox)))

    if(!file.exists(x))
        getSourceInfox(parse(text = x), filename = NA)
    else if(file.info(x)$isdir)
        getSourceInfox(getRFiles(x))
    else
        getSourceInfox(parse(x), filename = x)
}

getSourceInfox.expression =
function(x, filename, ...)    
{
    #    w = sapply(x, isSourceCall)
    # Now, can find source() in subexpressions, including if(FALSE)
    k = findCallsTo(x, "source")
    if(length(k))
        ans = cbind(rep(filename, length(k)), sapply(k, getCallParam, 1L))
    else
        ans = matrix(NA, 0, 2)

    colnames(ans) = c("from", "sourced")
    as.data.frame(ans)
        
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
}

if(FALSE) {
    # Following code was in the body of isVarInDefaultValue above
    # but we return() immediately before this.
    # But codetools doesn't recognize that the code cannot be reached and reports
    # undefined variables.
{    
#    calls = find_nodes(to_ast(use@code), is_symbol, var)
#    if(var == "calc_vars") browser()
    return(all(sapply(calls, isApplyFunArg, var)))

# Don't need this as not just looking at default argumemts.    
    u = use@code
    
    if(!isFunAssign(u))
        return(FALSE)

    any(sapply(u[[3]][[2]], function(x) (is.name(x) && as.character(x) == var) || length(find_nodes(to_ast(x), is_symbol, var))))
}
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
    #
    # toSymbol - logical value. If TRUE, left hand side must be a simple symbol/name
    #  otherwise can be a call, e.g., obj$x = function () 1
    #
    # This is now recursive for non rstatic objects (can do for rstatic)
    # to handle x = y = function(z) z
    #
    # MISSING:
    # Rodam uses reference classes and defines functions within these.
    # RVenn defines only S4 generics and methods, no top-level functions.
    # [added] The package RapidPolygonLookup uses var = structure(function().., ex = function())    
    # [added] Rlab assigns to a character, not a symbol e.g. "US" <- function()
function(x, toSymbol = TRUE)
{
    if(is(x, "R6"))
        is(x, "Assignment") && (!toSymbol || is(x$write, "Symbol") || is(x$write, "Character")) && is(x$read, "Function")
    else {
        class(x) %in% c("=", "<-") &&
            (!toSymbol || (is.name(x[[2]]) || is.character(x[[2]]))) && (isFuncDef(x[[3]]) || isFunAssign(x[[3]]))
                
        }
}

isFuncDef =
function(x)
{
    is.call(x) && is.name(x[[1]]) &&
        (x[[1]] == "function"  ||
         (x[[1]] == "structure" && is.call(x[[2]]) && is.name(x[[2]][[1]]) && x[[2]][[1]] == "function") ||
         (x[[1]] == "local" && evalsToFunction(x[[2]])))
}



evalsToFunction =
    #
    # for analyzing the expression in local() to see if it returns a function.
    #
function(e)
{
    expr = e # original in case we need it.
    if(inherits(e, "{"))
        e = as.list(e)

    res = e[[ length(e) ]]
    if(is.call(res) && is.name(res[[1]]) && as.character(res[[1]]) == "function")
        return(TRUE)

    if(is.name(res)) {
        i = lapply(expr, getInputs)
        var = as.character(res)
        w = sapply(i, function(x) var %in% x@outputs)
        if(any(w)) {
            x = i[[ which.max(w) ]]
            return(isFunAssign(x))
        }
    }

    FALSE
}

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
    # see skipIfFalse()
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

PrimitiveGraphicDeviceFuns = c("png" = "filename",
                               "pdf"= "file",
                               "jpeg" = "filename",
                               "svg" = "filename",
#                               "cairo" = "", # XXX
                               "quartz" = "file",
                               "pictex" = "file",
                               "cairo_pdf" = "filename",  # can create multiple files
                               "cairo_ps" = "filename",
                               "bitmap" = "file")


#XXX  Make a function that mirrors the findReadDataFuns .

listGraphicsDevFuns =
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findWriteDataFuns(ff)
    #
function(funs, ..., primitiveFuns = c(PrimitiveGraphicDeviceFuns, ...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    findReadDataFuns(funs, ..., primitiveFuns = primitiveFuns)
}




getGraphicsDeviceCalls =
   # Find calls to (known) graphics devices.    
function(f, omitNotRunCode = FALSE, graphicDeviceFuns = PrimitiveGraphicDeviceFuns)
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
    #
    # fun is a generic function such as getInputFiles, getOutputFiles, getGraphicsOutputFiles
    #
    # If x is a collection of file names, apply
    # .funNamesFun = list(fun, op)
    #   where op is "readFunNames", "writeFunNames", ...
    #    and fun is the function that returns the list/character vector of c(funName = argName)
    # ... are extra arguments to fun
    #
function(x, fun, ..., .funNamesFun = character())
{
    if(length(x) > 1)
        return(unlist(lapply(x, fun, ...)))

    if(file.info(x)$isdir) {
        files = getRFiles(x)
        if(length(.funNamesFun) && !(.funNamesFun[[2]] %in% names(list(...)))) {
            # Is this worth the complexity to reduce the amount of duplicated code????

            # If the parameter names readFunNames and writeFunNames had the same parameter name across functions
            # we could simplify this code to have no switch and no 3rd version of fun(getRFiles(x), ...)
            funDefs = getFunctionDefs(x)
            funNames = .funNamesFun[[1]](funDefs)
            return(switch(.funNamesFun[[2]],
                   "readFunNames" = fun(files, readFunNames = funNames, definitions = funDefs, ...),
                   "writeFunNames" = fun(files, writeFunNames = funNames, definitions = funDefs, ...),
                   stop("Duncan made a mistake in the .funNamesFun setup")))
# Added but hopefully original back to previous.            
#                 "readFunNames" = getInputFiles(files, readFunNames = funNames, definitions = funDefs, ...),
#                 "writeFunNames" = getOutputFiles(files, writeFunNames = funNames, definitions = funDefs, ...),
#                 stop(paste0("Need to implement generalCharacterMethod for ", .op))))
        }
        return(fun(getRFiles(x), ...))
    }

    fun(parse(x), x, ...)
}

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
    generalCharacterMethod(x, getGraphicsOutputFiles, ..., .funNamesFun = list(findGraphicsDevFuns, "writeFunNames"))

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
             match.call(fun, call)[if(is.numeric(idx)) idx + 1 else idx]
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

asScript =
function(x)
{
    while(!is.null(x$parent))
        x = x$parent
    x
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

asFunction =
function(x)
{
    while(!is.null(x)) {
        if(is(x, "Function"))
            return(x)
        x = x$parent
    }
    
    NULL
}


###################################

PrimitiveReadDataFuns = list("readLines" = "con",
                          "read.csv" = "file",
                          "read.table" = "file",
                          "read.fwf" = "file",
                          "read_excel" = "path",                          
                          "excel_sheets" = "path",
                          "scan" = "file",
                          "data.table" = "...",
                          "readRDS" = "file",
                          "load" = "file")

getReadDataFuns =
    #
    # combine ... with the PrimitiveReadDataFuns
function(..., .els = list())
    mkFunNameList(.els, PrimitiveReadDataFuns)


findReadDataFuns =
function(funs, ..., primitiveFuns = getReadDataFunList())
{
    if(missing(funs))
        return(primitiveFuns)
    
    UseMethod("findReadDataFuns")
}


findReadDataFuns.list =
function(funs, ..., primitiveFuns = getReadDataFunList())
{
    # DEBUGGING EG.  Leave primtiveFuns as ReadDataFuns. or PrimitiveReadDataFuns.
    # Also shows that not using global variables but parameter with default value which is global variable is better.
    xtra = names(funs)[sapply(funs, function(f) any(getGlobals(f)$functions %in% names(primitiveFuns)))] 
    unique(c(primitiveFuns, xtra))
}

findReadDataFuns.environment =
function(funs, ..., primitiveFuns = getReadDataFunList())
    findReadDataFuns(as.list(funs), primitiveFuns = primitiveFuns, ...)

findReadDataFuns.character =
function(funs, ..., primitiveFuns = getReadDataFunList())
   findReadDataFuns(getFunctionDefs(funs), primitiveFuns = primitiveFuns)


findReadDataFuns.expression =
    # a parsed file
function(funs,..., primitiveFuns = getReadDataFunList())
    findReadDataFuns(as.list(funs), primitiveFuns = primitiveFuns, ...)



if(FALSE) {
    i1 = getInputFiles("code")
    i2 = getInputFiles(getAllCalls("code"))
    i3 = getInputFiles(parse("code/getPOWER.R"))
    i4 = getInputFiles(getAllCalls("code/getPOWER.R"))    
}


getInputFiles =
function(x, readFunNames = getReadDataFuns(), ...)    
  UseMethod("getInputFiles")

getInputFiles.character =
function(x, readFunNames = getReadDataFuns(), ...)
    generalCharacterMethod2(x, getInputFiles, ..., .funNames = readFunNames)

getInputFiles.expression =
    #
    # Could actually call findCallsTo(x, readFunNames)
    #
function(x, readFunNames = getReadDataFuns(), filename = NA, ...)
{
    k = structure(findCallsTo(x, readFunNames), class = "ListOfCalls")
    # was k = getAllCalls(x) but now filtering as we walk the code to only keep the calls
    # that are calls to functions in readFunNames.
    # Could make subsequent methods faster by knowing these are pre-filtered, but leave for now.
    getInputFiles(k, filename = filename, readFunNames = readFunNames, ...)
}

getInputFiles.ListOfCalls =
function(x, readFunNames = getReadDataFuns(), filename = NA, ...)
{
    ans = findCallsToFunctions(x, readFunNames, 1L, ...)
    if(length(ans))
        names(ans) = rep(filename, length(ans))
    ans
}

getInputFiles.DirectoryCalls =
function(x, readFunNames = getReadDataFuns(), ...)
{
    ans = mapply(getInputFiles, x, names(x), MoreArgs = list(readFunNames, ...), SIMPLIFY = FALSE)
    structure(unlist(ans), names = rep(names(x), sapply(ans, length)))
}


# These are from dependFuns.R
# Were previously S4 methods.

if(FALSE) 
getInputFiles.character = 
function(x, num = NA, ...)
     getInputFiles(readScript(x), num = NA, ...)

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
    


PrimitiveSaveDataFuns = c("saveRDS" = "file",
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

listWriteDataFuns =
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findWriteDataFuns(ff)
    #
function(funs, ..., primitiveFuns = c(PrimitiveSaveDataFuns, ...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    listReadDataFuns(funs, ..., primitiveFuns = primitiveFuns)
}



findCallsToFunctions =
    #
    # ¿ How is this related to or compares to findCallsTo()?
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
                                      if(is.language(calls) && isCallTo(calls, funNames))
                                          calls
                                      else if(is(calls, "ListOfCalls"))
                                          calls[sapply(calls, function(x) x$fn$value) %in% funNames]
                                      else if(is(calls$fn, "Symbol") && calls$fn$value %in% funNames)
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
    # fun can be findCallsTo or getAllCalls.  The latter returns rstatic language objects.
    #
function(x, parse = TRUE, fun = findCallsTo, ...)
{
    if(!parse)
        return(NULL)
    
    isDir = file.info(x)$isdir
    if(isDir) 
        x = getRFiles(x)

    if(length(x) > 1)
        return(structure(lapply(x, fun), # getAllCalls
                         names = basename(x),
                         class = if(isDir) "DirectoryCalls" else "list"))

    if(file.exists(x))
        code = parse(x)
    else
        code = parse(text = x)

    # Put the class on this to ensure it is the same as the original getAllCalls()    
    structure(fun(code, ...), class = "ListOfCalls")
}

getAllCalls.expression =
    #XXX  consider implement this with findCallsTo(x) with no function names.
function(x, ...)
{
    k = findCallsTo(x)
    # k = find_nodes(to_ast(x), is, "Call")
    structure( k, class = "ListOfCalls" )
}

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
    # XX Make more efficient.
    #
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

usedInCode =
function(dir, dropIfFalse = TRUE, notInToplevelFunctions = TRUE,  rfiles = getRFiles(dir),
          fun = function(x) x$value)
{
    asts = lapply(rfiles, function(f) to_ast(parse(f)))
    names(asts) = basename(rfiles)

    if(dropIfFalse)
        asts = lapply(asts, dropNotRunCode)
    
    if(notInToplevelFunctions)
        asts = lapply(asts, function(x) x$contents[ !sapply(x$contents, isFunAssign) ])

    if(is.null(fun))
        fun = function(x) x
    
    allSyms = unlist(lapply(unlist(asts),
                            function(x)
                             sapply(find_nodes(x,
                                               function(x)
                                                  is(x, "Symbol") && !(is(x$parent, "Assignment") && x$parent$write == x)),
                                    fun)))
}


getSymbolValues =
function(x)
    sapply(find_nodes(x, is, "Symbol"), function(x) x$value)


usedAsFunction =
function(x, parent = x$parent)    
{
    if(is(x, "Parameter"))
        return(FALSE)
    else if(is(parent, "ArgumentList")) {
        p = parent$parent
        if(is(p, "SubsetDollar"))
            return(FALSE)
        
        if(is(p, "Call")) {
            fun = p$fn$value

            if(fun %in% c("$", "$<-", "~", "[", "[[", "==", "|", "||", "&", "&&", "list", "c", "paste", "paste0", "+",
                           "-","*", "/", "%in%", "%/%", "unique", "as.numeric", "as.integer", "plot"))
                return(FALSE)
            

            if(!grepl("apply", fun))
                return(NA)

            # May not be able to find the function definition if it is defined locally within a function
            # e.g. stats::dendrapply
            mcall = match_call(p, get(fun))
            return(identical(mcall$args$contents$FUN, x))
        }
    } else if(is(parent, "Assignment")) 
        return(parent$write != x)
    else if(is(parent, "Call"))
        return(x == parent$fn )
    else if(is(parent, "Return"))
        return(FALSE)
    else if(is(parent, "Loop") && x == parent$iterator)
        return(FALSE)#XXXXX
    else if(is(parent, "If") && x == parent$condition)
        return(FALSE)
    else if(is(parent, "Brace")) # do we need to see if it is the result and then used as a function later.
        return(FALSE)            

    # Other cases:
    # default value of a parameter
    # variable in a for loop. Need to see if used as a function in the loop.
    # value looping over.
    browser()
}




isParamUsedAsFun =
    #
    # See findCallsParam()!!! Don't recreate the wheel.
    #
    #  isParamUsedAsFun("FUN", dendrapply)
    #
function(param, fun)
{
    if(!is(fun, "ASTNode"))
        fun = to_ast(fun)
    k = find_nodes(fun, function(x) is(x, "Call") && is_symbol(x$fn, param))
    if(length(k) > 0)
        return(TRUE)

    # Next, see if it is used in a call to a function that is know
    k = find_nodes(fun, function(x) !is(x, "Parameter") && is_symbol(x, param))
    w = sapply(k, usedAsFunction)

    any(!w)
}




showSig =
function(f, p = formals(f), name = "function")
{
    sprintf("%s(%s)", name,
    paste(names(p), sapply(p, function(v) {
                           if(is.name(v) && as.character(v) == "")
                             ""
                           else
                             paste0("= ", paste(deparse(v), collapse = ""))
      }), collapse = ", "))
}
