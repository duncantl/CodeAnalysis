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

insertSource =
    #
    # works on a file and the regular R language objects from parse()
    # not the CodeDepends Script/ScriptNodeInfo or the rstatic AST objects.
    #
    #  Perhaps also see insertLang()
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
    class(x) %in% c("=", "<-", "<<-") &&
        (!toSymbol || (is.name(x[[2]]) || is.character(x[[2]]))) && (isFuncDef(x[[3]]) || isFunAssign(x[[3]]))
                
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
    
    if(is(x, "if")) 
        as.character(x[[1]]) == "if" && is.logical(x[[2]]) && x[[2]] == FALSE && length(x) == 3
    else 
        FALSE
}



    
##################
getCallParam =
    #
    # Given a call, gets the idx argument in the call after match.call.
    # The function must be available on the search path.
    #
function(call, idx = 1, definitions = globalenv())
{
    if(!is(call, "call"))
        stop("call argument must be a call language object")

    if(is.name(call[[1]])) {
        fnName = as.character(call[[1]])

        env = globalenv()
        if(is.environment(definitions))
            env = definitions
    
        fun = if(is.list(definitions) && fnName %in% names(definitions))
                  definitions[[ fnName ]]
              else
                  get(fnName, env, mode = "function")

        ans = match.call(fun, call)
    } else
        ans = call

    idx = if(is.numeric(idx)) idx + 1 else idx
    ans = ans[idx]
    
    if(length(idx) == 1)
        ans[[1]]
    else
        ans
}



###################
propConstant =
function(code, info)
{
    isLit = sapply(code, isLiteralAssign)
    info

}

isLiteralAssign =
function(x, assignmentOps = c("=", "<-", "<<-"))
{
    class(x) %in% assignmentOps && is.name(x[[2]]) && class(x[[3]]) %in% c("logical", "integer", "numeric", "character")
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



# What do these function intend to do specifically?
# Seems we want to
# find the names of IO arguments for read, write, graphics 
#  1) given a set of function-parameter details we already have
# and
#  2) create that list of function-parameter(s) pairs by analyzing
#  functions to see if they read or write.

# See getIOArgs.R for the former and a list of primitive function-parameter
# info with which we can analyze other functions to see if they call those
# so as to compute 2).  (Also analyze the C code.)

findWriteDataFuns =
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findWriteDataFuns(ff)
    #
function(funs, ..., primitiveFuns = getOutputDataFuns(...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    findReadDataFuns(funs, ..., primitiveFuns = primitiveFuns)
}



findReadDataFuns =
function(funs, ..., primitiveFuns = getReadDataFuns(...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    UseMethod("findReadDataFuns")
}


findReadDataFuns.list =
function(funs, ..., primitiveFuns = getReadDataFuns(...))
{
    # DEBUGGING EG.  Leave primtiveFuns as ReadDataFuns. or PrimitiveReadDataFuns.
    # Also shows that not using global variables but parameter with default value which is global variable is better.
    xtra = names(funs)[sapply(funs, function(f) any(getGlobals(f)$functions %in% names(primitiveFuns)))] 
    unique(c(primitiveFuns, xtra))
}

findReadDataFuns.environment =
function(funs, ..., primitiveFuns = getReadDataFuns(...))
    findReadDataFuns(as.list(funs), primitiveFuns = primitiveFuns, ...)

findReadDataFuns.character =
function(funs, ..., primitiveFuns = getReadDataFuns(...))
   findReadDataFuns(getFunctionDefs(funs), primitiveFuns = primitiveFuns)


findReadDataFuns.expression =
    # a parsed file
function(funs,..., primitiveFuns = getReadDataFuns(...))
    findReadDataFuns(as.list(funs), primitiveFuns = primitiveFuns, ...)



############

findGraphicsDevFuns =
    #
    # ff = list(foo = function(f) save(1:10, file = f))
    # findWriteDataFuns(ff)
    #
function(funs, ..., primitiveFuns = getGraphicsDevFuns(...))
{
    if(missing(funs))
        return(primitiveFuns)
    
    findReadDataFuns(funs, ..., primitiveFuns = primitiveFuns)
}


###################################



getGraphicsDeviceCalls =
   # Find calls to (known) graphics devices.    
function(f, omitNotRunCode = FALSE, graphicDeviceFuns = names(PrimitiveGraphicDeviceIOInfo))
{
    if(is.character(f))
        f = parse(f)
    
    if(omitNotRunCode)
        e = dropNotRunCode(e)

    findCallsTo(e, graphicDeviceFuns)
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
