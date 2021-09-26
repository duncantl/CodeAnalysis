# Need to identify variables that are updated before defined.
# Handle source().
# See updateAvgTemp.R and all_vt_comp which is updated before defined.
# 

substituteSource =
    #
    # Given a ScriptInfo object, find the elements corresponding to
    # source() calls and for each of these call the fun() which
    # is expected to modify the ScriptNodeInfo.
    #
function(inputs, fun, ...)    
{
    isSource = sapply(inputs, isSourceCall)

    inputs[isSource] = lapply(inputs[isSource], fun, ...)
    inputs
}

updateSourceOutputs =
function(x, baseFile) {
    x@outputs = getSourceDefinedVars(x@code, baseFile)
    x
}

isSourceCall =
function(x)
{
    if(is(x, "ScriptNodeInfo"))
        x = x@code
    
    if(is(x, "R6"))
        is(x, "Call") && is_symbol(x$fn, "source")
    else 
        is(x, "call") && is.name(x[[1]]) && as.character(x[[1]]) == "source"    
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
function(sc, inputs = getInputs(sc),
         exclude = getSearchPathVariables())
{
#    inputs = substituteSource(inputs, updateSourceOutputs, sc@location)
    
    defs = lapply(inputs, slot, "outputs")
    when = rep(seq(along.with = defs), sapply(defs, length))
    names(when) = unlist(defs)

    # XXX Have to handle forward references to variables used in default value of a function parameter.
    # These will be inputs to the function definition but not actually used until the function is first called and maybe not even
    # then.
    free = lapply(seq(along.with = inputs), function(i) notDefBefore(c(inputs[[i]]@inputs, inputs[[i]]@updates), i, when, inputs[[i]]@code, exclude))
    ans = unique(unlist(free))

    # Check the variables that are identified as not yet defined but may be being used as default values in functions.
    browser()
    spurious = sapply(ans, isSpuriousFreeVar, inputs, when)
    # now check if the spurious ones are defined.
    spurious = spurious & ans[spurious] %in% names(when)
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
function(use, var)
{
    u = use@code
    if(!isFunAssign(u))
        return(FALSE)

#    browser()
#    fun = to_ast(use[[3]])
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
function(call, idx = 1)
{
    if(is(call, "call")) 
       match.call(get(as.character(call[[1]]), mode = "function"), call)[[idx + 1]]
    else
       as_language(match_call(call, get(call$fn$value, mode = "function"))$args$contents[[idx]])
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



