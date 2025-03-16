countNestedFunctionLevel =
    #
    # Given an rstatic Function object, this walks up the AST 
    #
    #
function(f, count = 0)
{
    f2 = asFunction(f$parent)
    if(!is.null(f2))
        return(countNestedFunctionLevel(f2, count + 1))

    count

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



###########

#############
# See S3Assigns.R

findS3ClassDefs =
function(x, ...)
   UseMethod("findS3ClassDefs")

findS3ClassDefs.character =
function(x, ...)
    findS3ClassDefs(parse(x), ...) # deal with vector, directory.

findS3ClassDefs.expression = findS3ClassDefs.function =
function(x, ...)
{
    w = mkCallWalkerPred(isS3ClassSetNode)
    k = findCallsTo(x, walker = w)
    # Now have to post process the nodes.

    # extractS3Class wants the RHS of the class() = ..
    # not the full assignment call. So this would need to be
    # fixed if we want to use this function. But it is not exported
    # or called.
    browser()
    lapply(k, extractS3Class)
}


isS3ClassSetNode =
function(x, isName, ...)
{
    if(isCallTo(x, "structure") && "class" %in% names(x))
        return(TRUE)
    

    if(!isComplexAssignTo(x))
        return(FALSE)

    lhs = x[[2]]
    
    if(isCallTo(lhs, "class"))
        return(TRUE)

    isCallTo(lhs, "attr") && length(lhs) >= 2 && is.character(x[[2]]) && x[[2]] == "class"
}



#########
#freeVariables.R

dropNotRunCode.R6 =
function(x, ...)    
{
    nodes = find_nodes(x, isIfFalse)
    if(length(nodes)) 
           lapply(nodes, function(x) children(x$parent) = children(x$parent)[ - where_is(x) ])

    x
}


#####
# passGlobals.R

passGlobals =
    #
    # Add additional arguments to calls to any of the functions
    # named in gVarsByFun
    #
function(fun, gVarsByFun)    
{
    ofun = fun
    
    ast = to_ast(fun)
    replace_nodes(ast, updateCallsFun(gVarsByFun), in_place = TRUE)    
    #astTraverse(ast, updateCallsFun(gVarsByFun))
    
    fun = eval(as_language(ast))
    environment(fun) = environment(ofun)
    fun
}


################
# checkLoopDepend.R

# Check that an iterator is guaranteed to contain unique objects when evaluated.
checkUnique = function(iterator, uniqueFuncs)
{
    if(is(iterator, "Symbol")){
        return(list(
            result = FALSE
            , reason = sprintf("cannot be sure that the variable `%s` being looped over contains unique values", iterator$value)
            , reasonCode = "ITERATOR_FREE_VAR"
        ))
    } else if(is(iterator, "Call")) {
        if(!(iterator$fn$value %in% uniqueFuncs)){
        return(list(
            result = FALSE
            , reason = sprintf("the iterator is a call to the function `%s`, which may not produce unique values", iterator$fn$value)
            , reasonCode = "ITERATOR_UNKNOWN_FUNC"
        ))
        }
    } else {
        iter_msg = deparse(rstatic::as_language(iterator))
        return(list(
            result = FALSE
            , reason = sprintf("iterator `%s` is not a symbol or a call", iter_msg)
            , reasonCode = "ITERATOR_UNKNOWN"
        ))
    }
    list(
        result = TRUE
        , reason = "passed tests for uniqueness"
        , reasonCode = "PASS_UNIQUE"
    )
}


checkVariableDependency =
    #??? Comments describing what this does.
function(v, body, ivar, fixed_globals)
{
    vs = rstatic::Symbol$new(v)
    assigns_over_var = findAssignsOverVar(body, vs)
    if(0 < length(assigns_over_var)){
        return(list(
            result = FALSE
            , reason = sprintf("read after write dependency on variable `%s`", v)
            , reasonCode = "RAW"
        ))
    }

    all_updates = findAllUpdates(body, vs)
    ok_updates = find_nodes(body, independentUpdate, vs, ivar, fixed_globals)
    bad_updates = setdiff(all_updates, ok_updates)
# ok and bad are not helpful terms that convey why they are ok/bad.

    if(0 < length(bad_updates)){
        bad_up = body[[bad_updates[[1L]]]]
        bad_up_msg = deparse(rstatic::as_language(bad_up))
        return(list(
            result = FALSE
            , reason = c(sprintf("variable `%s` is assigned to using an index which is not the iterator variable in the loop:", v), bad_up_msg)
            , reasonCode = "COMPLEX_UPDATE"
        ))
    }

    # So this says everything is fine eventhough it may not be.  So (false) positive is the default.
    list(
        result = TRUE
        , reason = "passed variable dependency tests"
        , reasonCode = "PASS_DEPENDENCY"
    )
}


independentUpdate = function(node, v, ivar, fixed_globals = character())
{
    if(is(node, "Replacement") && varAppears(node$write, v) ){

        # The order of these checks matters.

        rhs = rstatic::arg_value(node)
        if(is(rhs, "Symbol") && rhs$value %in% fixed_globals){
            #browser()
            # This case:
            # x[foo(i)] = const
            return(TRUE)
        }

        if(varAppears(node$write, ivar)){
            # This case:
            # x$foo$bar[[ivar]]$baz = ...
            return(TRUE)
        }
        index_args = rstatic::arg_index(node)
        index_same_as_ivar = sapply(index_args, `==`, ivar)

        # If it's a multidimensional array and at least one of the subscripts is the same as the iteration variable, then it doesn't matter what the rest of the subscripts are.
        if(any(index_same_as_ivar)){
            return(TRUE)
        }
    }
    FALSE
}


checkLoopDepend = function(forloop, checkIterator = FALSE, uniqueFuncs = c("seq", ":", "unique"))
{

    forloop = rstatic::to_ast(forloop)
    if(!is(forloop, "For"))
        stop("Not a for loop.")

    body = forloop$body
    ivar = forloop$variable    

    deps = CodeDepends::getInputs(rstatic::as_language(body))
    changed = c(deps@outputs, deps@updates)

    # The easy way out
    if(length(changed) == 0){
        return(list(
            result = TRUE
            , reason = "loop does not define or update any variables"
            , reasonCode = "NO_CHANGE"
        ))
    }

    if(ivar$value %in% changed){
        return(list(
            result = FALSE
            , reason = sprintf("iteration variable %s is changed within the body of the loop", ivar$value)
            , reasonCode = "ITERATION_VAR_CHANGE"
        ))
        # This would be OK if the loop body does not subsequently use the iterator variable in a subset assignment.
        # In that case it can be fixed by renaming the variable.
        # Indeed, there's really never any reason to redefine the iteration variable rather than just use a new variable.
    }

    #?? What is the meaning of fixed_globals? Why does it include ivar (i) which is not fixed?
    global_updates = intersect(deps@inputs, deps@updates)
    fixed_globals = setdiff(deps@inputs, changed)

    for(v in global_updates){
        tmp = checkVariableDependency(v, body, ivar, fixed_globals = fixed_globals)
        if(!tmp[["result"]]){
            return(tmp)
        }
    }

    if(checkIterator && 0 < length(global_updates)){
        tmp = checkUnique(forloop$iterator, uniqueFuncs)
        if(!tmp[["result"]]){
            return(tmp)
        }
    }

    return(list(
        result = TRUE
        , reason = "passed all tests for loop carried dependencies"
        , reasonCode = "PASS_LOOP_DEPEND"
    ))
}



############
# extractFunctions.R
removeFromBody =
function(b, index)
{
  b$body = b$body[- index]
}


collectRemoveFun =
function(body)
{
    funcs = list()
    function(expr, i) {

           #XXX This should check that the body of the function does not call <<-
        if(is(expr, "Assign") && is(expr$read, "Function") 
           && is(expr$write, "Symbol")) {
            # collect this
            # funcs <<- append(funcs, expr)
            funcs[[ expr$write$value ]] <<- expr
            # Remove from the body
            removeFromBody(body, i)
        }
    }
}


extractFunctions =
    #
    #  Perhaps use indexWalkCode()
    #  or just write a walkCode() walker that removes the functions
    #  and stores them so can return the updated code and the function.
    #  Will look for function definitions in the current expression,
    #  i.e. from the parent from which we will have to remove them.
    #
function(fun)
{
    b = to_ast(body(fun))
    col = collectRemoveFun(b)

    # Note we go from last to first so that if we remove an element
    # this doesn't change the index of the next element.
    mapply(col, b, rev(seq(along = b$body)))

    funcs = lapply(environment(col)$funcs, as_language)
    body(fun) = as_language(b)
    list(fun = fun, externalFunctions = funcs)
}
