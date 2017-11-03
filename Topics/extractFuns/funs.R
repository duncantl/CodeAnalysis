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

        if(is(expr, "Assign") && is(expr$read, "Function") 
           && is(expr$write, "Symbol")) {
            # collect this
            # funcs <<- append(funcs, expr)
            funcs[[ expr$write$name ]] <<- expr
            # Remove from the body
            removeFromBody(body, i)
        }
    }
}



extractFunctions =
function(fun)
{
    b = to_ast(body(fun))
    col = collectRemoveFun(b)

    # Note we go from last for first so that if we remove an element
    # this doesn't change the index of the next element.
    invisible(mapply(col, b$body, rev(seq(along = b$body))))

    funcs = lapply(environment(col)$funcs, to_r)
    body(fun) = to_r(b)
    list(fun = fun, externalFunctions = funcs)
}
