#
# This is an experiment in creating a version of walkCode() from codetools
# that allows us to collect indices if elements in the AST that match some criteria.
# The intent is to be able to use these indices to
# a) move up the tree, e.g., to check the parent, ancestors and siblings of a node in the predicate
#    or after the traversal, and
# b) modify the AST after the travesal by inserting new/updated language objects at a specific location.
#
# Nick (Ulle) did this in rstatic::find_nodes().
#
# This comes from working on copyParameters.R (which I'll move here soon) and rewriting
# functions - formals and body - to add parameters to calls.  I used rstatic to be able to
# update the language objects. 
#
#

if(FALSE) {
    f = function(x, y = do.call(foo, x))  x + y
    g = function(x, y ) {
           z = do.call(foo, list(x, y))
           z + 1L
        }

    eg3 = function(x, y, df) {
            o = order(x)
            o2 = do.call(order, list(x, y))

            o3 = do.call(order, list(x, y, na.last = FALSE))

            # Need to update args - or inline c(args, p1 = p1, p2 = p2)
            args = list(x, y)
            args$z = x + y
            o2 = do.call(order, args)

            # not order
            o3 = do.call(foo, args)

            o4 = lapply(df, order, decreasing = TRUE)
            o5 = mapply(order, x, y)
            o5 = mapply(order, x, y, MoreArgs = list(decreasing = TRUE))

            o5 = mapply(order, x, y, MoreArgs = list(decreasing = TRUE, method = "radix"))

            o5 = mapply(order, x, y, MoreArgs = foo(x + y))        

            foo(x[order(y)])
         }
}

if(FALSE) {
    
    pred = function(x, idx, type, ast) {
                k = is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) == "do.call"
                if(k) browser()
                k
    }

    indexWalkCode(g, pred)

    w = mkIndexWalker(pred, g)
    walkCode2(g, w, idx = integer(), NA)
    
    w = mkIndexWalker(pred, f)
    walkCode2(f, w, idx = integer(), NA)

    sc = parse("~/GitWorkingArea/CodeAnalysis/R/indexASTWalker.R")[[2]][[3]]
    i = indexWalkCode(sc, mkIsCallTo("is.name"))
    k = getByIndex(sc, i[[1]])
    k[[1]] = as.name("isName")
    sc2 = insertByIndex(k, sc, i[[1]])


    # example of rewriting calls to order() to add decreasing = decreasing and na.last = na.last
    # Comes from copyParameters.R example.

    eval(parse("~/GitWorkingArea/CodeAnalysis/R/indexASTWalker.R")[[1]][[3]])

    # Find the locations of the direct calls to order in the body.
    # Get the call objects using the indices.
    # Rewrite them to add named arguments
    # Insert them into the body
    # Update the body of eg3
    b = body(eg3)
    i = indexWalkCode(b,  mkIsCallTo("order"))
    k = lapply(i, function(i) getByIndex(b, i))
    k2 = lapply(k, function(x) { x[ c("decreasing", "na.last")] = sapply(c("decreasing", "na.last"), as.name); x})
    for(j in seq(along.with = i)) 
        b = insertByIndex(k2[[ j ]], b, i[[ j ]])
    body(eg3) = b
    # Note that we can't use lapply() as we are updating b in each iteration.
    # Also, if we change the structure of b, the following indices may not be correct.
}

if(FALSE)  {
    # use the index walker to check the parent in a predicate function.
    # Find the use of args in the call
    #  o2 = do.call(order, args)
    #
    pred2 = function(x, idx, type, ast) {
        print(x)
            isSymbol(x, "args") &&
            isCallTo(p <- getParent(ast, idx, type = type), "do.call") &&
            isSymbo(p[[2]], "order" )
      }

    idx = indexWalkCode(body(eg3), pred2)
    

}


mkIsCallTo =
function(fun)    
    function(x, idx, type, ast) 
        is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% fun


indexWalkCode =
function(code, pred)
{
    w = mkIndexWalker(pred, code)
    walkCode2(code, w, idx = integer(), type = NA)
    w$ans()
}

mkIndexWalker = 
function(pred, ast)
{
    ans = list()

    leaf = function(x, w, idx, type) {
        ty = typeof(x)
        if(ty == "pairlist" || ty == "list") {
            lapply(seq(along.with = x), function(i) walkCode2(x[[i]], w, c(idx, i), type))
            return(NULL)
        } else if(ty == "closure") {
            walkCode2(formals(x), w, idx, "formals") # lapply(formals(x), walkCode, w)
            walkCode2(body(x), w, idx, "body")
        } else if(ty == "symbol")
            pred(x, idx, type, ast)

        NULL
    }

    capture =
        function(idx, type) {
#            browser()
            idx = c(if(is.na(type))
                       NA
                    else if(type == "body")
                        1L
                    else 0L,
                    idx)
            ans[[ length(ans) + 1L]] <<- idx
        }
    
    
    call = function(x, w, idx, type) {
        if(pred(x, idx, type, ast))
            capture(idx, type)

        tmp = as.list(x)
        for(i in seq(along.with = tmp)) {
            ee = tmp[[i]]
            if (!missing(ee))
                walkCode2(ee, w, c(idx, i), type)
        }
    }

    list(handler = function(...) NULL,
         call = call,
         leaf = leaf,
         ans = function() {
                 ans
             })    
}


getIndexObj = 
function(ast, idx, type = NA, dropSelf = TRUE)
{    
    obj = if(is.na(type)) {
              ty = idx[1]
              idx = idx[-1]
              if(is.na(ty))
                  ast
              else switch(as.character(ty),
                     "0" = formals(ast),
                     "1" = body(ast),
                     ast)
          } else               
              switch(type,
                     body = body(ast),
                     formals = formals(ast),
                     ast)

    if(dropSelf)
        idx = idx[ - length(idx)]

    list(obj = obj, idx = idx)
}

getComponent =
function(idx, type = NA)    
{
    if(is.na(type)) {
        ty = idx[1]
         switch(as.character(ty),
               "0" = "formals",
               "1" = "body",
                "")
    } else               
        switch(type,
               body = "body",
               formals = "formals",
               "")    
}

insertByIndex =
function(x, ast, idx, type = NA)
{
    tmp = getIndexObj(ast, idx, type)

    comp = getComponent(idx, type)
    comp = if(comp == "") "ast" else sprintf("%s(ast)", comp)
    txt = sprintf("%s%s <- x",
                  comp, 
                  paste(sprintf("[[ %d ]]", c(tmp$idx, idx[length(idx)])), collapse = "" ))
    e = parse(text = txt)

    val = eval(e, sys.frame(sys.nframe()))
    ast
}


getByIndex =
function( ast, idx, type = NA)
{
    p = getParent(ast, idx, type)
    p[[ idx[ length(idx) ] ]]
}

getParent =
function(ast, idx, type = NA)
{
    tmp = getIndexObj(ast, idx, type)

    obj = tmp$obj
    for(i in tmp$idx)
        obj = obj[[i]]

    obj
}

getAncestors =
    #
    # Need to enhance to handle when type is NA and the type is the first element
    # of idx. Same as getParent().
    #
function(ast, idx, type = NA)
{
    tmp = getIndexObj(ast, idx, type)
    
    orig = obj = tmp$obj
    idx = tmp$idx
    
    ans = vector("list", length(idx))
    for(i in seq(along.with = idx) )
        ans[[i]] = obj = obj[[i]]

    # add the body or parameter and then the ast itself.
    rev(c(ast, orig, ans))
}



# Copied from the codetools package and adapted to allow additional arguments
# so that we can pass the index  as we loop over elements in the call, leaf and other handler functions.
# The changes are adding ... in the function signature and in the calls to the different handlers.
# These walker-specific functions have to accept these additional arguments.
walkCode2 =
function (e, w = makeCodeWalker(), ...) 
{
    if (typeof(e) == "language") {
        if (typeof(e[[1]]) %in% c("symbol", "character")) {
            h <- w$handler(as.character(e[[1]]), w)
            if (!is.null(h)) 
                h(e, w, ...)
            else w$call(e, w, ...)
        }
        else w$call(e, w, ...)
    }
    else w$leaf(e, w, ...)
}
