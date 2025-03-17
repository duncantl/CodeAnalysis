mkModifyCodeWalker = mkConstPropWalker =
function(rewrite = function(x, ...) x, skipIfFalse = TRUE, mkLiteralMap = FALSE, ..., verbose = FALSE)
{
    Invalid = structure(NA, class = "Invalid")
    map = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(verbose) {
            message("leaf: ", ty)
            print(x)
        }
        
        if(ty %in% c("pairlist", "expression", "list", "language")) {
            return(lapply(x, walkCode, w))
            # was return(NULL)
        } else if(ty == "closure") {
            fm = walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
            b = walkCode(body(x), w)
            # build the new version of the function
#            browser()
            f = function() {}
            formals(f) = fm
            body(f) = b
            environment(f) = environment(x)
            return(f)
        } else
            x
        
    }

    
    call = function(x, w) {
        if(verbose) {
            message("call:")
            print(x)
        }
        
#        browser()
        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)

        els = as.list(x)
        isName = is.name(x[[1]])
        if(isName && as.character(x[[1]]) %in% c(".Internal", ".Primitive")) 
            els = els[-2]

        y = x
        assignmentOps = c("<-", "=", "<<-")
        cur = 1L
        for (i in seq(along.with = els)) {
            el = els[[i]]
            if (!missing(el)) {
                tmp = walkCode(el, w)
                if(is.null(tmp)) {
                    message("null value from walkCode()")
                    browser()
                }

                # if this is a simple assignment and we are dealing
                # with the lhs, don't rewrite()
                if(i == 2 && isName && as.character(x[[1]]) %in% assignmentOps) {
                    if(isComplexAssignTo(x)) {
                        tmp = rewrite(tmp, w, map)
                    }
                } else
                    tmp = rewrite(tmp, w, map)


                if(is.null(tmp)) {
#                    message("null value from rewrite()")
                    x = x[-cur]
                    next
                }
                
                x[[cur]] = tmp   # els[[cur]] = tmp
                cur = cur + 1L
            }
        }

        # Process assignments to update the map of variable values.
        # Do this at the end after folding constants.
        if(mkLiteralMap && isSymbol(x[[1]], c("<-", "=", "<<-"))) { #  && is.call(x[[2]])) {

            var = CodeAnalysis:::getAssignedVars(x)
            literal_p = isLiteral(x[[3]])
        # isComplexAssignTo(x)
            if(is.call(x[[2]]) || !literal_p)
                map[[var]] <<- Invalid
            else
                map[[ as.character(var[[1]] )]] <<- x[[3]]
        }

        x
    }    

    list(handler = function(x, w) NULL, leaf = leaf, call = call, ans = function() calls )
}

###################################
# Generator functions to create the function we pass to the mkModifyCodeWalker()

genRemoveCode =
function(pred)
{
    function(x, w, ...) {
        if(pred(x))
            NULL
        else
            x
    }
}


genAddArgsToCalls =
function(funArgs)    
{
    function(x, w, ...) {
        if(isCallTo(x, names(funArgs))) {
            args = funArgs[[ as.character(x[[1]]) ]]
            x[names(args)] = sapply(args, as.name)
            x
        } else
            x
    }
}

genRewriteVars =
function(names)    
{
    function(x, w, ...) {
        if(is.name(x) && (v <- as.character(x)) %in% names(names))
            as.name(names[v])
        else
            x
    }
}



###################
# Examples/tests for the different generator functions and modifying the code.


if(FALSE) {
    fun = function(x, y, z) {
        a = 2
        b = 1+2
        c = a + b
        foo(a, b, c)

        # remove the intermediate assignment, but leave for now so still have 2 commands.
        ans = (x + y)*z
        ans
    }

    p = function(x, ...) {
        isCallTo(x, "foo") || isAssignTo(x, c("a", "c"))
    }

    rw = genRemoveCode(p)
    w = CodeAnalysis:::mkConstPropWalker(rw, FALSE)
    f2 = walkCode(fun, w)    
}


if(FALSE) {

    fun2 = function(x, y, z) {
        len = sapply(list(x, y, z), length)
        if(any(len) == 0)
            stop("zero-length argument")
        
        a = function(v) x+v
        b = function(o) o/z

        while(length(z) > 1){
            w = a(x) > 10 & b(y) < 100
            z = z[w]
            x = x[w]
            y = y[w]
        }

        z
    }

    p = function(x, ...) 
           isCallTo(x, "function")


    rw = genRemoveCode(p)
    w = CodeAnalysis:::mkConstPropWalker(rw, FALSE)
    f2 = walkCode(fun2, w)        
    
}

##


if(FALSE) {

    f9 = function(n, B = 999) {
        replicate(B, { a = f(rnorm(n)); mean(g(a))})
    }
    
    fv = list(f = c("alpha" = "alpha1", "beta" = "beta1"), g = c("alpha" = "alpha2"))
    
    rw = genAddArgsToCalls(fv)
    w = mkConstPropWalker(rw, FALSE)
    f2 = walkCode(f9, w)    
}

##

if(FALSE) {

    f = function(x) {
        alpha *x + beta
    }

    rw = genRewriteVars(c(alpha = ".alpha", beta = ".beta"))
    w = mkConstPropWalker(rw, FALSE)
    f2 = walkCode(f, w)    
}


##

if(FALSE) {
    ef = parse("~/GitWorkingArea/CodeAnalysisWORstatic/explorations/constProp.R")
    source("~/GitWorkingArea/CodeAnalysisWORstatic/explorations/propagate.R")
    chVar = function(x, w, map, ...) {
        #    browser()
        if(is.name(x)) {
            v = as.character(x)
            if(v %in% names(map) && !inherits(map[[v]], "Invalid"))
                return(map[[v]])
        }
        
        x	   
    }

    w = mkConstPropWalker(chVar, FALSE)
    o = walkCode(ef$f1, w)
}
