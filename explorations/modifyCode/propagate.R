library(codetools)

mkConstPropWalker =
function(rewrite = function(x, ...) x, skipIfFalse = TRUE, mkLiteralMap = FALSE, ...)
{
    Invalid = structure(NA, class = "Invalid")
    map = list()

    leaf = function(x, w, ...) {
        ty = typeof(x)
        message("leaf: ", ty)
        print(x)
        if(ty %in% c("pairlist", "expression", "list", "language")) {
            return(lapply(x, walkCode, w))
            # was return(NULL)
        } else if(ty == "closure") {
            fm = walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
            b = walkCode(body(x), w)
            # build the new version of the function
            browser()
            f = function() {}
            formals(f) = fm
            body(f) = b
            environment(f) = environment(x)
            return(f)
        } else
            x
        
    }

    
    call = function(x, w) {
        message("call:")
        print(x)
#        browser()
        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)

        els = as.list(x)
        isName = is.name(x[[1]])
        if(isName && as.character(x[[1]]) %in% c(".Internal", ".Primitive")) 
            els = els[-2]

        y = x
        assignmentOps = c("<-", "=", "<<-")
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
                
                x[[i]] = els[[i]] = tmp
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

if(FALSE) {

    f9 = function(n, B = 999) {
        replicate(B, { a = f(rnorm(n)); mean(g(a))})
    }
    
    fv = list(f = c("alpha" = "alpha1", "beta" = "beta1"), g = c("alpha" = "alpha2"))
    
    rw = genAddArgsToCalls(fv)
    w = mkConstPropWalker(rw, FALSE)
    f2 = walkCode(f9, w)    
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

if(FALSE) {

    f = function(x) {
        alpha *x + beta
    }

    rw = genRewriteVars(c(alpha = ".alpha", beta = ".beta"))
    w = mkConstPropWalker(rw, FALSE)
    f2 = walkCode(f, w)    
}



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
