

mkModifyCodeWalker = 
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
        if(ty == "pairlist") {
            # When processing the pairlist in the next if() in the same way as list/language/expression
            # the function we created by putting this list as the formals() was corrupted
            # Error: badly formed function expression
            # when we printed it. And we couldn't used it.
            # So we modify the elements of the pairlist directly w/o creating a new pairlist.

            for(i in seq(along = x)) {
                tmp = walkCode(x[[i]], w)
                if(!missing(tmp) && !is.null(tmp))
                    x[[i]] = tmp
            }
            
            return(x)
            
        } else if(ty %in% c("expression", "list", "language")) {
            return(lapply(x, walkCode, w))
            # was return(NULL)
        } else if(ty == "closure") {
            fm = walkCode(formals(x), w) 
            b = walkCode(body(x), w)
            # build the new version of the function
            fun = function(x) {}            
            formals(fun) = fm
            body(fun) = b
            environment(fun) = environment(x)
            return(fun)
        } else
            x
    }

    
    call = function(x, w) {
        if(verbose) {
            message("call:")
            print(x)
        }
        
        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)

        els = as.list(x)
        isName = is.name(x[[1]])
        if(isName && as.character(x[[1]]) %in% c(".Internal", ".Primitive")) 
            els = els[-2]

        assignmentOps = c("<-", "=", "<<-")
        cur = 1L

        for (i in seq(along.with = els)) {
            el = els[[i]]

            #?? if  is.null(el) next and leave it there.

            if (!missing(el)) {

                if(is.null(el)) {
                    cur = cur + 1L
                } else {
                
                tmp = walkCode(el, w)

                # ??? Get rid of this and put the map contruction into the predicate.
                # It is for constant propagation only.
                # if this is a simple assignment and we are dealing
                # with the lhs, don't rewrite()
                if(i == 2 && isName && as.character(x[[1]]) %in% assignmentOps) {
                    if(isComplexAssignTo(x)) {
                        tmp = rewrite(tmp, w, map)
                    }
                } else 
                    tmp = rewrite(tmp, w, map)


                if(is.null(tmp)) {
                    x = x[-cur]
                    next
                }
                
                x[[cur]] = tmp   # els[[cur]] = tmp
                cur = cur + 1L
                }
            } else
                cur = cur + 1L
        }

        # Process assignments to update the map of variable values.
        # Do this at the end after propogating constants.
        # We could have the rewwrite function do this. It will be called for every element
        # if we remove the if(i==2... above for simple assignments. Just let those
        # go to the rewrite function.
        # 
        if(mkLiteralMap && isSymbol(x[[1]], c("<-", "=", "<<-"))) { #  && is.call(x[[2]])) {

            var = getAssignedVars(x)
            literal_p = isLiteral(x[[3]])
        # isComplexAssignTo(x)
            if(is.call(x[[2]]) || !literal_p)
                map[[var]] <<- Invalid
            else
                map[[ as.character(var[[1]] )]] <<- x[[3]]
        }

        x
    }    

    list(handler = function(x, w) NULL, leaf = leaf, call = call) # , ans = function() NULL )
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



