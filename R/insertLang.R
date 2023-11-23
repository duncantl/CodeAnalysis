
insertLang =
    #
    #  Make the multiple expression version use a single subsetting
    #  rather than calls
    #
    #  Fix after = 0 in example in Rd case
    #  insertLang(quote(print(c(length(x), length(y)))), f, 0)
    #  Get function (x, y) print(c(length(x), length(y)))(x + sin(y))
    #
    # √ handle case when body is not a { but a single call
    #
    # √ Also allow before = integer() but only one of before and after
    #    This is to allow inserting at the beginning.
    # √ Probably just use after = -1 
    #
    # √ What if after is > length(b). get NULLs in the result.
    #
    # √ Allow for a list of expr and a vector of after.
    # Note that the target positions will increase as we insert a new element.    
    #
    # Need to describe in help page what is indexing model.
    # The at argument for trace()  includes the { in the indexing.
    # It could be cleaner for insertLang to ignore that and have 1 be
    # the first expression.
    # Then if there is no {} for the body, but just a call/symbol, then 1 is still the first position.
    # With a {, after 1 would mean immediately after the first call/symbol.
    # And this would be the same for no {}.
    #
    #
function(expr, to, after = integer(), useSplice = TRUE, includeBraceInCount = FALSE)
{
    # Helper function we i
    fixResult = function(to, b) {
        if(is.function(to)) {
            body(to) = b
            to
        } else
            b
    }
    
    if(is.function(to))
        b = body(to)
    else
        b = to

    # If this is a single expression - name or call - and not a { }, we need to make this a {}
    # and add the original call/name.  Then we can add/insert expr.
    #
    # body could have a user defined class.
    # So may want to check class is name or call.
    # typeof(b) could be language for a { or a call.
    
    if(class(b) != "{") {
        tmp = function() {x}
        b2 = b
        b = body(tmp)
        b[[2]] = b2
        if(length(after) && after > 0)
            after = after + 1L
    }


    if(useSplice)  
        b = spliceIn(expr, b, after, includeBraceInCount)
    else {
        # check for list() of language objects.
        if(is.list(expr) && (nex <- length(expr)) > 1) {
            if(length(after) == 1)
                after = rep(after, nex)
            else if(length(after) != nex)
                stop("after needs have the same number of elements as expr")

            # when we add the first element of expr, the next value of after
            # needs to account for the new entry, and so on.
            after = after + seq_len(nex) - 1L
            for(i in seq(along.with = expr)) 
                b = insertLang(expr[[i]], b, after = after[i], useSplice = FALSE, includeBraceInCount = includeBraceInCount)

            return(fixResult(to, b))
        }

        if(length(after) && after > length(b)) {
            warning("after is beyond the length of the body - truncating")
            after = length(b)
        }

        # if after is empty or the end of the body, just append expr
        if(length(after) == 0 || is.na(after) || after == length(b) ) {
            b[[length(b) + 1L ]] = expr
        } else {
            if(after < 0) {
                idx = c(1, seq_len(length(b)))
                after = 1L
            } else
                idx = c(seq_len(after), after, seq(after + 1L, length(b)))

            if(includeBraceInCount)
                after = after + 1L
            
            b2 = b[idx]
            b2[[ after + 1L]] = expr
            b = b2
        }
    }
    
    fixResult(to, b)
}


spliceIn =
function(expr, b, after = integer(), includeBraceInCount = FALSE)
{
    if(length(after) == 0 || (length(after) == 1 && is.na(after)))
        after = length(b)
    else if(length(after) == 1) {
        if(after < 1)
            after = 1L
        else
            after = after + 1L
    } 

    multi = is.list(expr)
    reps = rep(1, length(b))
    if(multi) {
        if(length(after) == 1) {
            reps[ after ] = length(expr) + 1L
            after = after + seq_len(length(expr))
        } else if(length(after) != length(expr)) 
            stop("after must have same length as expr")
        else {
            w = after < 0
            if(any(w))
                after[w] = 0L
            reps[ after + 1L ] = 2L
            after = after + 1L + seq_len(length(expr))
        }

#        browser()                
    } else
        reps[after] = 2L

    sub = rep(seq_len(length(b)), reps)
    b2 = b[sub]

    if(!includeBraceInCount)
        after = after - 1L
    
    if(multi) 
        b2[ after ] = expr
    else 
        b2[[ after + 1L ]] = expr

    b2
}

if(FALSE) {

    f = function() {
        e1
        e2
        e3
        e4
        e5
    }
    nw = list(quote(n1), quote(n2), quote(n3))

    spliceIn(nw[[1]], body(f), after = 3)
    spliceIn(nw[[1]], body(f), after = -1)
    spliceIn(nw[[1]], body(f))
    
    spliceIn(nw, body(f), after = 3)
    spliceIn(nw, body(f), after = c(2, 4, 5))
    spliceIn(nw, body(f), after = -1)
}




if(FALSE) {

    insertLang(quote(print("hi")), insertLang, -1)
    insertLang(quote(print("hi")), insertLang)
    insertLang(quote(print("hi")), insertLang, 4)

    f = function(x) x + 1
    insertLang(quote(print("hi")), f, -1)
    insertLang(quote(print("hi")), f)
    insertLang(quote(print("hi")), f, 1)


    # Insert multiple calls

    insertLang(list(quote( x <- 101), quote(print("hi"))), insertLang, 1)
    
    tmp0 = insertLang(list(quote( x <- 101), quote(print("hi"))), insertLang, c(1, 4))
    # Check same as doing it separately but caller has to know to increment by previous number of elements added.
    tmp = insertLang
    tmp = insertLang(quote(x <- 101), tmp, 1)        
    tmp = insertLang(quote(print("hi")), tmp, 5)
    stopifnot(identical(tmp, tmp0))
}
