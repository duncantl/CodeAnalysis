if(FALSE) 
findLoops =
function(f, code = parse(f))
{
    k =  to_ast(code)
    find_nodes(k, function(x) is(x, "For") || is(x, "While"))
}

############
# The real one

mkLoopWalker =
    #
    # Find loops.
    # If nested = FALSE, don't descend in to the loops so don't find nested loops.
    #
function(nested = TRUE)
{
    ans = list()
    
    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty == "pairlist" || ty == "expression") {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
            walkCode(body(x), w)
            return(NULL)
        } 
    }

    call = function(x, w) {

        isName = is.name(x[[1]])
        if(isName && as.character(x[[1]]) %in% c("for", "while", "repeat")) {
            ans[[length(ans) + 1L]] <<- x
            if(!nested)
                return(NULL)
        }
        
        
        els = as.list(x)
        for (ee in els) {
            if (!missing(ee))
                walkCode(ee, w)
        }
    }    

    list(handler = function(x, w) NULL, leaf = leaf, call = call, ans = function() ans )
}


# Example
# findLoops("../nestedFor.R")
# Find both loops.

findLoops =
function(f, nested = TRUE, code = parse(f), w = mkLoopWalker(nested))
{
    if(is(code, "for")) {
        # Need to analyze the second 
        code = code[length(code)]
    }
    
    walkCode(code, w)
    w$ans()
}


numNestedLoops =
    #
    # given either a file, a character vector of code or a language object
    # (e.g., a for loop object), count the number of loops it contains.
    #
function(code, recursive = TRUE)
{
    if(is.character(code)) {
        if(file.exists(code))
            code = parse(code)
        else
            code = parse(text = code)
    }
    if(inherits(code, "for"))
        tl = list(code)
    else
        # Get the top-level loops.
        # Should this be inside the if(is.character())??
        tl = findLoops(code = code, nested = FALSE)
    
#browser()    
    # call numNestedLoops() on each of these
    sum(sapply(tl, function(x) length(findLoops(code = x))))
}

