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
function(nested = TRUE, skipIfFalse = TRUE)
{
    ans = list()
    
    leaf = function(x, w, ...) {
        ty = typeof(x)
        if(ty %in% c("pairlist", "expression", "list", "language")) {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) 
            walkCode(body(x), w)
            return(NULL)
        } 
    }

    call = function(x, w) {

        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)
        
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

    list(handler = function(x, w) NULL,
         leaf = leaf,
         call = call,
         ans = function() ans
        )
}


# Example
# findLoops("../nestedFor.R")
# Find both loops.

findLoops =
function(f, nested = TRUE, code = parse(f),
         w = mkLoopWalker(nested, skipIfFalse = skipIfFalse), skipIfFalse = TRUE)
{
    # At one point had
    # if(is(code, "for")) code = code[length(code)]
    # but this appears not to be necessary now.
    # Perhaps additions to the code walker to handle language and list objects.

    if(missing(code) && isRCode(f))
        code = f
    
    walkCode(code, w)
    w$ans()
}

isRCode =
function(x)
{
    typeof(x) %in% c("language", "expression", "name", "closure", "call", "pairlist")
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

    # Get the top-level loops.
    # Should this be inside the if(is.character())??
    tl = findLoops(code = code, nested = FALSE)
    
    # call numNestedLoops() on each of these
    ans = sapply(tl, function(x) length(findLoops(code = x)))
    # If recursive
    sum(ans)
}

