mkLiteralCollector =
    #
    # XXX
    # This doesn't collect names such as the "abc/def"
    #  c("abc/def" = "xyz")
    #
function(ignoreParams = TRUE, skipIfFalse = TRUE, predicateFun = isLiteral)
{
    values = list()
    leaf = function(x, w, ...) {
        if(inherits(x, "srcref"))
            return(NULL)
        
        ty = typeof(x)
        if( (ty == "pairlist" && !ignoreParams) || ty %in% c("list", "language", "expression")) { #XXX add expression, list, language ?
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            # ignore default values as literals here are okay.
            if(!ignoreParams)
                walkCode(formals(x), w) 
            walkCode(body(x), w)
        }

        if(predicateFun(x, ty)) 
            values[[ length(values) + 1L]] <<- x

        NULL
    }
    call = function(x, w) {
             if(skipIfFalse && skipIfFalse(x, w))
                 return(NULL)
        
             for (ee in as.list(x))
               if (!missing(ee))
                  walkCode(ee, w)
                        }
    list(handler = function(x, w) NULL ,
         call = call,
         leaf = leaf,
         .values = function() values)

}

isLiteral2 =
    # Old version. I suggest not using this but isLiteral below instead.
    # So have changed the name of this and added the one below.
function(x, type = typeof(x))
{
    # "logical", 
   type %in% c("integer", "numeric", "character", "complex", "double")
}

isLiteral =
function(x, type = typeof(x))
{
    type %in% c("logical", "integer", "numeric", "character", "complex", "double", "NULL")  ||
       ( isCallTo(x, "c") && all(sapply(x[-1], isLiteral)) )
}

literalType =
    # Assumes isLiteral() already verified before call
    ##
    # literalType(TRUE)
    # literalType(1L)
    # literalType(1)    
    # literalType("abc")
    # literalType(quote(c("abc", 1, 2L)))
    # literalType(quote(list("abc", 1, 2L)))    
    #
function(x)
{
    if(is.call(x)) {
        if(isCallTo(x, "list"))
            return("list")

        # So must be c()
        ty = sapply(x[-1], literalType)
        
        # now determine the common type.
        m = c(logical = 0, integer = 1, double = 2, complex = 3, character = 4)
        names(m)[ m == max(m[ty])]
        
    } else
        typeof(x)
}



findLiterals =
function(code, walker = mkLiteralCollector(ignoreParams, skipIfFalse = skipIfFalse, ...),
         ignoreParams = TRUE, skipIfFalse = TRUE, ...)
{
    walkCode(code, walker)
    walker$.values()
}

