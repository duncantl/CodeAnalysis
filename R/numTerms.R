# walkCode.  No documentation for the model
#  if language object -
#     if first element symbol or character
#        call handler
#        get result
#        if not NULL call the result as a function
#          otherwise call  call() element
#     otherwise call call()
#  otherwise call leaf()


mkCounter =
function(self = FALSE, ctr = 0L, skipIfFalse = TRUE)
{
    els = list()
    leaf = function(x, w, ...) { #print((x));
        ty = typeof(x)
        if(ty %in% c("pairlist", "expression", "list", "language")) {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w)
            walkCode(body(x), w)
            if(!self) return(NULL)
        }

         ctr <<- ctr + 1L
         els[[ctr]] <<- x

        NULL
    }
    call = function(x, w) {

              if(skipIfFalse && skipIfFalse(x, w))
                  return(NULL)
        
              if(self) w$leaf(x)

              for (ee in as.list(x))
                  if (!missing(ee))
                      walkCode(ee, w)
    }
    
    list(handler = function(x, w) NULL ,
         call = call,
         leaf = leaf,
         .result = function() ctr,
         .els = function() els
        )
}

numTerms =
function(x, ctr = mkCounter(self = self, skipIfFalse = skipIfFalse), skipIfFalse = TRUE, self = FALSE)
{
    walkCode(x, ctr)
    ctr$.result()
}

getTerms =
    # get the terms the code walker sees.
function(code)
{
    ctr = mkCounter()
    walkCode(code, ctr)
    ctr$.els()
}

