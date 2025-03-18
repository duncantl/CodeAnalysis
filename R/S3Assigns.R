#################################################

S3Assignments =
function(code, walker = mkS3AssignWalker(...), ...)
{
    walkCode(code, walker)
    walker$ans()
}

mkS3AssignWalker =
function(recursive = TRUE, skipIfFalse = TRUE)
{

    defs = list()

    leaf = function(x, w, ...) {
        if(inherits(x, "srcref"))
            return(NULL)
        
        ty = typeof(x)
        if(ty %in% c("pairlist", "list", "expression", "language")) {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) 
            walkCode(body(x), w)
        } else if(ty == "call" && is.name(x[[1]]) && as.character(x[[1]]) == "function") {
            #browser()
            walkCode(eval(x), w)
        } 

        NULL
    }

    
    call = function(x, w) {

        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)
         
        if(isS3Assign(x)) { 
            defs[[ length(defs) + 1L ]] <<- extractS3Class(x) 
                          # structure(class = "aat_splithalf") only has 2 elements.  In AATools
        }
        for (ee in as.list(x))
            if (!missing(ee))
                walkCode(ee, w)
    }

    list(handler = function(x, w) NULL,
         call = call,
         leaf = leaf,
         ans = function() defs)
}

isS3Assign =
    #
    # See  isS3ClassSetNode in packageAnalysis.R
    #
function(x)
{
    (is.name(x[[1]]) && ((f <- as.character(x[[1]])) == "<-" ||  f == "=" || f == "<<-") &&
     is.call(x[[2]]) && is.name(x[[2]][[1]]) && (as.character(x[[2]][[1]]) == "class")) ||
         ( is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) == "structure" && "class" %in% names(x))

    #                     (length(x) == 3 && is.call(x[[3]]) && is.name(x[[3]][[1]]) && as.character(x[[3]][[1]]) == "structure" && "class" %in% names(x[[3]])))
}

extractS3Class =
function(x)
{
    val = x 

    if(isAssignTo(val))
        val = val[[3]]
    
    
    if(is.call(val) && as.character(val[[1]]) == "structure")
       val = val$class

    if(is.call(val) && is.name(val[[1]]) && as.character(val[[1]]) == "c")
            return(sapply(val[-1], function(x) if(is.character(x)) x else NA))

    if(is.character(val))
        return(val)

    if(isCallTo(val, "if")) {
        tmp = as.list(val[-c(1,2)] )
        if(all(sapply(tmp, isLiteral)))
            return( structure(unlist(tmp), class = "Either"))
    }
    
    NA
}

if(FALSE) {

    tst = function(x) {
        x = x + 1
        class(x) = structure(lapply(x, foo), class = c("a", "x"))
        class(x) = structure(lapply(x, foo), class = "y")
        class(x) = c("1", "2")
        class(x) =  "A"
        class(x) <- structure(lapply(x, foo), class = c("B", "C"))
        class(x) <- structure(lapply(x, foo), class = "D")
        class(x) <- c("E", "F")
        class(x) <-  "G"
        class(x) <-  foo("H")
        class(x) <-  structure(x, class = foo("I"))
        class(x) <-  structure(x, class = c("J", foo("K"))  )
        1+2
        x
    }
    # Missing H and I


    tst2 =
        function(x) {


            lapply(x, function(z) {
                class(z) = "other"
                z
            })
        }

    tst3 =
        function(x) {

            z = list()

            lapply(x, function(z) {
                class(z) <<- "other"
                z
            })
        }    


tmp = lapply(tufuns, S3Assignments2)    
tmp0 = lapply(tufuns, S3Assignments)
    w = sapply(tmp, length)
    w0 = sapply(tmp0, length)

   bad = names(w)[(w != w0)]

    
tmp2 = unlist(tmp, recursive = FALSE)    


tmp00 = unlist(tmp0, recursive = FALSE)
}

