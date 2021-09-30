#
#  Given a function, we call findIfInFun() to get the If expressions.
#  getIfValue() then gives us the return values from an If statement
#  Finally callType() determines the type of each return value.
#      i = findIfInFun(foo2)
#      v = getIfValue(i[[1]])
#   



findIf =
function(pkg, ns = getNamespace(pkg), vars = ls(ns, all = TRUE))
{
    ans = lapply(vars, function(v) findIfInFun((get(v, ns))))
    names(ans) = vars
    ans[ sapply(ans, length) > 0]
}

findIfInFun =
function(f)
{
    if(!is.function(f) && !is(f, "Function"))
        return(list())

    if(is.function(f))
        f = to_ast(f)
    
    find_nodes(f, function(x) is(x, "If"))
}



getIfValue =
function(x)
    UseMethod("getIfValue")

getIfValue.default =
function(x)
{
    if(inherits(x, "Parenthesis")) cat("getIfValue.Parenthesis\n")
    x
}

getIfValue.Brace =
function(x)
    if(length(x$contents))
        getIfValue(x$contents[[length(x$contents)]])

getIfValue.If =
function(x)    
{
    ans = list(true = getIfValue(x$true))
    if(length(x$false$contents))
       ans$false = getIfValue(x$false)

    ans
}


getIfValue.Call =
function(x)    
{
    if(is(x$fn, "Symbol")) {
        switch(x$fn$value,
               tryCatch = tryCatchValue(x),
               x)
    } else
        x

}

tryCatchValue =
function(x)    
{
  lapply(x$args$contents, getIfValue)
}

getIfValue.Function =
function(x)
{
   getIfValue(x$body)
}

getIfValue.Symbol =
    # Find the value assigned to the symbol.
function(x)
{
    ans =  findAssignTo(x)
#    if(identical(ans, x)) {   }      # didn't find any assignments.
    ans
}

findAssignTo =
    #
    # This needs to be extended to handle skipping
    #  names(x) = value
    #  attr(x, "xx") = value
    # and finding defininga
    #
    #
function(x, var = x$value)
    UseMethod("findAssignTo")

findAssignTo.Symbol =
function(x, var = x$value)
{
    v = findAssignTo(x$parent, var)
    if(is.null(v))
        return(x)
    else
        v
}

findAssignTo.Brace =
function(x, var = x$value)
{
    #XXX Need to recognize if() statements that assign var within the if() or else()
    #  lapply(x$contents, getIfValue)
    w = sapply(x$contents, function(x) inherits(x, c("Assignment", "Replacement1")) && is_symbol(x$write, var))
    if(any(w)) {
#        browser()
        return(x$contents[[which.max(w)]]$read)
    }

    p = x$parent
    if(is(p, "If"))
        p = p$parent
    if(is(p, "Function")) {
#        return(findAssignTo(p$body, var))
        return(NULL)
    }

    findAssignTo(p, var)
}

findAssignTo.ArgumentList =
function(x, var = x$value)
    findAssignTo(x$parent, var)


# Should this be the .default method?
findAssignTo.Assignment = findAssignTo.Loop = findAssignTo.Return = findAssignTo.If = findAssignTo.Call =
function(x, var = x$value)
    findAssignTo(x$parent, var)


findAssignTo.Parenthesis =
function(x, var = x$value)
{
#    if(length(x$args$contents) > 1)
#        warning("findAssignTo.Parenthesis with more than one element")
    
    findAssignTo(x$parent, var)
}


###############

callType =
    #
    # not just for calls but all language/AST objects.
    #  Trying to determine the return type of a function and that includes
    # return values such as
    #    foo(x)
    #    y
    #
function(call)
    UseMethod("callType")

callType.Literal =
function(call)
{
    tolower(class(call)[1])
}

callType.Assignment =
function(call)
    callType(call$read)

callType.If =
function(call)
   unique(unlist(lapply(unlist(getIfValue(call)), callType)))

callType.Symbol =
function(call)
{
#    if(call$value == ".GlobalEnv")    browser()

   ans = findAssignTo(call)
   if(identical(ans, call)) {
       p = find(call$value)
       if(length(p) > 0)
         return(typeof(get(call$value)))
   }
    
    NA
}

callType.Brace =
function(call)
{
    if(length(call$contents)) 
        callType(call$contents[[ length(call$contents) ]])
    else
        "NULL"
}

callType.Loop = callType.Next = callType.Break =
function(call)
  "NULL"

callType.Return =
function(call)
    callType(call$read)

PrimitiveTypes =
    c("logical", "integer", "numeric", "complex", "character")

callType.Call =
function(call)
{
    if(!is(call$fn, "Symbol"))
        return(NA)
    fun = call$fn$value
    if(fun == "c") {
        types = unique(unlist(lapply(call$args$contents, callType)))
        if(any(is.na(types)))
            return(c("list", "vector"))
        m = match(types, PrimitiveTypes)

        if(!any(is.na(m)))
            return(PrimitiveTypes[max(m)])
    } else if(fun == ".Internal") {
#        browser()
        ifun = call$args[[1]]$fn$value
        return( InternalReturnTypes[[ifun]] )
    } else {

        # Check if the function is a parameer
        ff = asFunction(call)
        if(fun %in% names(ff$params)) {
           warning("analyze locally provided function as parameter ", fun)
           return("<function parameter>")            
        }

        # check to see if this locally defined as a nested function.
        ff = asFunction(call, outer = TRUE)
        def = find_nodes(ff, function(x) is(x, "Assignment") && is_symbol(x$write, fun) && is(x$read, "Function"))
        if(length(def) > 0) {
           warning("analyze locally defined nested function ", fun)
           return("<local function>")
        }

        i = match(fun, names(FunReturnTypes))
        if(is.na(i)) {
            warning("no type information for ", fun)
            return(NA)
        }
        v = FunReturnTypes[[i]]
        if(is(v, "function"))
            v(call)
        else
            v
    }
}


##########################################################################################################
# These are functions used to determine the return type of calls to specific
# functions whose return types depend on the arguments in the specific call
# e.g. grep returns integer or character depending on the argument value = missing/FALSE/TRUE

getVectorCallType =
function(call)
{
    k =  MatchCall(call, vector)
    k$args$mode
}


getVapplyCallType =
function(call)
{
    k =  MatchCall(call, vapply, TRUE)
    v = k$args$contents$FUN.VALUE

    callType(v)
}

getStructureCallType =
function(call)
{
    k = MatchCall(call, structure)
    # We need the arguments from the original call, not k which has no parent information
    # Can't use all.equal
#    i = which(sapply(call$args$contents, `==`, k$args$contents[[1]]))
    v = k$args$contents[[1]]
    callType(v)
}

getSystemCallType =
function(call)
{
    k =  MatchCall(call, system)
    int = k$args$intern
    if(!is.null(int)) {
       browser()
    } else
        "integer"

}


getRepCallType =
function(call)
{
    fun = call$fn$value
    k =  MatchCall(call, get(fun))
    v = k$args$contents[[1]]
    if(is(v, "Literal"))
        tolower(class(v)[1])
    else
        NA
}

getSeqCallType =
function(call)
{
    fun = call$fn$value
    if(fun == ":") {
        w = sapply(call$args$contents, is, "Literal")
        if(w[1])
            return("integer")

        # Have to be careful here. The other argument could be non-integer value.        
        return("numeric")        
    }
    
    #    k = MatchCall(call, get(fun))
    v = call$args$contents[[1]]
#    v = k[[1]]
    if(is(v, "Literal"))
        tolower(class(v)[1])
    else
        NA
}

getAsCallType =
function(call)
{
    k = MatchCall(call, as)
    v = k$args$contents[[2]]$value
}



getDoCallType =
function(call)
{
    k = MatchCall(call, do.call)
    v = k$args$contents[[1]]
    # This is the fn being called. So lookup its return type.
    callType(Call$new(v))
}


getGrepCallType =
function(call)
{
    k =  MatchCall(call, grep)

    value = k$args$contents$value
    if(is.null(value) )
        "integer"
    else if(is(value, "Logical"))
        if(value$value) "character" else "integer"
    else
         c("integer", "character")  # can't tell.

}


getSwitchCallType =
function(call)
{
    # switch is a special so match.call and MatchCall don't work
    #    k =  MatchCall(call, switch)
    # Assume first argument is the value. Has to be the way switch() doesn't do regular argument matching.
    args = call$args$contents[-1]
    types = lapply(args[!sapply(args, is, "EmptyArgument")], callType)
    ty = unique(unlist(types))
#    browser()
    ty
}

#########################################

asFunction =
    # 
    # from is an ASTNode. Get the Function object of which it is a part
    # by walking the parent chain to the Function.
    #
function(from, outer = FALSE) {
    while(!is.null(from) && !is(from$parent, "Function"))
        from = from$parent

    if(!is.null(from)) {
        ans = from$parent
        if(outer && !is.null(ans) ) {
            # this slows things down considerably.  Fix it later.
            tmp = asFunction(ans, TRUE)
            if(!is.null(tmp))
                ans = tmp
        }
        
        ans
    } else
        from
}

if(FALSE) {
    # See ifAnalysis/matchCall.R
MatchCall =
function(call, def, ...)
{
    w = sapply(call$args$contents, is_symbol, "...")
    if(any(w))
        call$args$contents = call$args$contents[!w]
    ans = match_call(call, def, ...)

    # could be multiple matches
    o = sapply(ans$args$contents, function(a) which(sapply(call$args$contents, isEqual, a)))

    if(is.list(o)) {
    browser()        
        idx = integer()
        for(el in o) {
            if(length(el) == 1)
                idx = c(idx, el)
            else {
                idx = c(idx, min(setdiff(el, idx)))
            }
        }
        o = idx
    }

    tmp = structure(call$args$contents[o], names = names(ans$args$contents))
    call$args$contents = tmp
    call
}

isEqual =
function(a, b)
{
    if(is(a, "Null") && is(b, "Null"))
        return(TRUE)

    if(is(a, "Literal") && inherits(a, class(b)) && all(sapply(list(a, b), function(x) is.na(x$value))))
        return(TRUE)    

    if(!identical(class(a), class(b)))
        return(FALSE)
    
    a == b
}
}



getSingleSubsetAssignType =
function(x)
{
#    MatchCall(
#    browser()
    "ANY"
}



getMatchArgType =
function(x)
{

    # MatchCall() to make certain access the correct
    tmp = if(length(x$args$contents) > 1)
               x$args$contents[[2]]
           else {
               f = asFunction(x)
               param = x$args$contents[[1]]$value
               f$params$contents[[param]]$default
           }

    if(is(tmp, "Call"))
        vals = tmp$args$contents
    else
        stop("problem")
    
    unique(tolower(sapply(vals, function(x) class(x)[1])))
}



FunReturnTypes =
    list(paste0 = "character",
         paste = "character",
         gettext = "character",
         ngettext = "character",         
         gettextf = "character",
         as.character = "character",
         as.double = "numeric",
         as.logical = "logical",
         as.integer = "integer",
         as.numeric = "numeric",         
         integer = "integer",
         numeric = "numeric",
         logical = "logical",
         character = "character",
         stop = "ERROR", # was "NULL"
         length = "integer",
         make.names = "character", # verify
         all.equal.character = c("character|logical"),
         all.equal.list = c("character|logical"),
         all.equal.raw = c("character|logical"),         
         all.equal = c("character|logical"),
         all.equal.numeric = c("character|logical"),
         attr.all.equal = "character",     #XXX can this return logical.
         identical = "logical",

         format = "character",

         as.expression = "language",
         as.list = "list",
         sprintf = "character",
         
         loadNamespace = "environment",
         asNamespace = "environment",
         "!" = "logical",
         "&&" = "logical",
         "||" = "logical",
         "&" = "logical",
         "|" = "logical",
         "==" = "logical",
         "!=" = "logical",
         ">" = "logical",
         "<" = "logical",
         ">=" = "logical",
         "<=" = "logical",         
         any = "logical",
         all = "logical",
         lapply = "list",
         vapply = getVapplyCallType,
         "dim<-" = "integer", # actually is the value assigned
         # / depends on types         
         "/" = "numeric",  
         "+" = "numeric",
         "-" = "numeric",
         "*" = "numeric",
         "%/%" = "numeric",
         is.logical = "logical",
         is.numeric = "logical",
         is.character = "logical",
         is.integer = "logical",
         is.factor = "logical",
         is.na = "logical",

         tolower = "character",
         toupper = "character",
         
         vector = getVectorCallType,
         .POSIXlt = "POSIXlt", # or the class?
         .POSIXct = "POSIXct", # or the class?
         as.Date = "Date",
         as.Date.character = "Date",
         Sys.Date = "Date",
         Sys.time = "POSIXct",


         strptime = "POSIXlt",

#         NextMethod = "ANY" #XXX Depends on the generic function
         
         pmatch = "integer",
         match = "integer",
         structure = getStructureCallType,
         .set_row_names = "integer", # but could be numeric.
         "names<-" = "character",
         system = getSystemCallType,
         "substr<-" = "character", #!!
         "substr" = "character",
         "gsub" = "character",
         "sub" = "character",         
         "regmatches" = "character",         
         "unique" = "<input>",  #!!
         "[" = "<input>",
         ".subset" = "<input>",
         ".subset2" = "ANY",
         "[[" = "ANY",
         "$" = "ANY",
         "$<-" = "ANY",
         "@" = "ANY",  #XX Can potentially determine this.
         slot = "ANY", 
         "matrix" = "matrix",
         "readRDS" = "ANY",
         "log" = "numeric",
         "readLines" = "character",
         writeLines= "NULL",
         "do.call" = getDoCallType,
          body = "language",
         system.file = "character",
         baseenv = 'environment',
         parent.frame = 'environment',
         as.environment = 'environment',
         new.env = 'environment',
         emptyenv = 'environment',
         
         rep.int = getRepCallType,
         rep_len = getRepCallType,

         seq_len = getSeqCallType,
         seq.int = getSeqCallType,         
         seq_along = getSeqCallType,
         ":" = getSeqCallType,         

         list = "list",
         split = "list",
         class = "character",
         dQuote = "character",

         sum = "numeric", # XXX or complex
         cumsum = "<input>",

         message = "NULL",

         simpleError = "simpleError",  # Hierarchy
         simpleMessage = "simpleMessage",         


         as.call = "language",

         as.matrix = "matrix",
         as.array = "array",
         array = "array",

         try = "ANY",
         tryCatch = "ANY",

         "Encoding<-" = "character",
         "Encoding" = "character",

#         unlist =  !!

         levels = "character",
         nchar = "integer",
         
         deparse = "character",
         deparse1 = "character",
         as.factor = "factor",
         factor = "factor",
         parse = "language",
         expression = "language",
         
         file = "connection",
         gzfile = "connection",
         bzfile = "connection",
         xzfile = "connection",                  
         stdin = "connection",
         stdout = "connection",
         stderr = "connection",
         gzcon = "connection",         

         names = "character",
         abs = "<input>",

         file.copy = "logical",
         file.path = "character",

         rev = "<input>",

         load = "character",

         cat = "NULL",

         remove = "NULL",
         rm = "NULL",

         kappa = "numeric",
         .kappa_tri = "numeric",
         kappa.qr = "numeric",   # return value is call to to .kapp_tri        

         ceiling = "numeric",
         floor = "numeric",
         
         "class<-" = "character", #!!
         
         getOption = "ANY",  # If we know the name of the option, we can make an educated guess.

         storage.mode = "character",
         

         as.POSIXct = "POSIXct",
         as.POSIXlt = "POSIXlt",
         as.POSIXlt.character = "POSIXlt",                  
         rawToChar = "character",
         charToRaw = "raw",
         raw = "raw",


         duplicated = "logical",

         sys.frame = "environment",
         parent.frame = "environment",

         quote = "language",

         diag = "matrix",
         get = "ANY",
         get0 = "ANY",

         Sys.getenv = "character",
         is.null = "logical",

         data.frame = "data.frame",
         
         as = getAsCallType,
         grep = getGrepCallType,


         sort = "<input>",
         eval = "ANY",
         assign = "ANY", # Type of second argument.

         by = "list", #XX "by" as a class.
         tapply = "list|array|vector", #XXX
         apply = "list|array|vector", #XXX

         setdiff = "<input>",
         intersect = "<input>",         


         "[<-" = getSingleSubsetAssignType,
         

         pmax = "<input>",
         pmin = "<input>",
         unclass = "<input>",

         invisible = "ANY",
         
         
         nrow = "integer",
         attr = "ANY",
         "length<-" = "integer", #!!!

         ordered = "logical",
         order = "integer",

         print = "character",

         max = "<input>",
         min = "<input>",

         mode = "character",

         switch = getSwitchCallType,
         
         setwd = "character", # directory path.

         prettyNum  = "character",
         "attr<-" = "<input>",
         rep = "<input>",

         unloadNamespace = "NULL"         ,

         .bincode = 'integer',
         match.arg = getMatchArgType,
         dyn.load = "list", # class DLLInfo

         NextMethod = "ANY",
         t = "matrix",
         Map = "ANY", # Really determined by the argument f and its return type along with the ...   
         cbind = "matrix|data.frame",
         rbind = "matrix|data.frame",
         as.octmode = "character|integer|octmode",
         .Date = "<input>", # adds a class
         on.exit = "NULL",
         storage.mode = "integer"
         )         


InternalReturnTypes = list(
      getRegisteredNamespace = "environment"
)



if(FALSE) {
    source("ifFuns.R")
    options(nwarnings = 1000)
    if.types = lapply(b3, callType)
    w = names(warnings())
    w2 = grep("no type information for", w, value = TRUE)
    if(length(w2) != length(w))
        grep("no type information for", w, value = TRUE, invert = TRUE)

    funs = unique(gsub("no type information for ", "", w2))
}





