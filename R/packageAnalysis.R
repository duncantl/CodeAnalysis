funLength =
    #
    # This currently just computes the number of top-level expressions
    # See numTerms for the complete breakdown.
    #
    #
    #
function(f)
{
    b = body(f)
    if(is(b, "call"))
        1L
    else
        length(b) - 1L # for the {
}


astnumTerms = numTerms =
    #
    # The number of nodes in the AST.
    #
    #
function(x)
{
  length(rstatic::find_nodes(rstatic::to_ast(x), function(x) TRUE))
}


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
            walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
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
 ##                           if(is(x, "if")) browser()
              for (ee in as.list(x))
                  if (!missing(ee))
                      walkCode(ee, w)
    }
    list(handler = function(x, w) NULL ,
         call = call,
         leaf = leaf,
         .result = function() ctr,
         .els = function()els)
}

numTerms =
function(x, ctr = mkCounter(skipIfFalse = skipIfFalse), skipIfFalse = TRUE)
{
#    if(is.function(x)) {
#        walkCode()
#    }
    walkCode(x, ctr)
    ctr$.result()
}


findS3ClassDefs =
function(x, ...)
   UseMethod("findS3ClassDefs")

findS3ClassDefs.character =
function(x, ...)
    findS3ClassDefs(parse(x), ...) # deal with vector, directory.

findS3ClassDefs.expression = findS3ClassDefs.function =
function(x, ...)
    findS3ClassDefs(to_ast(x), ...)


findS3ClassDefs.ASTNode =
function(x, asNodes = FALSE, ...)
{
    nodes =  find_nodes(x, isS3ClassSetNode)
    if(asNodes)
        return(nodes)

    sapply(nodes, getS3ClassInfo)
}

getS3ClassInfo =
function(x)
{
    switch(x$fn$value,
           "class<-" = paramValue(x$args$contents$value),
           "structure" = paramValue(x$args$contents$class))
}


isS3ClassSetNode = 
function(x)
{
    # Add attr(, "class") = ...
   (is(x, "Replacement") && is(x$read, "Call") && is_symbol(x$read$fn, "class<-")) ||
      is(x, "Call") && is_symbol(x$fn, "structure") && "class" %in% names(x$args$contents)
}
#  (is(x, "Replacement") && (is_symbol(x$fn, "class<-"))) ||




countNestedFunctionLevel =
    #
    # Given a Function object, this walks up the AST 
    #
    #
function(f, count = 0)
{
    f2 = asFunction(f$parent)
    if(!is.null(f2))
        return(countNestedFunctionLevel(f2, count + 1))

    count

}

if(FALSE) {
  p = to_ast(parse("~/R-devel3/src/library/tools/R/QC.R"))
  s3 = findS3ClassDefs(p)

  inh = find_nodes(p, function(x) is(x, "Call" ) && is_symbol(x$fn, "inherits"))
}

if(FALSE) {
xmlFiles = getRFiles("~/GitWorkingArea/XML/R")
asts = lapply(xmlFiles, function(f) to_ast(parse(f))); names(asts) = basename(xmlFiles)

xml.s3 = lapply(asts, findS3ClassDefs)
xml.s3 = xml.s3[sapply(xml.s3, length) != 0]

xml.inh = lapply(asts, function(p) find_nodes(p, function(x) is(x, "Call" ) && (is_symbol(x$fn, "inherits") || is_symbol(x$fn, "is"))))
names(xml.inh) = names(asts)
xml.inh = xml.inh[sapply(xml.inh, length) > 0]
}





nonLocalAssigns =
    #  nla = nonLocalAssigns(tools:::.check_packages, varName = TRUE)
    # Note how we make this function more flexible by adding ...
    # and passing that to w$ans(...)
    # And that passes it to assignName.
    # This allows the caller to control deparse, varName or default for assignName
    # Also note how we moved the code in assignName into its own function
    # from an anonymous function in the structure( names = sapply(, function))
    # so it could be reused and called independently by the user.
    # Then we made it more intelligent, adding the options
    # and also making it take different types of language objects.
    #
function(code, w = mkNLAWalker(skipIfFalse = skipIfFalse), ..., skipIfFalse = TRUE)
{
    codetools::walkCode(code, w)
    w$ans(...)
}

mkNLAWalker =
    # Can we use findCallsTo(...,  "<<-")
function(skipIfFalse = TRUE)
{
    ans = list()
    
    leaf = function(x, w) {
        ty = typeof(x)
        if(ty %in% c("pairlist", "list", "language", "expression")) {
            lapply(x, walkCode, w)
            return(NULL)
        } else if(ty == "closure") {
            walkCode(formals(x), w) # lapply(formals(x), walkCode, w)
            walkCode(body(x), w)
        }

        NULL
    }

    call = function(x, w) {

        if(skipIfFalse && skipIfFalse(x, w))
            return(NULL)        
        
        if(is.name(x[[1]]) && as.character(x[[1]])== "<<-")
            ans[[ length(ans) + 1L]] <<- x
        
        for (ee in as.list(x))
            if (!missing(ee))
                walkCode(ee, w)
    }

    list(handler = function(...) NULL,
         call = call,
         leaf = leaf,
         ans = function(...) {
                 structure(ans, names = sapply(ans, assignName, ...))
             })
}

assignName =
function(x, deparse = FALSE, varName = FALSE)
{
    if(is.name(x[[2]]))
       as.character(x[[2]])
    else {
        if(varName)
            switch(class(x),
                   "<-"=,
                   "=" = assignName(x[[2]], varName = TRUE),
                   call = switch(as.character(x[[1]]),
                                  "["=,
                                  "[["=,
                                  "<<-"=,                   
                                 "$" = assignName(x[[2]], varName = TRUE),
                       deparse(x[[2]])),
                   "??"
                   )
        else if(deparse)
            paste(deparse(x), collapse = "")
        else
            ""
        }
}

if(FALSE)  {
findOS =
function(code, w = mkOSWalker(), ...)
{
    walkCode(code, w)
    w$.results(...)
}

mkOSWalker =
function()
{
    list(leaf,
         call,
         handler = function(x, e) NULL)
}
}






################################################

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
   type %in% c("logical", "integer", "numeric", "character", "complex", "double", "NULL")
}

findLiterals =
function(code, walker = mkLiteralCollector(ignoreParams, skipIfFalse = skipIfFalse, ...),
         ignoreParams = TRUE, skipIfFalse = TRUE, ...)
{
    walkCode(code, walker)
    walker$.values()
}
