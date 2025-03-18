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



###############


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




