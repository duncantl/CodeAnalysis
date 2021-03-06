
isReadFun =
function(x, readFuns)
    (is(x, "Call") && x$fn$ssa_name %in% readFuns) ||
       is(x, "Symbol") && x$value %in% readFuns && !(is(x$parent, "Call") && identical(x$parent$fn, x))

findReadDataCalls =
    #
    # Need to handle
    #  f = read.csv
    #  f(file)
    #
    #
function(code, readFuns = getReadFunNames(), recursive = TRUE)
{
    code1 = to_ast(code)

    code1 = substSource(code1)

    if(is(code1, "Function")) {
        # Find all the calls or references to anything in readFuns
        idx = find_nodes(code1, isReadFun, readFuns)
        #return(code1[idx])
        return(lapply(idx, function(i) code1[[i]]))
    } else 
        # look only at the top-level expressions in the script that do some calculation and  not the Function defintions.
       exprs = getTopLevelCalls(code1)

# browser()    
    # need to worry about the order here as the alias can come into effect after being called
    w = sapply(exprs, isAlias)
    aliases = findAliases(exprs[w])
    exprs = exprs[!w]

    w = aliases %in% readFuns
    if(any(w))
        readFuns = c(readFuns, names(aliases)[w])

    if(recursive) {
         # So look at functions defined in the script.
         # We automatically go down top-level expressions except function definitions.
        funs = getDefinedFuns(code)
        w = sapply(funs, function(f)
                             findReadDataCalls(f, readFuns))
        w2 = sapply(w, length) > 0
        readFuns = c(readFuns, names(funs)[w2])
    }


  ans =  lapply(exprs, function(e) {
                           idx = find_nodes(e, isReadFun, readFuns)
                            ans = list()
                            if(isReadFun(e, readFuns))
                               ans[[1]] = e
      
                            if(length(idx))
                               append(ans, lapply(idx, function(i) e[[i]]))
                            else
                               ans
                       })

    ans = unlist(ans, recursive = FALSE)
    ans = unique(lapply(ans, function(x) if(is(x, "Symbol")) {p = x$parent; if(is(p, "ArgumentList")) p = p$parent; p} else x))
    return(ans)
    
if(FALSE) {    
    els = lapply(idx, function(i) code1[[i]])
    w = sapply(els, isReadDataCall, readFuns)
    els[w]
}    
    
#    w = sapply(code1[idx], isReadDataCall)
#    code1[idx][w]
}


getTopLevelCalls =
    # exclude function definitions
function(code)
{
    k = children(code)
    w = sapply(k, function(x) !(is(x, "Assign") && is(x$read, "Function")))
    k[w]
}
    

isReadDataCall =
    #
    #
    #
function(call, readFuns = getReadFunNames())
{
  call$fn$ssa_name %in% readFuns
}




getReadFunNames =
function(..., .names = ReadFunNames)
{
  c(unlist(list(...)), .names)
}

ReadFunNames = c(
    "readLines",
    "scan",
    "read.csv",
    "read.csv2",    
    "read.table",
    "read.delim",
    "read.delim2",
    "read.fwf",
    "count.fields"
    )


getDefinedFuns =
function(code)
{
    isFun = find_nodes(code, function(x) is(x, "Assign") && is(x$read, "Function"))
    els = lapply(isFun, function(i) code[[i]])
    structure(lapply(els, function(x) x$read), names = sapply(els, function(x) x$write$ssa_name))
#    structure(lapply(code[isFun], function(x) x$read), names = sapply(code[isFun], function(x) x$write$ssa_name))
}


findAliases =
function(code, ...)
    UseMethod("findAliases")

findAliases.list =
function(code, ...)
{
    ans = lapply(code, findAliases, ...)
    unlist(ans)
}

isAlias = function(x) is(x, "Assign") && is(x$read, "Symbol")

findAliases.ASTNode =
function(code, ...)    
{
     # have to test code itself since find_nodes doesn't apply the test to the top node.

    if(!isAlias(code)) {
      isFun = find_nodes(code, isAlias)
      els = lapply(isFun, function(i) code[[i]])
  } else
      els = list(code)
    
    structure(sapply(els, function(x) x$read$value), names = sapply(els, function(x) x$write$ssa_name))
#    structure(lapply(code[isFun], function(x) x$read), names = sapply(code[isFun], function(x) x$write$ssa_name))
}



isSourceCall =
function(x)
      # make more flexible / sophisticated
 is(x, "Call") && x$fn$value == "source"  && (length(x$args) > 0 && is(x$args[[1]], "Character"))

substSource =
    #
    # This modifies code in place.!
    #
function(code, recursive = FALSE, dir = ".")
{
    idx = find_nodes(code, isSourceCall)
    # These better all be top-level AST nodes at this point, not nested within top-level expressions.

    if(length(idx)) {
        # Replace each of these with the parsed contents of the source() call.
        ans = code$contents
        for(i in rev(idx)) {
            file = code[[i]]$args[[1]]$value
            if(!file.exists(file) && file.exists(file.path(dir, file)))
                file = file.path(dir, file)

           if(!file.exists(file)) {
                warning(file, " to be source()'d doesn't exist")
                next
           }
            
           tmp = to_ast(parse(file))
           if(recursive)
               tmp = substSource(tmp, recursive, dirname(file))
           if(is(tmp, "Brace"))
               tmp = tmp$contents

           if(i == length(ans))
               ans = c(ans[-i], tmp)
           else
               ans = c(ans[seq_len((i[1] - 1))], tmp, ans[( (i[1]+1):length(ans))])
        }
        code$contents = ans        
    }
    code
}


getReadFunName =
    #
    # Resolves aliases
    #  f = read.csv
    #  f()
    #
function(expr, aliases = getAliases(expr))
{
    nm = expr$fn$value
    if(length(aliases) && !is.na(m <- match(nm, names(aliases))))
        aliases[m]
    else
        nm
}

getAliases =
function(expr, root = getRoot(expr))
{
    idx = find_nodes(root, function(x) is(x, "Assign") && is(x$read, "Symbol") && is(x$write, "Symbol"))
    if(length(idx))
        structure( sapply(idx, function(i) root[[i]]$read$value), names =  sapply(idx, function(i) root[[i]]$write$value))
    else
        character()
}


getRoot =
function(x)
{
    while(!is.null(x$parent))
        x = x$parent
    
    x
}
