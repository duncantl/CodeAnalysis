
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
        return(code1[idx])
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
function(code, recursive = FALSE)
{
    idx = find_nodes(code, isSourceCall)
# browser()
    if(length(idx)) {
        ans = code$contents
        for(i in rev(idx)) {
            tmp = to_ast(parse(code[[i]]$args[[1]]$value))
            ans = c(ans[seq_len((i[1] - 1))], tmp$contents, ans[-(i[1]:length(ans))])
        }
        code$contents = ans        
    }
    code
}
