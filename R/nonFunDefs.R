getNonFunDefs =
function(file, code = parse(file), ignoreIfFalse = TRUE)
{
    w = !sapply(code, isFunAssign)
    ans = code[w]
    if(ignoreIfFalse)
        as = ans[!sapply(ans, isIfFalse)]
    
    ans
}


isIfFalse =
function(x)
    isCallTo(x, "if") && isFALSE(x[[2]])
