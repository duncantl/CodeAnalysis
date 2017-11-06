getAssigns =
function(file, e = parse(file))
{
  sapply(e, isSimpleAssign)  
}

isSimpleAssign =
function(x) 
{
  is.call(x) && x[[1]] %in% c("<-", "=") && is.name(x[[2]])
}

findFuns =
function()    
{

}
