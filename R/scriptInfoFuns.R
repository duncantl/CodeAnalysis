missingScriptVars =
function(script, dir = ".", allScripts = list.files(dir, pattern = "\\.R$", full.names = TRUE),
         varDefs = varDefsByFile(, allScripts),
         unbox = TRUE)
{
    ans = lapply(script,
                 function(file) {
                     gv = getScriptGlobals(file)$variables
                     findMissingVariables(unique(gv), varDefs)
                   })

    if(length(script) == 1 && unbox)
        return(ans[[1]])

    
    names(ans) = script
    ans
}

         

topLevelAssignedVars =
function(x, unique = TRUE)
{
    if(is.character(x)) {
        if(file.exists(x))
            x = parse(x)
        else
            x = parse(text = x)
    }
    
    x = x[sapply(x, isSimpleAssignTo)]
    ans = sapply(x, function(x) deparse(x[[2]]))
    if(unique)
        unique(ans)
    else
        ans
}


findMissingVariables =
function(miss, defs)    
{
   miss = unique(miss)
   structure(lapply(miss, function(v) defs$file[v == defs$variable]), names = miss)
}

varDefsByFile =
function(vars, files = names(vars))
{
    if(missing(vars))
       vars = lapply(files, topLevelAssignedVars)
    
    data.frame(variable = unlist(vars),
               file = rep(files, sapply(vars, length)),
               row.names = NULL
               )
}
