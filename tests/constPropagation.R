library(codetools)
library(CodeAnalysis)

if(FALSE) {

ef = parse("~/GitWorkingArea/CodeAnalysisWORstatic/explorations/constProp.R")

chVar =
function(x, w, map, ...)
{
    if(is.name(x)) {
        v = as.character(x)
        if(v %in% names(map) && !inherits(map[[v]], "Invalid"))
            return(map[[v]])
    }
    
    x	   
}

w = mkModifyCodeWalker(chVar, FALSE)

o = walkCode(ef$f1, w)
}
