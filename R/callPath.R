if(FALSE) {

    gr0 =  c("A", "B",
             "B", "C",
             "C", "fn",
             "X", "Y",
             "Y", "fn"
             )
    gr1 = as.data.frame(matrix(gr0, , 2, byrow = TRUE))
    getCallPaths("fn", gr1)

    gr2 = c(gr0, "D", "C")
    gr3 = as.data.frame(matrix(gr2, , 2, byrow = TRUE))
    getCallPaths("fn", gr3)

    gr4 = c(gr2, "F", "B", "G", "F", "H", "G")
    gr5 = as.data.frame(matrix(gr4, , 2, byrow = TRUE))
    getCallPaths("fn", gr5)        
}

getCallPaths =
    #
    #  for a given function name, get all paths/sequences of function calls that
    #  invoke this function, based on the direct call graph in `calls`.
    #
    #
    #  fun - name of the function for which we want to find the path of calls to this.
    #  callGradph - a 2 column data.frame from callGraph(, asDF = TRUE)
    #   column 1 name of caller, column 2 name of called
    #
    #  If asString = TRUE, we return the string for each path from called to callers
    #  If  FALSE, then we return a character vector for each path and reverse the order
    #   so it is in the order top-down.
    #
    #
function(fun, callGraph, recursive = TRUE, cur = "", depth = 1, asString = FALSE, map = NULL,
         recordTypes = if(!is.null(map)) map$name[map$type == "recordType"] else character())
{
    w = (callGraph[,2] == fun)

    tmp = paste(cur, fun, sep = ";")
    if(!any(w)) 
        return(if(depth == 1) gsub(";", "", tmp) else tmp)

    if(!recursive)
        return(callGraph[w,1])

    ans = sapply(callGraph[w, 1], function(x) getCallPaths(x, callGraph, recursive, tmp, depth = depth + 1))

    if(depth == 1) {
        ans2 = gsub("^;", "", unlist(ans))
        if(length(recordTypes)) {
            rx = paste0(";(", paste(recordTypes, collapse = "|"), ");")
#            browser()
            ans2 = ans2[ !grepl(rx, ans2) ]
        }
        if(asString)
            ans2
        else
            lapply(strsplit(ans2, ";"), rev)
    } else
        ans
}
