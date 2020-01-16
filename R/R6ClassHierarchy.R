
getR6ClassInherits =
function(pkg, ns = getNamespace(pkg))
{
    ty = sapply(ns, class)
    w = (ty == "R6ClassGenerator")

    inh = lapply(names(ty)[w],
                    function(id)
                       c(class = id, parentClass = as.character(ns[[id]]$inherit)))

    w2 = sapply(inh, function(x) length(unique(x))) > 1

    structure(do.call(rbind, inh[w2]), class = c("R6ClassInherits", "matrix"))
}

plot.R6ClassInherits =
function(x, y, ...)
   plot(igraph::graph_from_edgelist(x[,2:1]), ...)


#setOldClass(c("R6ClassInherits", "matrix"))
edgesToAdjacencyMatrix =
function(m)
{
    ak = unique(as.character(m))
    adj = matrix(0, length(ak), length(ak), dimnames = list(ak, ak))
    adj[m] = 1
    adj
}
#setAs("R6ClassInherits", "AdjacencyMatrix")

####################################


getDirectSubclasses =
    # 
    #  Given an R6 class name (root) and a R6ClassInherits object, get the classes that
    #  inherit from root, i.e. have root as the base class.
    #
function(root, edges)
{
   edges[ edges[, 2] == root, 1]   
}


##########################
# Drawing in tikz.

tikz =
function(root, edges, level = 0L, indent = paste(rep(" ", (level + 1)*3), collapse = ""))
{
    sub = getDirectSubclasses(root, edges)

    tmp = lapply(sub, tikz, edges = edges, level = level + 1L, indent = paste0("   ", indent))
    tmp = tmp[sapply(tmp, length) > 0]
#    if(root == "ConditionalBranch") browser()
    kids = if(length(tmp))
#              sprintf("%schild {\n %s \n%s}", indent, paste(unlist(tmp), collapse = paste0("\n", indent)),  indent)
              sprintf("%schild {\n %s \n%s}", indent, sapply(tmp, paste, collapse = "\n"),  indent)
    else
              kids = character()
    n = sprintf("%s%snode (%s) {%s}", indent, if(level == 0) "\\" else "",  root, root)
    c(n, kids)
}




tikzHier =
function(root, edges, level = 0L, indent = paste(rep(" ", (level + 1)), collapse = ""))
{
    sub = getDirectSubclasses(root, edges)
    tmp = lapply(sub, tikzHier, edges = edges, level = level + 1L, indent = paste0(" ", indent))
    tmp = tmp[sapply(tmp, length) > 0]
    c(sprintf("%s\\coordinate (%s) at (,);", indent, root), tmp)
}
