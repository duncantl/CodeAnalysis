#!!! Add edges for as() connections. And also is() via setClassUnion(). Put a type column for the type of edge.
# or hang these edges off the df. as attributes

getAllClasses =
function(classes, classDefs = lapply(classes, getClass))
{
    extra = sapply(classDefs, function(x) names(x@contains)[sapply(x@contains, slot, "distance") == 1])
    extra = c(extra, sapply(classDefs, function(x) names(x@subclasses)))
    classes = unique(c(classes, unlist(extra)))        
}

mkClassHier =
function(pkg, classes = getClasses(pkg), classDefs = structure(lapply(classes, getClass), names = classes))
{
    if(!grepl("^package:", pkg))
        pkg = paste0("package:", pkg)
    
    classes = getAllClasses(classes, classDefs)
    ans = matrix(0, length(classes), length(classes), dimnames = list(classes, classes))

    for(k in classes) {
        def = getClass(k)
        from = names(def@contains)[sapply(def@contains, slot, "distance") == 1]
        w = !(from %in% colnames(ans))
        if(any(w)) {
            ans = cbind(ans, matrix(0, nrow(ans), sum(w), dimnames = list(rownames(ans), from[w])))
        }
        ans[k, from] = 1
    }

    # add as rows too to make square, i.e. all 0s
    m = setdiff(colnames(ans), rownames(ans))
    if(length(m))
        ans = rbind(ans, matrix(0, length(m), ncol(ans), dimnames = list(m, colnames(ans))))
    
    ans
}


mkClassGraph =
function(pkg, addIs = TRUE, classes = getClasses(pkg), classDefs = structure(lapply(classes, getClass), names = classes))
{
    if(!grepl("^package:", pkg))
        pkg = paste0("package:", pkg)
    
    classes = getAllClasses(classes, classDefs)
    o = setdiff(classes, names(classDefs))
    if(length(o))
        classDefs[o] = lapply(o, getClass)
    df = lapply(classes, function(k) {
                      def = classDefs[[k]] # getClass(k)
                      from = names(def@contains)[sapply(def@contains, slot, "distance") == 1]
                      if(length(from) == 0)
                          from = NA
                      data.frame(to = rep(k, length(from)), from = from, stringsAsFactors = FALSE)
                  })
    # what about the classes that are not inherited from or inherit. Omitted so need to add them.
    ans = do.call(rbind, df)
    class(ans) = c("ClassHierarchy", "data.frame")
    
    if(addIs) {
        as = getIsRelationships(pkg, classes)
        ans2 = rbind(ans, as)
        ans2$type = rep(c('inherits', 'as'), c(nrow(ans), nrow(as)))
        class(ans2) = c("CoerceRelationships", class(ans))
        attr(ans2, "definedClasses") = classes
        ans = ans2
    }
    
    ans
}

getIsRelationships =
function(ns, classNames = character())    
{
    if(is.character(ns)) {
        if(grepl("^package:", ns))
            ns = gsub("^package:", "", ns)
        ns = getNamespace(ns)
    }
    
    as = as.list(ns$`.__T__coerce:methods`)
    targets = lapply(as, slot, "target")
    ans = as.data.frame(do.call(rbind, targets), stringsAsFactors = FALSE, row.names = NULL)

    if(length(classNames))
        ans = ans[ans$from %in% classNames | ans$to %in% classNames, ]

    ans
}

#setOldClass(c("ClassHierarchy", "data.frame"))
setOldClass(c("CoerceRelationships", "ClassHierarchy", "data.frame"))

setAs("ClassHierarchy", "igraph",
      function(from) {
          # drop the rows with NAs in either column, but use all of the
          # rows to compute the names of the nodes. This allows for nodes that are unrelated
          # to any other nodes to appear in the graph.
         graph_from_data_frame(from[!is.na(from$from),], TRUE, unique(na.omit(c(from$from, from$to))))
     })

setAs("CoerceRelationships", "igraph",
      function(from) {
          # Create the graph and color the nodes based on whether the
          # class name in from is in the explicitly defined class names.
          # And use lty = 3 if the relationship is not an inheritance, but created by setAs().
          # which is in the type column of the data.frame from.
          nodes = unique(na.omit(c(from$from, from$to)))
          w = !is.na(from$from)
          g = graph_from_data_frame(from[w, c("from", "to")], TRUE, nodes)
          defs = attr(from, "definedClasses")
          E(g)$lty = c(1, 3)[factor(from$type[w])]
          V(g)$color = ifelse(nodes %in% defs, "green", "grey")          
          g
      })


# g = mkClassGraph("package:CodeDepends")
# plot(graph_from_data_frame(g[!is.na(g$from),], TRUE, unique(na.omit(c(g$from, g$to)))))


# gg = graph_from_incidence_matrix(o)


# o = mkClassGraph(, c("Shape", "CenteredShape", "Square", "Circle", "Triangle"))
# gg = graph_from_data_frame(o, directed = TRUE)
