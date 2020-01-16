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
function(pkg, classes = getClasses(pkg), classDefs = lapply(classes, getClass))
{
    classes = getAllClasses(classes, classDefs)
    ans = matrix(0, length(classes), length(classes), dimnames = list(classes, classes))
    for(k in classes) {
        def = getClass(k)
        from = names(def@contains)[sapply(def@contains, slot, "distance") == 1]
        ans[k, from] = 1
    }
    ans
}


mkClassGraph =
function(pkg, classes = getClasses(pkg), classDefs = structure(lapply(classes, getClass), names = classes))
{
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
    ans
}

# g = mkClassGraph("package:CodeDepends")
# plot(graph_from_data_frame(g[!is.na(g$from),], TRUE, unique(na.omit(c(g$from, g$to)))))


# gg = graph_from_incidence_matrix(o)


# o = mkClassGraph(, c("Shape", "CenteredShape", "Square", "Circle", "Triangle"))
# gg = graph_from_data_frame(o, directed = TRUE)
