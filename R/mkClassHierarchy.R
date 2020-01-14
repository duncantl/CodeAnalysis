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
function(pkg, classes = getClasses(pkg), classDefs = lapply(classes, getClass))
{
    classes = getAllClasses(classes, classDefs)    
    df = lapply(classes, function(k) {
                      def = getClass(k)
                      from = names(def@contains)[sapply(def@contains, slot, "distance") == 1]
                      data.frame(to = rep(k, length(from)), from = from, stringsAsFactors = FALSE)
                    })
    do.call(rbind, df)
}


# gg = graph_from_incidence_matrix(o)


# o = mkClassGraph(, c("Shape", "CenteredShape", "Square", "Circle", "Triangle"))
# gg = graph_from_data_frame(o, directed = TRUE)
