# Table of contents of the files in a directory.
# 2 versions -
#   static analysis but currently just of the functions.
#   source() so not static analysis.

# See utils::rtags for something similar.

toc =
    # XXX Consider getting non-function symbols.
function(dir = ".", files = list.files(dir, pattern = pattern, full.names = TRUE), pattern = "\\.[RrsSq]")
{
    byFile = lapply(files, getFunctionDefs)
    data.frame(functions = unlist(lapply(byFile, names)),
               file = rep(files, sapply(byFile, length)))
}

dynToc =
    #
    # This evaluates the code via source().
    #
function(dir = ".", files = list.files(dir, pattern = pattern, full.names = TRUE), pattern = "\\.[RrsSq]")    
{
    e = new.env()
    lapply(files, source, e)
    tmp = as.list.environment(e, all = TRUE)
    data.frame(name = names(tmp), class = sapply(tmp, class), size = sapply(tmp, object.size))
}
