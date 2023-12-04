getSourceInfo =
    # Handles cases where the file being sourced doesn't exist
    # so we don't attempt to recursively determine what it source()s
    # When recursive = TRUE, files that are sourced but don't exist
    # will have a row in the result, but an NA in the sourced column for that row.
    # If recursive is true, a file A will have <n> rows in the result
    # where <n> is the number of files that the file A source()s.
    # This will be 0 if the file A does not source() any other files.
    #
function(x, recursive = TRUE, ...)    
{
    ans = getSourceInfox(x)

    if(recursive) {
        done = unique(ans$from)
        while(TRUE) {
            # Avoid dealing with the files that don't exist and processing
            # the in subsequent iterations, i.e., don't consider
            # sourced files with from == NA that we may have added in previous
            # iterations.
            ok = !is.na(ans$sourced)
            xtra = unique(getRelativeFiles(ans$sourced[ok], ans$from[ok]))
            ex = file.exists(xtra)
            w = !(xtra %in% done) & ex

            if(any(!ex))
                ans = rbind(ans, data.frame(from = xtra[!ex], sourced = rep(NA, sum(!ex))))
            
            if(any(w)) {
                new = getSourceInfox(xtra[w])
                if(nrow(new) == 0)
                    break
                
                ans = rbind(ans, new)
                done = unique(c(done, new$from[!is.na(new$sourced)]))
            }
        }
    }
    
    ans    
}

getRelativeFiles =
    #
    # Is there a function in R that does this? e.g., file.path
    #  No for file.path - file.path("A/B", "~/foo.R")
    #
    # for each of element of a character vector of `files` that were
    # processed relative each element of `rel`
    # compute the relative path of file[i] relative to rel[i]
    #
    # e.g., files = foo/bar.R and rel = ../A/B/C/abc.R
    # The result should be ../A/B/C/foo/bar.R (check)
    #
    # But  files = ~/foo/bar.R and rel = ../A/B/C/abc.R
    # should be ~/foo/bar.R or path.expand.
    #
    # getRelativeFiles("~/foo.R", "A/B/foo.R")
    # 
function(files, rel)
{
    dir = dirname(rel)
    w = grepl("^(~|/)", files)
    ans = file.path(dir, files)

    if(any(w))
        ans[w] = files
    ans
}

relativeFile =
    # Seems to be an earlier and similar version but not vectorized.
    # Called updateLoad() and insertSource().
function(name, base)
{
    # temporary
    name =  path.expand(name)
    if(grepl("^/", name))
        return(name)

    file.path( dirname(base), name)
}


# Call this getSourceInfox rather than getSourceInfo() so that
# we can use the latter to call this and then do the recursive
# step.


getSourceInfox =
function(x, recursive = TRUE, ...)    
  UseMethod("getSourceInfox")


getSourceInfox.character =
function(x, ...)    
{
    if(length(x) > 1)
        return(do.call(rbind, lapply(x, getSourceInfox)))

    if(!file.exists(x))
        getSourceInfox(parse(text = x), filename = NA)
    else if(file.info(x)$isdir)
        getSourceInfox(getRFiles(x))
    else
        getSourceInfox(parse(x), filename = x)
}

getSourceInfox.expression =
function(x, filename, ...)    
{
    #    w = sapply(x, isSourceCall)
    # Now, can find source() in subexpressions, including if(FALSE)
    k = findCallsTo(x, "source")
    if(length(k))
        ans = cbind(rep(filename, length(k)), sapply(k, getCallParam, 1L))
    else
        ans = matrix(NA, 0, 2)

    colnames(ans) = c("from", "sourced")
    as.data.frame(ans)
}
