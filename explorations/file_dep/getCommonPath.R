getCommonPath =
    #
    # getCommonPath(c("~/Data/A", "~/Data/B"))
    ##
    # getCommonPath( c("~/vt_project/code/funs.R", "~/vt_project/code/calc_weather_funs.R", 
#"~/vt_project/data/vt1995_2015_clean.Rda", "~/vt_project/data/weather_stn_info.Rda", 
#"~/vt_project/data/all_weather_filled.Rda", "~/vt_project/vt_database/site_characteristics.csv", 
#"~/vt_project/data/site_weather.rda", "~/vt_project/data/all_vt_weather.Rda"))
    #
function(files, dirs = dirname(files))
{
    u = unique(dirs)
    els = list()
    while(length(u) > 1) {
        els[[length(els)+1]] = u
        u = unique(dirname(u))
    }
    els[[length(els)+1]] = u

    rev(els)
}

defPathVars =
    #
    # Want a data frame with the name of the variable, the directory
    # 
    #
function(files, commonPaths = getCommonPath(files))
{
    ans = list()
    ans = data.frame(var = "baseDir",
                     path = commonPaths[[1]],
                     subdir = "",
                     rvar = "baseDir",
                     stringsAsFactors = FALSE)

    for(i in commonPaths[-1]) {
        ans = rbind(ans,
                    data.frame(var = sprintf("%sDir", basename(i)),
                               path = i,
                               subdir = basename(i), 
                               rvar = sprintf("file.path(baseDir, '%s')", basename(i)),
                               stringsAsFactors = FALSE))
    }
    ans
}


replacePath =
function(expr, paths, ast = to_ast(expr))
{
    astTraverse(ast, changePathFun(paths))
    to_r(ast)
}

changePathFun =
function(paths)
{
    paths = paths[order(nchar(paths$path), decreasing = TRUE), ]
    function(node) {
        if(is(node, "Character"))  {
#            browser()
            f = path.expand(node$value)
            w = sapply(paths$path, grepl, f)
            if(any(w)) {
               w = which(w)[1]
               e = Call$new("file.path", args = list(Symbol$new(paths$var[w]),
                                             Character$new(basename(f))))
               browser()
               i = which(sapply(node$parent$args, identical, node))
               node$parent$args[[i]] = e
               # replaceNode(node$parent, node, e)
            }
        }

        TRUE
    }
}
