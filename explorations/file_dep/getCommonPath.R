getCommonPath =
    #
    # getCommonPath(c("~/Data/A", "~/Data/B"))
    ##
    # getCommonPath( c("~/vt_project/code/funs.R", "~/vt_project/code/calc_weather_funs.R", 
"~/vt_project/data/vt1995_2015_clean.Rda", "~/vt_project/data/weather_stn_info.Rda", 
"~/vt_project/data/all_weather_filled.Rda", "~/vt_project/vt_database/site_characteristics.csv", 
"~/vt_project/data/site_weather.rda", "~/vt_project/data/all_vt_weather.Rda"))
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

rewritePaths =
function(files, commonPaths = getCommonPath(files))
{
    ans = list()
    if(length(commonPaths) > 1) 
        ans$baseDir = commonPaths[[1]]


    for(i in files[-1]) {
        sprintf("file.path(%s, '%s')", )
    }
}


replacePath =
function(expr, paths, ast = to_ast(expr))
{
    astTravers(ast, changePathFun(paths))
    to_r(ast)
}

changePathFuns =
function(paths)
{
    function(node) {
        if(is(node, "Character"))  {
            sapply(paths$dir, function(x) grep)
        }
    }
}
