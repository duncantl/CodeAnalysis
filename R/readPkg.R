
loadCode =
function(dir, env = new.env(), files = list.files(dir, pattern = "\\.[RrsSqQ]$", full.names = TRUE))
{
    env$.functionFileMap = data.frame(funcName = character(), file = character())
    lapply(files, function(f) {
                     cur = ls(env, all.names = TRUE)
                     source(f, env)
                     new = setdiff(ls(env, all.names = TRUE), cur)
                     env$.functionFileMap = rbind(env$.functionFileMap, data.frame(funcName = new, file  = rep(f, length(new))))
                  })
    env
}
