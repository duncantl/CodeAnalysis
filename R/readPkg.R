
loadCode =
function(dir, env = new.env(), files = list.files(dir, pattern = "\\.[RrsSqQ]$", full = TRUE))
{
    env$.functionFileMap = data.frame(funcName = character(), file = character())
    lapply(files, function(f) {
                     cur = ls(env, all = TRUE)
                     source(f, env)
                     new = setdiff(ls(env, all = TRUE), cur)
                     env$.functionFileMap = rbind(env$.functionFileMap, data.frame(funcName = new, file  = rep(f, length(new))))
                  })
    env
}
