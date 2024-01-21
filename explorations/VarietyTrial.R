library(igraph)
library(CodeAnalysis)

dir = "~/Book/ExploreCode/Variety_trial_analysis/code"
ff = list.files(dir, pattern = "\\.R$", full = TRUE)

src = getSourceInfo(dir)
src2 = src[ !is.na(src$sourced), ]
src2$from = gsub(paste0("^", path.expand(dir), "/"), "", src2$from)
g = graph_from_data_frame(src2)
V(g)$color = "salmon"
V(g)$color[ grepl("^(~|/)", V(g)$name) ] = "green"
plot(g)

# plot(g, layout = layout_as_tree)
# plot(g, layout = layout_with_fr)
# plot(g, layout = layout_in_circle)

#
# read_xl_data.R has source("code/funs.R").
#  Should be ../code/funs.R
# We can find this by looking at the files that don't exist
# src$from[ is.na(src$sourced) ]
# src$from[ !file.exists(src$from) ]
#
#
# or if we know this, we can look at 
# src[ !is.na(src$sourced) & src$sourced == "code/funs.R", ]
#
#

# Can see
# prep_stan_data.R ->  prep_col_score.R -> funs.R
# prep_stan_data.R -> funs.R
# and prep_col_score.R is not sourced by any other file.
# So prep_col_score.R doesn't need to source("funs.R")
# unless the order of the commands makes this necessary.
#
# Similarly,
#  prep_stan_data.R -> prep_temp_data.R -> est_PI.R -> funs.R
# and prep_stan_data.R also source()'s funs.R
#
# 3 disconnected sub graphs
#  plot_weather.R -> ~/vt_project/code/funs.R
#  qc_weather.R -> qc_weather_funs.R
#  retrieve_ucipm.R ->  [ getCIMIS.R & uc_ipm_funs.R ]
#
# Do these scripts depend on variables created in other files.

gv = lapply(file.path(dir, c("plot_weather.R", "qc_weather.R", "retreive_ucipm.R")), freeVariables)

v1 = freeVariables("/Users/duncan/Book/ExploreCode/Variety_trial_analysis/code/plot_weather.R")
# so
#  plot_weather: "site_weather"
#  qc_weather : "all_weather" "all_info"   
#  retreive_ucipm.R: "xmlValue"
#     And freeVariables doesn't know that xmlValue is availale via the library(XML) call
#     Also doesn't know about xpathSApply() as being like lapply() and calling its FUN argument.



###############

inp = lapply(ff, getInputFiles, FALSE)
names(inp) = basename(ff)

out = lapply(ff, getOutputFiles, FALSE)
names(out) = basename(ff)

grout = lapply(ff, getGraphicsOutputFiles, FALSE)
names(grout) = basename(ff)

a = data.frame(from = rep(names(inp), sapply(inp, length)), to = unlist(inp))
b = data.frame(from = rep(names(out), sapply(out, length)), to = unlist(out))
c = data.frame(from = rep(names(grout), sapply(grout, length)), to = unlist(grout))


all = rbind(a, b, c)
all = all[ !is.na(all[,2]),]

nels = sapply(list(a, b, c), function(x) sum(!is.na(x$to)))

all$type = rep(c("read", "write", "grwrite"), nels)

g = graph_from_data_frame(all)

# basename(ff)

# From brewer.pal(4, "PiYG") and changed order.
colors = c(R = "#4DAC26", input =  "#F1B6DA", output =  "#B8E186" , graphicsOutput = "#D01C8B")
V(g)$color[ V(g)$name %in% all$from ] = colors[ "R" ]
V(g)$color[ V(g)$name %in% a$to ] = colors[ "input" ]
V(g)$color[ V(g)$name %in% b$to ] = colors[ "output" ]
V(g)$color[ V(g)$name %in% c$to ] = colors[ "graphicsOutput" ]

V(g)$name[ is.na(V(g)$color) ]
# data.frame(V(g)$name, V(g)$color)


