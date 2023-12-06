library(igraph)
library(CodeAnalysis)

dir = "~/Book/ExploreCode/Variety_trial_analysis/code"
ff = list.files(dir, pattern = "\\.R$", full = TRUE)

src = getSourceInfo(dir)
src2 = src[ !is.na(src$sourced), ]
src2$from = gsub(paste0("^", path.expand(dir), "/"), "", src2$from)
g = graph_from_data_frame(src2)
V(g)$color[ grepl("^(~|/)", V(g)$name) ] = "blue"
plot(g)

plot(g, layout = layout_as_tree)
plot(g, layout = layout_with_fr)
plot(g, layout = layout_in_circle)

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


