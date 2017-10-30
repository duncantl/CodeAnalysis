# What are we trying to do: Find files read and written by scripts

## Motivation

If you are inhereting a project from someone else, e.g. former lab
member, and you need to understand what the minimal set of input data
are. Or if you are trying to package your analysis for publication,
and need to include only relevant CSV or Rda/Rds files. Rather than
manually collecting the items, we would like to run over a collection
of scripts, and return a table that looks like:

Script | input needed | output created
------|--------|------------
bar.R | foo.csv | bar-out.rda

This will facilitate:

1. Understanding a new project: Learn quickly what script uses and
   creates other files.
   
1. Packaging/cleaning: Remove items that are not used/critical to the
   final product.
   
## General approach

Identify which functions are doing the reading (read.csv, load,
readRDS, ect.) and writting (save, write.csv, etc.). Then use
CodeDepends to parse the arguments to each to find the files
referenced. We might need to traverse backwards to find the file name
in cases like:

f = "/some/file/foo.csv"
read.csv(f)

## Bonus

Create a dependency graph, which can be plotted/visualized. Identify
orphaned files "automatically", e.g. scripts that produce files that are not used
elsewhere, files that are never used.

## Code

library(RCleanProject)
library(CodeDepends)

base_dir = "file_dep"

scripts = list.files(base_dir, pattern = "\\.[Rr]$", full.name = TRUE)


sscripts = lapply(scripts, readScript)
info = lapply(sscripts, as, "ScriptInfo")
names(info) = scripts

tmp = lapply(info, function(x) getDepends(,x))

all.dep = do.call(rbind, tmp)
all.dep$SourceFilename = rep(names(info), sapply(tmp, nrow))
rownames(all.dep) = NULL # seq(1, nrow(all.dep))

loaded = basename(all.dep[all.dep$operation %in% c("read.csv", "load"), "filename"])
written = basename(all.dep[all.dep$operation %in% c("write.csv", "save"), "filename"])
data_files = list.files(base_dir, pattern = "\\.csv$|\\.rda$")

# The ones that both written and read
intersect(written, loaded)

# Not read
setdiff(data_files, loaded)

# Not read or written
setdiff(data_files, c(written, loaded))

