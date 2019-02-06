# Getting Information About Project Structure

Projects tend to build up a lot of files/functions over time.  Even
well-documented projects can be difficult to understand and navigate
function and file dependencies and relationships.  
This is especially the case in long-running projects or projects
worked on by many people.

We would like to be able to,

1. Identify input/output data files (see CodeDepends).
    * Useful to typical useRs.
2. Identify function dependencies, entry points, and wrapper functions
3. Summarize code from a project (see RCleanProject).  Find minimal
   example: create graph of scripts for directory?
    * Richer example of what can be done by tool developers.
4. Aids in creation of build files, e.g. GNU make

## Find files read and written by scripts

### Motivation

As a project evolves, we often have a directory with a collection of R
script files, functions in other files, and data in CSV, RDS, and/or
RDA files.  At some point, we need to make these available to other
people.  These people might be reviewers or readers of a paper.  They
might also be collaborators who are helping to debug some code or who
just want to explore the computations.  Or we simply may want to
commit the relevant files to a version control system.  In these
cases, we don't want to include every file in the directory as there
are often unneeded files that merely confuse others (and ourselves).
Instead, we want to be able to provide the minimal set of files these
people need to run the computations.  In some cases, we may even want
to provide the minimal set of files for a subset of the computations.

If you are inhereting a project from someone else, e.g. former lab
member, and you need to understand what the minimal set of input data
are. Or if you are trying to package your analysis for publication,
and need to include only relevant CSV or Rda/Rds files. Rather than
manually collecting the items, we would like to run over a collection
of scripts, and return a table that looks like:

Often, we will send the code to somebody and omit one or more critical
files.  The recipient runs the code and gets an error, has to figure
out what the problem is and then ask us to send these separately. This
is frustrating, takes time and slows the entire process.

So we would like a mechanism that analyzes the different code files
and identifies, where possible, which files are needed as inputs and
provide a list of these input files.  Summarizing these dependencies
in a table might look like


Script | input needed | output created ------|--------|------------
bar.R | foo.csv | bar-out.rda

This will facilitate:

1. Understanding a new project: Learn quickly what script uses and
   creates other files.
   
1. Packaging/cleaning: Remove items that are not used/critical to the
   final product.
   
### General approach

Conceptually, the task of mapping out the structure of a project
involves two steps.  First, we need to identify which functions are
doing the reading of input files (e.g., read.csv, load, readRDS, ect.)
and writting output file (e.g., save, write.csv, etc.).  We then can
use CodeDepends to parse the scripts, find the relevant function
calls, and then parse the arguments to each to find the files
referenced.  In some cases, we might need to traverse backwards to
find the file name, e.g.:

```
f = "/some/file/foo.csv"
read.csv(f)
```

To go one step further, once we have the results in tabular form, we
can create a dependency graph, which can be plotted/visualized. Using
the results, we can even identify orphaned files "automatically",
e.g. scripts that produce files that are not used elsewhere, files
that are never used, etc.

### Implementation

**NEEDS WORK**

```
library(CodeAnalysis)
library(CodeDepends)
```

Suppose we have a project which resides in "project\_dir".  First, we
need to find all the R scripts in the project,

```
base_dir = "project_dir"

scripts = list.files(base_dir, pattern = "\\.[Rr]$", full.name = TRUE)
```

Next, we utilize the `readScript()` function in CodeDepends to
programmatically read each of these scripts,

```
sscripts = lapply(scripts, readScript)
info = lapply(sscripts, as, "ScriptInfo")
```

We will also name each element of the list after the file name to help
keep everything straight,

```
names(info) = scripts
```

Now, we will use the `getDepends()` function to find all the file
dependencies,

```
tmp = lapply(info, function(x) getDepends(,x))
```

Next, we will combine all the dependencies from all the different
scripts into a single table,

```
all.dep = do.call(rbind, tmp)
```

and fill in the source file names

```
all.dep$SourceFilename = rep(names(info), sapply(tmp, nrow))
rownames(all.dep) = NULL # seq(1, nrow(all.dep))
```

Now that we have all the file dependencies, we can separate them into files which are data input files, loaded, and written, 
```
loaded = basename(all.dep[all.dep$operation %in% c("read.csv", "load"), "filename"]) 
written = basename(all.dep[all.dep$operation %in% c("write.csv", "save"), "filename"]) 
data_files = list.files(base_dir, pattern = "\\.csv$|\\.rda$")
```

#### The ones that both written and read
intersect(written, loaded)

#### Not read
setdiff(data_files, loaded)

#### Not read or written
setdiff(data_files, c(written, loaded))

