People often use full paths to the files they read (either R code or data files)
in their scripts, e.g.

(code and data available at https://osf.io/gkwjx/)

```
source("~/vt_project/code/funs.R")
source("~/vt_project/code/calc_weather_funs.R")

load("~/vt_project/data/vt1995_2015_clean.Rda")
load("~/vt_project/data/weather_stn_info.Rda")
load("~/vt_project/data/all_weather_filled.Rda")
```
One issue with this is that when we want to run this on
another machine - either another person or the original author -
or with a different dataset, we have to change the path(s) throughout 
the script. This is an example of repeating ourselves.
In this example, we have to change ~/vt_project in five places.
Instead, a more flexible way to write this code is
```
baseDir = "~/vt_project"
codeDir = file.path(baseDir, "code")
dataDir = file.path(baseDir, "data")
```
```
source(file.path(codeDir, "funs.R"))
source(file.path(codeDir, "calc_weather_funs.R"))

load(file.path(dataDir, "vt1995_2015_clean.Rda"))
load(file.path(dataDir, "weather_stn_info.Rda"))
load(file.path(dataDir, "all_weather_filled.Rda"))
```
With this approach, it is relatively easy to specify alternative directories
in just one place and then all of the code remains the same.
So we would like to be able analyze a script and rewrite it using this idiom.

We can find these calls  using CodeDepends.
We read the script and then get the information for each expression with
```
library(CodeDepends)
f = "~/DSIProjects/MattCleanProject/Variety_trial_analysis/code/calc_weather_var.R"
sc = readScript(f)
info = as(sc, "ScriptInfo")
```
We can now loop over these and find the functions they call:
```
funs = sapply(info, function(x) names(x@functions))
```

We then find which expression call read.table, read.csv, load or save or any of the other
I/O functions:
```
w = sapply(funs, function(x) any(x %in% c("read.table", "read.csv", "load", "save", "source")))
```

We get the strings from these expressions:
```
files = sapply(info[w], slot, "strings")
```
[1] "~/vt_project/code/funs.R"                         
[2] "~/vt_project/code/calc_weather_funs.R"            
[3] "~/vt_project/data/vt1995_2015_clean.Rda"          
[4] "~/vt_project/data/weather_stn_info.Rda"           
[5] "~/vt_project/data/all_weather_filled.Rda"         
[6] "~/vt_project/vt_database/site_characteristics.csv"
[7] "~/vt_project/data/site_weather.rda"               
[8] "~/vt_project/data/all_vt_weather.Rda"             


We find the path(s) common to these files:
```
dirname(files)
```
We see ~/vt_project/code, ~/vt_project/data and ~/vt_project/vt_database.
So we can create variables for these directories as we did before
and then change the code use these.
We can compute the common path using a simple approach or via suffix trees (see R suffixtree
package):

```
paths = getCommonPath(files)
```

```
pathVars = defPathVars(, paths)
```

We then have a collection of strings identifying paths to files in the code and a 
one or more variables. We process each of these expressions and substitute the 
string with a call to file.path() with the relevant variable 
```
sc[w] = lapply(sc[w], replacePath, pathVars)
```
And then we can write the script back to a file.



## Changing the Working Directory
People also sometimes use setwd() in their scripts 
to temporarily move to a directory and then read or write a file in that directory.
This is generally not a good idea. It amounts to using global variables.
As we add code to the script, it has to know the current working directory.
If we move code arom one part of the script to another, it has to know which
directory we are currently in.
And if there is an error in the script, we are left in an unexpected directory.

So we would like to detect uses of setwd() and remove them, and update
calls to read.csv(), write.csv(), source(), load(), etc.  to use the appropriate directory name.

Consider the file FAO56_dualcropcoeff.R
This starts with
```
modelscaffoldDir = "Data/Oct2017"
setwd(modelscaffoldDir)

irrigation.parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop.parameters.df <- read.csv('crop_parameters.csv', stringsAsFactors = F)
```

Ideally we would like the calls to read.csv() to read as
```
irrigation.parameters <- read.csv(file.path('Data/Oct2017', 'irrigation_parameters.csv'), stringsAsFactors = FALSE)
crop.parameters.df <- read.csv(file.path('Data/Oct2017', 'crop_parameters.csv'), stringsAsFactors = FALSE)
```
Or we could directly fuse the string literals together to get, e.g., 
'Data/Oct2017/irrigation_parameters.csv'
If the script uses variables within calls to read.csv(), etc., we can't fuse them
at code analysis time, so using file.path() is more flexible.
Similarly, if the actually directory is not fixed, but dynamically computed,
we will want to assign it to a variable when we replace the call to setwd(),
e.g.,
```n
.CurrentWorkingDirectory = "Data/Oct2017"
```

As before, we need to parse the expressions in our script
and then find the terms we need to change.
These include the calls to setwd() and all of the expressions that 
read or write.

```
f = "~/CompileExamples/GreenWater/ModelConstruction/FAO56_dualcropcoeff.R"
sc = readScript(f)
info = as(sc, "ScriptInfo")
```

```
w = which(sapply(info, function(x) "setwd" %in% names(x@functions)))
```





One might argue that we have to know all the functions that read and write
from or to a file. Indeed this is true. However, we can find these with code analysis,
once we know the basic functions that do this.
