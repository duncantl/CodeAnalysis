As a project evolves, we often have a directory with a collection of R script files,
functions in other files, and data in CSV, RDS, and/or RDA files.
At some point, we need to make these available to other people.
These people might be reviewers or readers of a paper.
They might also be collaborators who are helping to debug some code
or who just want to explore the computations.
Or we simply may want to commit the relevant files to a version control system.
In these cases, we don't want to include every file in the directory
as there are often unneeded files that merely confuse others (and ourselves).
Instead, we want to be able to provide the minimal set of 
files these people need to run the computations. 
In some cases, we may even want to provide the minimal set 
of files for a subset of the computations.

Often, we will send the code to somebody and omit one or more critical files.
The recipient runs the code and gets an error, has to figure out what the problem is
and then ask us to send these separately. This is frustrating, takes
time and slows the entire process.

So we would like  a mechanism that analyzes the different 
code files and identifies, where possible, which files are needed as inputs
and provide a list of these input files.


This may not always be possible as the names of the files may be computed dynamically
when the script is run. However, often these are static files.


Another 






