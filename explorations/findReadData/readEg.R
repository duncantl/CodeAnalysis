
file = "bob.csv"
d = read.csv(file)


d[c(1, 4)] = lapply(d[c(1, 4)], as.Date)


source("funsEg.R")

z = myRead("data1", "data2")

z2 = myRead2("xdata1", "xdata2")


f = read.csv
g = file  # other alias that is not a read data function.
d3 = f("jane.csv")


getData = 
function(f)
{
   readLines(f)
}

foo =
function(x)
    sin(x+1)

# Never gets called so not returned, but is included
# in the functions that do a read when recursive = TRUE
bar = function(x)
         scan(x, "integer")

points = getData("data.txt")

plot(points, d$x)

d2 = lapply(files, getData)


