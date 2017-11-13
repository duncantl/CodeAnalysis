source("early_eval.R")

safe_funcs = c("=", "<-", "c", ":", "$", "[", "[[", "list", "seq")


mini = new.env(parent = emptyenv())

# Nothing here yet:
ls(mini)

# This works, discovers + in base
eval(1 + 2, envir = mini)

# Same
local(1 + 2)

# And it finds this in the stats package
eval(median(c(1, 2)), envir = mini)

# Then I need to modify the search path.
# docs say that it's not possible to remove base.

# Explicitly add them:
for(fname in safe_funcs){
    assign(fname, get(fname), mini)
}

ls(mini)


# Tests
############################################################

ex1 = parse(text = '
col = 100
dframe = read.csv("mydata.csv")
plot(dframe[, col])
')


# Expect it to fail and it does.
eval2(ex1, mini)


ex2 = parse(text = '
x = seq(from = 0, to = 1, length.out = 11)
y = 4:2
z = c(x, y)
')

# We need the methods for seq to actually work
for(m in methods(seq)){
    assign(m, get(m), mini)
}

# After this we have x, y, z defined inside mini. The end goal is to "fill
# in" the AST with evaluated expressions wherever possible.
eval2(ex2, mini)
