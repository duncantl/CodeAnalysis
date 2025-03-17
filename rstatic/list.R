ll = readLines("~/GitWorkingArea/CodeAnalysis/rstatic/ToChange.md")
ll = ll[ll != ""]
s = split(ll, cumsum(grepl("^#", ll)))
names(s) = gsub("# ", "", sapply(s, `[`, 1))
sapply(s[2:5], function(x) length(grep("^\\+", x)))

f = sapply(s[2:5], function(x) gsub("^\\+ ", "", grep("^\\+ *", x, value = TRUE)))

u = unlist(f)
u2 = gsub("^\\[[^]]+ ", "", u)
w = duplicated(u2)
table(w)

# Show the current ones we haven't finished.
sapply(f[2:5], function(x) grep("^[\\[√]", x, value = TRUE, invert = TRUE))


