d = read.csv("../foo.csv")
ll = readLines("sub/lines.txt")

d2 = read.csv(sprintf("Data_%s.csv", Sys.Date()))

readRDS("results.rds")

saveRDS(d2, "today.rds")
write.csv(d2, "today.csv")



