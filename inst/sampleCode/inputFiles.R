d = read.csv("../foo.csv")
ll = readLines("sub/lines.txt")

d2 = read.csv(sprintf("Data_%s.csv", Sys.Date()))

readRDS("results.rds")

saveRDS(d2, "today.rds")
write.csv(d2, "today.csv")

utils::write.table(d2, "foo.csv")

MASS::write.matrix(mat, "myMatrix.mat")


png("foo.pdf", 7, 8)
dev.off()

jpeg("foo.jpg", 1000, 1000)
dev.off()

png("foo.png", 1000, 1000)
dev.off()


