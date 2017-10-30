# Reads and modifies the data

df = read.csv("first.csv")

df$d = as.integer(df$a) + df$b

write.csv(df, file = "second.csv", row.names = FALSE)

# An unused file
save(df, file = "not_used.rda")
