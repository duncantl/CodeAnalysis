# Simple script to create a data file

df = data.frame(a = letters[1:10],
                b = sample(10),
                c = sample(c(TRUE,FALSE), 10, TRUE))

write.csv(df, file = "first.csv", row.names = FALSE)
