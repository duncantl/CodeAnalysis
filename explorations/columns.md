2. [TOP] [Clark] Identify code which can be safely evaluated during static
   analysis, ie. `c(1:2, 4:6)`. Example using this information for another part
   of analysis

Simple example:
```{R}

df = read.csv(...)

grp1 = c("a", "b")

df = df[, grp1]
```

Idea: Some things can be known at code analysis time.

Many functions we can (probably) safely evaluate: `c, paste, seq, :, `

Richer:
```{R}

df = read.csv(...)

grp1 = c("a", "b")
grp2 = c("e", "f")
grp3 = paste0("V", 1:3)

f(df[, grp1])
...

g(df[, grp2])
...

h(df[, c(grp1, grp2)])

```


Loop invariants:

```{R}

for(i in x){
    y = f(z)
    g(i, y)
}

# Becomes:

y = f(z)
for(i in x){
    g(i, y)
}

```

