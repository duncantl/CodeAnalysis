
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

