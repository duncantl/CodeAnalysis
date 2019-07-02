
Loop invariant code:
[wikipedia](https://en.wikipedia.org/wiki/Loop-invariant_code_motion)

```{r}
for(i in x){
    y = f(z)    # <- This line is loop invariant
    g(i, y)
}
```

_hoisting_ is rewriting the code so that the loop invariant code is outside of the loop.

```{r}
y = f(z)        # <- Hoist the loop invariant code outside the loop
for(i in x){
    g(i, y)
}
```

A code analyzer can use the generalized idea of use-definitions to decide when code is loop invariant.
It's easier to identify loops as parallel once we pull the loop invariants out.

Duncan's example:

```{r}
for(i in seq(n)){
    names(y)[i] = names(x)[i]
}
```

Of course, this whole code should be vectorized in the first place, as follows:

```{r}
# Probably what the user should be writing:
names(y) = names(x)

# If something weirder is going on:
sn = seq(n)
names(y)[sn] = names(x)[sn]
```

Instead, If we look at pulling out the loop invariants, the object that corresponds to the subexpression `names(x)` is only ever being read, never written to.
We could modify the loop as follows:

```{r}
nx = names(x)
for(i in seq(n)){
    names(y)[i] = nx[i]
}
```

We still have to think about how to handle the functional assignment in `names(y)[i]`, if we're going to make the loop parallel.
