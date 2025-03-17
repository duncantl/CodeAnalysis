Consider `.install_packages` in the tools package.

```
ns = getNamespace("tools")
tfns = as.list(ns, all.names = TRUE)
nt = sapply(tfns, numTerms)
summary(nt)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    15.0    47.5   167.5   122.0 20411.0 
```

```{r}
which.max(nt)
w = sapply(tfns, is.function)
nfns = lapply(tfns[w], getFunctionDefs)

table(sapply(nfns, length))
```

```
  0   1   2   3   4   5   6   7   8  10  11  12  15  18  22  23  25  36  48 
460 135  48  26  13   5   9   3   5   1   1   1   1   2   1   1   2   1   1 
```

```
id = names(nfns)[sapply(nfns, length) > 20]
```


```{r}
d = data.frame(numTerms = nt[id], 
               numFunctions = sapply(nfns[id], length),
			   sizeNestedFunctions = sapply(nfns[id], function(x) sum(sapply(x, numTerms))))
```

```{r}
ex = lapply(tfns[id], extractFunctions)
d$afterExtractedNumTerms = sapply(ex, function(x) numTerms(x$newFun))
```


