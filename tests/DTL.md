

l5 = quote(
    for(i in x){
        tmp = y[i]
        z[tmp] = foo()
    }
)
p5 = checkParLoop(l5)

This amounts to 
  z[y[i]] = foo()
And one can do
```
const = foo()
u = unique(y[x])
z[u] = const
```
  
CF: If we pull out the loop invariants, then yes, it can be parallel.

  
  
l6 = quote(
    for(i in x){
        i = 1
        y[i] = foo()
    }
)
Ultimately, we want to reduce this to 
```
y[1] = foo()
```

CF: Yes, we could make these changes statically.
  But who actually writes such code?


```
l7 = quote(
    for(i in x){
        y[i %% k] = foo(y[i %% k])
    }
)
```

Again, we can reduce this to
```
u = unique(x %% k)
y[u] = foo(y[u])
```

CF: This code transformation is only valid if i < k for all iterations.
    If i < k, then there's no reason to do i %% k.
    Generally there's a true loop dependency here, for example:
```{r}
n = 25
k = 10
y = rep(0, k)
x = seq(n)
x[x %% k == 0] = 1
foo = function(z) z + 1
for(i in x){
    y[i %% k] = foo(y[i %% k])
}
> y
 [1] 5 3 3 3 3 2 2 2 2 0

y2 = rep(0, k)
u = unique(x %% k)
y2[u] = foo(y2[u])
> y2
 [1] 1 1 1 1 1 1 1 1 1 0
```





Let's elevate the diagnosis here to the more general "what are you doing changing the iterator
variable"
If it is `i = fun(i)`, that might be okay depending on how i is used later.






From QTL/simulate.R 
quote(
    for(i in 1:nchr(cross)) {
        o <- grep("^QTL[0-9]+", colnames(cross$geno[[i]]$data))
        if(length(o) != 0) {
            qtlgeno <- cbind(qtlgeno, cross$geno[[i]]$data[,o,drop=FALSE])
            cross$geno[[i]]$data <- cross$geno[[i]]$data[,-o,drop=FALSE]
            if(is.matrix(cross$geno[[i]]$map))
                cross$geno[[i]]$map <- cross$geno[[i]]$map[,-o,drop=FALSE]
            else
                cross$geno[[i]]$map <- cross$geno[[i]]$map[-o]
        }
    })
	
This says not parloop. But this is setting the i-th element and only the i-th
element in the loop.

d2 = quote(for(i in 1:nchr(cross))
        storage.mode(cross$geno[[i]]$data) <- "integer")

+ Again, should be TRUE since just the i-th element.
+ This is an fun<- call which is interesting and challenging

So we need to broaden the criteria and analysis.
I think we need a predicate that says "only operates on the i-th element"

CF: Yes, this sounds good. Maybe "only modifies the i-th element".


d3 = quote(        for(i in 1:n.qtl) {
            temp <- map[[model[i,1]]]
            if(model[i,2] < min(temp)) {
                temp <- c(model[i,2],temp)
                names(temp)[1] <- paste("QTL",i,sep="")
            }
            else if(model[i,2] > max(temp)) {
                temp <- c(temp,model[i,2])
                names(temp)[length(temp)] <- paste("QTL",i,sep="")
            }
            else {
                j <- max((seq(along=temp))[temp < model[i,2]])
                temp <- c(temp[1:j],model[i,2],temp[(j+1):length(temp)])
                names(temp)[j+1] <- paste("QTL",i,sep="")
            }
            map[[model[i,1]]] <- temp
        })

So definitely not clear that model[i,j] is unique across iterations.





checkParLoop() doesn't get this right.
It says 
*variable `buildOut` is assigned to using an index which is not the iterator variable in the loop:
buildOut[[i]] = tmpSeed"*

e = quote(for (i in 1:max(ctrl$seed)) {
    ind <- ctrl$aID[ctrl$seed == i]
    buildSeed <- list()
    for (j in seq_along(ind)) {
        print(ind[j])
        fileIn <- list.files("./", pattern = paste0("_", ind[j],
            ".rds"), full.names = T)
        tmp <- readRDS(fileIn)
        buildSeed[[j]] <- tmp
    }
    tmpSeed <- do.call(cbind.data.frame, buildSeed)
    buildOut[[i]] <- tmpSeed
})
