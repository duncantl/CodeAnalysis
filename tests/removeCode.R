library(codetools)
library(CodeAnalysis)

fun = function(x, y, z) {
    a = 2
    b = 1+2
    c = a + b
    foo(a, b, c)

    # remove the intermediate assignment as another check and remove b = as well and see if we
    # handle the case with a single call in the body.
    ans = (x + y)*z
    ans
}

p = function(x, ...) 
       isCallTo(x, "foo") || isAssignTo(x, c("a", "c"))

rw = genRemoveCode(p)
w = mkModifyCodeWalker(rw, FALSE)
f2 = walkCode(fun, w)    

# Add tests


######
# 2nd test  Remove function definitions.


fun2 = function(x, y, z) {
    len = sapply(list(x, y, z), length)
    if(any(len) == 0)
        stop("zero-length argument")
    
    a = function(v) x+v
    b = function(o) o/z

    while(length(z) > 1){
        w = a(x) > 10 & b(y) < 100
        z = z[w]
        x = x[w]
        y = y[w]
    }

    z
}

p = function(x, ...) 
    isCallTo(x, "function")

rw = genRemoveCode(p)
w = mkModifyCodeWalker(rw, FALSE)
f2 = walkCode(fun2, w)        
    


