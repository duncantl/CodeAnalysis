findAssignsInLoop =
    # 
    # The intent of this is to find variables within a loop 
    # Our motivation is to be able to identify code where we can reuse the same
    # memory in each iteration rather than reallocate the entire space.
    # The bootstrap is a good example. See below.
    #
function(code, ...)
{
   UseMethod("findAssignsInLoop")
}

`findAssignsInLoop.for` =
function(code, ...)
{    
  as(code[[4]], "ScriptNodeInfo")
}

`findAssignsInLoop.{` = 
function(code, ...)
{    
  lapply(code[-1], findAssignsInLoop, ...)
}

`findAssignsInLoop.expression` =
function(code, ...)
{    
  lapply(code, findAssignsInLoop, ...)
}

#XXX This is not a replacement method, but a method for objects of class <-
# Added value just to keep R CMD check happy, but probably need to use S4 classes to avoid
# treating this as a replacement operator.
`findAssignsInLoop.<-` = `findAssignsInLoop.=` =
function(code, ..., value)
{    

}


if(FALSE) {
ee = expression({ans = numeric(B);
                 for(i in 1:B) {
                     d.star = data[sample(1:n, n, replace = TRUE), ]
                     ans[i] = T(d.star, ...)
                  }})
}
