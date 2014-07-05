library(CodeAnalysis)

f =
function(x)
{
   ans = c()
   for(i in x)
       ans = c(ans, h(i))
   ans
}

findLoopConcat(f)


g =
function(x)
{
   ans = c()
   for(i in x)
     for(j in x)
         ans = c(ans, h(i, j))
   ans
}

findLoopConcat(body(g))

