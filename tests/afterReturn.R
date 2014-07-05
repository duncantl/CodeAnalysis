library(CodeAnalysis)
f =
function()
{
  x = 1
  y = 2
  z = x + y
  return(z)
  a = 1
  b = 2 
}

removeAfterReturn(f)


g =
function()
{
  a = 1
  b = 2
  if(a < b) {
      z = a + b
      return(z)
      x = 1
      y = 2
  }

  return(a - b)

  x = 1
  y = 2
}

removeAfterReturn(body(g)[[4]][[3]])
removeAfterReturn(body(g))
removeAfterReturn(g)



w =
    # check a while loop
function()
{
   a = 1
   b = 2
   while(a < 10) {
       a = a + 1
       return(a)
       x = 10
   }
   return(b)
   x = 1
}
removeAfterReturn(w)


cond =
    # check an if
function()
{
   a = 1
   b = 2
   if(a < 10) {
       a = a + 1
       return(a)
       x = 10
   } else {
       b = 2
       return(b)
       x = 100
   }
   return(b)
   x = 1
}
removeAfterReturn(cond)
