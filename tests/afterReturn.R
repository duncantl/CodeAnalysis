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
