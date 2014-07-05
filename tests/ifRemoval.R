library(CodeAnalysis)
#XXX Fix h - body is list(x)
h = function(x) x

f =
function(x)
{
   if(FALSE) {
       print(x)
   }

   if(TRUE) {
       g(x)
   }  else
       h(x)

   if(FALSE && x > 10) {
       a(x)
   }  else 
       b(x)

}

g =
function(x)
{
   for(i in x) {
       if(FALSE)
           print(x)
       x[i] = x[i] + 1
   }
   x
}


ok =
function(x)
{
   if(x > 10)
       print(x)

   y = numeric(length(x))
   if(length(x) && TRUE)
        for(i in seq(along = x)) {
           if(i < -1)
               print(x)
           y[i] = sin(x[i])
        }
   y
}


notOK =
function(x)
{
   if(x > 10)
       print(x)

   y = numeric(length(x))
   if(length(x) && FALSE)  # this will be removed.
        for(i in seq(along = x)) {
           if(i < -1)
               print(x)
           y[i] = sin(x[i])
        }
   y
}



ifElse =
function()
{
  if(length(x) > 10)
      print(x)
  else if(length(x) > 5 && FALSE)
      show(x)
  else
      cat("x is small\n")
}
