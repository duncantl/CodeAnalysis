library(CodeAnalysis)

f =
function()
{
  a = 1
  b = 2
  c = 3
  d = 10
  a + c
}
f =
function()
{
  a = 1
  b = 2
  c = 3
  d = 10
  a + c
}

findUnusedAssignments(f)

f2 = function(x)
{
    a = 1
    if(x > 3) {
        b = foo(2)
        a = a + b
    }

    a
}

x = findUnusedAssignments(f2, FALSE, FALSE)
# WRONG:  thinks b is not necessary but it is.




