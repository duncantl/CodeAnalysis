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

findUnusedAssignments(f)


