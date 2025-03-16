hasHeader = TRUE
dir = "path/to/"
file = "foo.csv"

f = file.path(dir, file)
read.csv(f, header = hasHeader)



f1 =
function(x)    
{
    col = ",\n"
    paste(x, collapse = col)
}

f2 =
function(x, newLine = TRUE)    
{
    col = if(newLine)
              ",\n"
          else
              "; "
    paste(x, collapse = col)
}


f3 =
function(x)    
{
    a = TRUE
    label = "abc"
    col = ",\n"
    # don't propagate col    
    attr(col, label) = "xyz"
    foo(col) = 10
    # want to propagate a but not col here.
    foo(bar(col, a)) = 11
    paste(x, collapse = col)
}
