
myRead =
function(...)
{
  do.call(rbind, lapply(list(...), read.table))
}

myRead2 = myRead

