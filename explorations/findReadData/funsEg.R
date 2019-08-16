
myRead =
function(...)
{
  do.call(rbind, lapply(list(...), read.table))
}
