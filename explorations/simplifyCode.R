# From Scott Devine's GreenWater https://github.com/dsidavis/GreenWater repository
# in Analysis/chp1.Muaggregate.by.year.R

MUAggregate.AllYrs <- function(df, cropname) {
  Irr.1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.1'))
  Irr.Last_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.Last'))
  GW.ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.growing'))
  Irr.app.total_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.app.total'))
  ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.growing'))
  E.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.growing'))
  T.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.growing'))
  H2O.stress_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='H2O.stress'))
  GW.E.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.E.to.Irr1'))
  GW.T.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.T.to.Irr1'))
  GW.ET.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.to.Irr1'))
  ET.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.annual'))
  E.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.annual'))
  T.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.annual'))
  deep.perc.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='deep.perc.annual'))
  winter.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='winter.deep.perc'))
  post.Irr1.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='post.Irr1.deep.perc'))
  fall.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='fall.deep.perc'))
  GW.capture.net_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.capture.net'))
  Dr.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.end.season'))
  RAW.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="RAW.end.season"))
  PAW.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="PAW.end.season"))
  P.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="P.end.season"))
  Irr.end.storage_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="Irr.end.storage"))
  result <- list(Irr.1_allyrs, Irr.Last_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, H2O.stress_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, GW.ET.to.Irr1_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, deep.perc.annual_allyrs, winter.deep.perc_allyrs, post.Irr1.deep.perc_allyrs, fall.deep.perc_allyrs, GW.capture.net_allyrs, Dr.end.season_allyrs, RAW.end.season_allyrs, PAW.end.season_allyrs, P.end.season_allyrs, Irr.end.storage_allyrs)  
  names(result) <- c('Irr.1.doy', 'Irr.Last.doy', 'GW.ET.growing', 'Irr.app.total', 'ET.growing', 'E.growing', 'T.growing', 'H2O.stress', 'GW.E.to.Irr1', 'GW.T.to.Irr1', 'GW.ET.to.Irr1', 'ET.annual', 'E.annual', 'T.annual', 'deep.perc.annual', 'winter.deep.perc', 'post.Irr1.deep.perc', 'fall.deep.perc', 'GW.capture.net', 'Dr.end.season', 'RAW.end.season_allyrs', 'PAW.end.season_allyrs', 'P.end.season_allyrs', 'Irr.end.storage_allyrs')
  #rm(Irr.1_allyrs, Irr.Last_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, H2O.stress_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, GW.ET.to.Irr1_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, deep.perc.annual_allyrs, winter.deep.perc_allyrs, post.Irr1.deep.perc_allyrs, fall.deep.perc_allyrs, GW.capture.net_allyrs, Dr.end.season_allyrs, RAW.end.season_allyrs, PAW.end.season_allyrs, P.end.season_allyrs, Irr.end.storage_allyrs)
  #gc()
  if (cropname=='alfalfa.imperial') {
    result['GW.capture.net'] <- NULL
  }
  result
}


b = body(MUAggregate.AllYrs)
# Find the top-level  x <- do.call() 
w = sapply(b, function(x) is(x, "<-") && is.call(x[[3]]) && x[[3]][[1]] == "do.call")

# Check all the do.call() expressions are the same up to the varname
tmp = function(e) {
         e[[3]][[3]][[4]] = "bob"
         e
      }
sapply(b[w], function(a, b) identical(tmp(a)[[3]], b), tmp(b[w][[1]])[[3]])


# Since they are the same, get the value of varname in the expression
vars = sapply(b[w], function(x) x[[3]][[3]][[4]])

# Replace all of the
#   x <- do.call()
# with an
#   result <- lapply(vars, function(varname) do.call(..., varname = varname))
# 
f = function(varname) x
do = b[w][[1]][[3]]
do[[3]][[4]] = as.name("varname")
body(f) = do
e = substitute(result <- lapply(vars, f), list(f = f, vars = vars))

# Now create the replacement body
b2 = b[!w]
b2[[2]] = e

MUAggregate.AllYrs2 = MUAggregate.AllYrs
body(MUAggregate.AllYrs2) = b2
