points.resultsDir <- 'D:/Allowable_Depletion/results'
clean.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Sep2017/clean_results' #needs to be changed
original.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Sep2017'
#pointsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/data.frames/Aug2017'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
# setwd(modelscaffoldDir)
# list.files()
# cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
# alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
# grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
# almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
# walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
# pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
# cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
# raster.model.codes <- raster('model.codes.Aug2017.tif')
# P.df <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
# U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
# RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
# ETo.df <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533

#read-in all points of interest
# setwd(pointsDir)
# model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
# model_points$mukey_cropcode <- NULL
# model_points$model_code <- NULL
# sum(model_points$crop_code==75, na.rm=TRUE) #equal to 547,945.9 ha or 1,353,427 acres, an overestimate for almonds
# sum(model_points$crop_code==76, na.rm=TRUE)
# almond_points <- model_points[which(model_points$crop_code==almond_code),]
# walnut_points <- model_points[which(model_points$crop_code==walnut_code),]
# grape_points <- model_points[which(model_points$crop_code==grape_code),]
# pistachio_points <- model_points[which(model_points$crop_code==pistachio_code),]
# alfalfa_points <- model_points[which(model_points$crop_code==alfalfa_code),]

#read in climate summaries produced by script below
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# list.files()
# prism.annual.sums <- read.csv('P.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# ETo.summary <- read.csv('ETo.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# RHmin.summary <- read.csv('RHmin.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# U2.summary <- read.csv('U2.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)


#get the water year P total by cell number, including 2017 for annual average
# P.df$water.year <- P.df$year
# P.df$water.year[which(P.df$month >= 10)] <- P.df$water.year[which(P.df$month >= 10)] + 1
# prism.by.year <- split(P.df, P.df$water.year)
# for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now
#   prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
# }
# prism.annual.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
# prism.annual.sums <- t(prism.annual.sums)
# prism.annual.sums <- as.data.frame(prism.annual.sums)
# prism.annual.sums$cell_name <- rownames(prism.annual.sums)
# prism.annual.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.annual.sums$cell_name))
# colnames(prism.annual.sums)
# prism.annual.sums$mean.annual.P <- apply(prism.annual.sums[,1:14], 1, mean)
# gc()
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# write.csv(prism.annual.sums, 'P.WY.summary.10.6.17.csv', row.names = FALSE) #WY refers to 'water year'

#summarize ETo, RHmin, and U2 data by various time windows
#if output is water.year summary then modify='no' & WY.basis='yes'
#if output is calendar year summary then modify='no' & WY.basis='no'
#if output is dormant period, then modify='no' & WY.basis='no'
ClimateAggregate <- function(climate_df, varname, modify, Jdev, Jharv, WY.basis) {
  if (WY.basis=='yes') {
    climate_df$water.year <- climate_df$year
    climate_df$water.year[which(climate_df$month >= 10)] <- climate_df$water.year[which(climate_df$month >= 10)] + 1
    by.year <- split(climate_df, climate_df$water.year)
  } else {
      by.year <- split(climate_df, climate_df$year)
  }
  if (modify=='yes') {
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,5]==Jharv):which(by.year[[j]][,5]==Jdev),] #row 1 in each of these data.frames is Oct. 1st, start of the water.year; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
  }
  for (j in 1:length(by.year)) { #get the unnecessary columns out now, including last column which is water.year
    by.year[[j]] <- by.year[[j]][,6:(ncol(by.year[[j]])-1)]
  }
  annual.summary <- do.call(rbind, lapply(by.year, sapply, if(varname=='ETo') {sum} else{mean}, na.rm=TRUE)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
  annual.summary <- t(annual.summary)
  annual.summary <- as.data.frame(annual.summary)
  annual.summary$cell_name <- rownames(annual.summary)
  annual.summary$CIMIScellnumber <- as.integer(gsub('cell_', '', annual.summary$cell_name))
  colnames(annual.summary)
  annual.summary[[paste0(varname, '.mean.annual')]] <- apply(annual.summary[,1:14], 1, mean)
  annual.summary
}

PrecipAggregate <- function(precip_df, modify, Jdev, Jharv, WY.basis) {
  
}

# ETo.summary <- ClimateAggregate(ETo.df, 'ETo', 'no') #note that 2017 does not include latter half of September
# RHmin.summary <- ClimateAggregate(RHmin.df, 'RHmin', 'no')
# U2.summary <- ClimateAggregate(U2.df, 'U2', 'no')
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# write.csv(ETo.summary, 'ETo.WY.summary.10.6.17.csv', row.names = FALSE) #WY refers to 'water year'
# write.csv(RHmin.summary, 'RHmin.WY.summary.10.6.17.csv', row.names = FALSE)
# write.csv(U2.summary, 'U2.WY.summary.10.6.17.csv', row.names = FALSE)

#this aggregates results across all years by mukey and unique model code according to the 'func', such as taking the mean GW.ET.growing for each unique combination of climate, soil, and crop from 2004-2016, with major component weighted averages.
# AggregateSoilPars <- function(df, varname, func, ...) {
#   df$unique_model_code_final <- paste0(as.character(df$cokey), as.character(df$unique_model_code)) #necessary because some original unique_model_code_final were trimmed because they were too long.  better if coerced to character first
#   var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
#   mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
#   comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
#   modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
#   results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
#   results <- as.data.frame(results)
#   compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
#   colnames(compsums) <- 'compsums'
#   compsums$modelcode <- rownames(compsums)
#   results <- merge(results, compsums, by='modelcode')
#   var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
#   var.final <- as.data.frame(var.final)
#   colnames(var.final) <- 'var.final'
#   var.final$unique_model_code <- rownames(var.final)
#   var.final$var.final <- as.numeric(var.final$var.final)
#   var.final <- var.final[,c(2,1)]
#   colnames(var.final)[2] <- varname
#   var.final
# }
#35287 unique walnut codes
#these have all had model_codes eliminated ahead of time where PAW, TEW, or REW is NA or equal to 0, which would have been skipped in FAO56 dual crop coeff runs
# results <- cbind(df[!is.na(df[[varname]]), c(varname, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ])
AggregateSoilPars <- function(df, paw.varname, cell.counts) {
  df <- df[which(df$Model.Year==2004), ] #only need one year to get the soil stats
  compsums <- as.data.frame(tapply(df$comppct_r, df$unique_model_code, sum))
  colnames(compsums) <- 'compsums'
  compsums$unique_model_code <- as.integer(rownames(compsums))
  compsums$compsums <- as.numeric(compsums$compsums)
  results <- cbind(df[ ,c(paw.varname, 'TEW', 'REW', 'surface.depth', 'comppct_r')], compsums[match(df$unique_model_code, compsums$unique_model_code), ])
  var.final <- as.data.frame(tapply(results[[paw.varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  colnames(var.final) <- paw.varname
  var.final$unique_model_code <- as.integer(rownames(var.final))
  var.final[[paw.varname]] <- as.numeric(var.final[[paw.varname]]) #simplify from array to numeric vector and convert to mm_H2O
  var.final <- var.final[,c(2,1)] #make unique_model_code the first column
  var.final[[gsub('cm', 'mm', paw.varname)]] <- var.final[[paw.varname]]*10
  var.final$TEW <- as.numeric(tapply(results$TEW*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$REW <- as.numeric(tapply(results$REW*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$surface.depth <- as.numeric(tapply(results$surface.depth*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$comppct_total <- results$compsums[match(var.final$unique_model_code, results$unique_model_code)]
  var.final$mukey <- df$mukey[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$comps_total <- df$n_compkeys[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$comps_maj <- as.integer(tapply(results$unique_model_code, results$unique_model_code, length))
  compnames <- split(df$compname, df$unique_model_code)
  for (i in seq_along(compnames)) {
    if (length(compnames[[i]]) > 1) {
      compnames[[i]] <- paste(compnames[[i]], collapse = ', ')
    }
  }
  cokeys <- split(df$cokey, df$unique_model_code)
  for (i in seq_along(cokeys)) {
    if (length(cokeys[[i]]) > 1) {
      cokeys[[i]] <- paste(cokeys[[i]], collapse = ', ')
    }
  }
  var.final$compnames <- as.character(compnames)
  var.final$cokeys <- as.character(cokeys)
  var.final$PRISMcellnumber <- df$PRISMcellnumber[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$CIMIScellnumber <- df$CIMIScellnumber[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$cellcounts30m2 <- cell.counts$cell_counts30m2[match(var.final$unique_model_code, cell.counts$unique_model_code)]
  rownames(var.final) <- NULL
  var.final
}

#this aggregates the model results by map unit, so that there are no duplicate unique model codes in the results (i.e. unique model codes with more than one major component have their results averaged as a component weighted average).  For this function, results for all model years are maintained, whereas in AggregateResults() below, multiple years data is compressed into a single statistic.
MUAggregate <- function(df, varname) {
  year <- df$Model.Year[1]
  if (year==2003 | year==2017) { #test code to simplify results collection
    return(NULL)
  }
  #print(year)
  if (varname=='Irr.1' | varname=='Irr.Last') {
    df[[varname]][which(df[[varname]]=='1900-01-01')] <- NA
    df[[varname]] <- as.Date(df[[varname]], format='%Y-%m-%d')
    varname_doy <- paste0(varname, '.doy')
    df[[varname_doy]] <- as.integer(format.Date(df[[varname]], '%j'))
    compsums <- as.data.frame(tapply(df$comppct_r[!is.na(df[[varname]])], df$unique_model_code[!is.na(df[[varname]])], sum)) #this sums up component percentages (that have data) by unique_model_code
    colnames(compsums) <- 'compsums'
    compsums$unique_model_code <- as.integer(rownames(compsums))
    results <- cbind(df[!is.na(df[[varname]]), c(varname_doy, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ]) #this eliminates rows with varname as NA and then adds the total component percentage calculated above
    var.final <- tapply(results[[varname_doy]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
    var.final <- as.data.frame(var.final)
    colnames(var.final) <- 'var.final'
    var.final$unique_model_code <- rownames(var.final)
    rownames(var.final) <- NULL
    var.final$var.final <- as.integer(var.final$var.final)
    var.final <- var.final[,c(2,1)]
    colnames(var.final)[2] <- varname_doy
    #var.final$Model.Year <- year
    var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
    colnames(var.final)[3] <- 'Model.Year'
    #rm(compsums, results)
    #gc()
    var.final
  } else {
    compsums <- as.data.frame(tapply(df$comppct_r[!is.na(df[[varname]])], df$unique_model_code[!is.na(df[[varname]])], sum))
    colnames(compsums) <- 'compsums'
    compsums$unique_model_code <- as.integer(rownames(compsums))
    results <- cbind(df[!is.na(df[[varname]]), c(varname, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ])
    var.final <- tapply(results[[varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
    var.final <- as.data.frame(var.final)
    colnames(var.final) <- 'var.final'
    var.final$unique_model_code <- rownames(var.final)
    rownames(var.final) <- NULL
    var.final$var.final <- as.numeric(var.final$var.final)
    var.final <- var.final[,c(2,1)]
    colnames(var.final)[2] <- varname
    #var.final$Model.Year <- year
    var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
    colnames(var.final)[3] <- 'Model.Year'
    #rm(compsums, results)
    #gc()
    var.final
  }
}
#test the function with some almond results
# GW.ET.growing <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='GW.ET.growing'))
# Irr.1 <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.1'))
# Irr.Last <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.Last'))

#get data for all years into the points.  It works for a dataframe of points with or without multiple model years
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

#now, distribute these results to points across all years
SetValues <- function(main_df, var_df, varname){
  main_df[[varname]] <- var_df[[varname]][match(main_df$unique_model_code, var_df$unique_model_code)]
  #gc()
  main_df
}
SetValues.AllYrs <- function(var_df, varname, main_df) {
  if (is.null(main_df$Model.Year)) {
    main_df$Model.Year <- var_df$Model.Year[1]
    main_df[[varname]] <- var_df[[varname]][match(main_df$unique_model_code, var_df$unique_model_code)]
    main_df
  } else {
    model.year <- var_df$Model.Year[1]
    subset_main_df <- main_df[which(main_df$Model.Year==model.year), ]
    subset_main_df[[varname]] <- var_df[[varname]][match(subset_main_df$unique_model_code, var_df$unique_model_code)]
    #gc()
    subset_main_df
  }
}
SetValues.AllYrs.Combined <- function(var_df, main_df) { #note that gc() is called from function defined above, which is called within this function
  for (i in seq_along(var_df)) {
    final_results <- do.call(rbind, lapply(split(var_df[[i]], var_df[[i]]$Model.Year), SetValues.AllYrs, varname=names(var_df)[i], main_df= if (i==1) {main_df} else {final_results}))
    #print(i)
  }
  gc()
  final_results
}
SetPrecipValues.AllYrs <- function(df, precip_data, colname) {
  model.year <- as.character(df$Model.Year[1])
  df[[colname]] <- precip_data[[model.year]][match(df$PRISMcellnumber, precip_data$PRISMcellnumber)]
  gc()
  df
}
SetClimateValues.AllYrs <- function(df, varname, climate_data) {
  model.year <- as.character(df$Model.Year[1])
  df[[varname]] <- climate_data[[model.year]][match(df$CIMIScellnumber, climate_data$CIMIScellnumber)]
  gc()
  df
}
#replace scenario.resultsDir with clean.resultsDir
#run the functions above to loop through each crop's results folder to get the results to points on the landscape
#write only the map-unit aggregated results for years 2004-2016 and unique model_codes, along with cell_counts of those unique model codes 
data.to.allyrs <- function(cropname, cropname2) {
  setwd(modelscaffoldDir)
  cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
  cropcode <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME==cropname2]
  P.df <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = FALSE)
  ETo.df <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = FALSE)
  U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
  RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F)
  ETo.summary <- ClimateAggregate(ETo.df, 'ETo', modify='no', WY.basis = 'no') #note that 2017 does not include latter half of September
  RHmin.summary <- ClimateAggregate(RHmin.df, 'RHmin', modify = 'no', WY.basis = 'no')
  U2.summary <- ClimateAggregate(U2.df, 'U2', modify = 'no', WY.basis = 'no')
  cell.counts <- read.csv('cell_counts.csv', stringsAsFactors = FALSE)
  setwd(file.path(original.resultsDir, 'climate_summaries'))
  prism.annual.sums <- read.csv('P.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
  colnames(prism.annual.sums)

  setwd(file.path(clean.resultsDir, cropname))
  fnames <- list.files()
  print(fnames)
  for (i in seq_along(fnames)) {
    print(i)
    scenario_name <- gsub('_FAO56results_clean.csv', '', fnames[i])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    setwd(file.path(original.resultsDir, paste0(cropname, '_majcomps'), scenario_name))
    model_metadata <- read.csv(list.files(pattern = glob2rx('*_model_metadata.csv')), stringsAsFactors = FALSE)
    paw.varname <- model_metadata$paw.varname
    if (i==1) { #this block not relevant to alfalfa
      Jdev <- model_metadata$Jdev
      Jharv <- model_metadata$Jharv
      P.df$water.year <- P.df$year
      P.df$water.year[which(P.df$month >= 10)] <- P.df$water.year[which(P.df$month >= 10)] + 1
      prism.by.year <- split(P.df, P.df$water.year)
      for (j in 1:length(prism.by.year)) { #get the unnecessary rows out now
        prism.by.year[[j]] <- prism.by.year[[j]][which(prism.by.year[[j]][,5]==Jharv):which(prism.by.year[[j]][,5]==Jdev),] #this trims each data.frame to leaf-drop(Jharv) to bloom date (Jdev)
      }
      for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now, including last column which is water.year
        prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
      }
      prism.winter.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
      prism.winter.sums <- t(prism.winter.sums)
      prism.winter.sums <- as.data.frame(prism.winter.sums)
      prism.winter.sums$cell_name <- rownames(prism.winter.sums)
      prism.winter.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.winter.sums$cell_name))
      ETo.winter.sums <- ClimateAggregate(ETo.df, 'ETo', 'yes', Jdev, Jharv)
    setwd(file.path(clean.resultsDir, cropname))
    df <- read.csv(fnames[i], stringsAsFactors = FALSE)
    soil_summary <- AggregateSoilPars(df, paw.varname, cell.counts)
    if (!dir.exists(file.path(points.resultsDir, cropname))) {
      dir.create(file.path(points.resultsDir, cropname))
    }
    if (!dir.exists(file.path(points.resultsDir, cropname, 'MUaggregated_soildata'))) {
      dir.create(file.path(points.resultsDir, cropname, 'MUaggregated_soildata'))
    }
    setwd(file.path(points.resultsDir, cropname, 'MUaggregated_soildata'))
    write.csv(soil_summary, paste0(scenario_name, '_soilsdata.csv'), row.names = FALSE)
    results <- MUAggregate.AllYrs(df, cropname)
    #df_allyrs <- df[rep(seq.int(1, nrow(df)), 15), ] #change hard coding here
    #df_allyrs$years <- rep(2003:2017, nrow(df)) #change hard coding here
    df_allyrs <- SetValues.AllYrs.Combined(results, soil_summary) #check functions above
    rm(results, df, soil_summary)
    gc()
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=prism.annual.sums, colname='P.WY'))
    if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes') {
      df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=prism.winter.sums, colname='P.winter')) #Jharv to Jdev
    }
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, varname='ETo.annual', climate_data=ETo.summary)) #Jharv to Jdev
    if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes') {
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, varname='ETo.winter', climate_data=ETo.winter.sums))
    }
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, varname='RHmin.mean', climate_data=RHmin.summary))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, varname='U2.mean', climate_data=U2.summary))
    df_allyrs <- cbind(df_allyrs[ ,1:18], round(df_allyrs[ ,19:ncol(df_allyrs)], 3))
    fname <- paste0(gsub('_clean.csv', '', fnames[i]), '_points_rounded.csv')
    setwd(file.path(points.resultsDir, cropname))
    write.csv(df_allyrs, fname, row.names = FALSE)
    rm(df_allyrs)
    gc()
  }
 }
}
#run the function
data.to.allyrs('pistachios', 'Pistachios')
data.to.allyrs('almond.mature', 'Almonds')
data.to.allyrs('walnut.mature', 'Walnuts')
data.to.allyrs('grapes.table', 'Grapes')
data.to.allyrs('grapes.wine', 'Grapes')
#these results don't inlcude the P.winter or ETo.winter columns
data.to.allyrs('alfalfa.CV', 'Alfalfa')
data.to.allyrs('alfalfa.intermountain', 'Alfalfa')
#this result also does not include the GW.capture.net column
data.to.allyrs('alfalfa.imperial', 'Alfalfa') #this result does not include the 





#this is to obtain mean values across years, except for paw, which is static; almond_points_allyrs has the data by year
almond_gw_et <- AggregateResults(almond2.0m_AD50, 'GW.ET.growing', mean, na.rm=TRUE) #this produces 69,080 rows; concordance with above!
almond_paw <- AggregateResults(almond2.0m_AD50, 'z2.0m_cmH2O_modified_comp', unique)
almond_paw$paw_mm_2.0m <- almond_paw$z2.0m_cmH2O_modified_comp*10
almond_irr_app <- AggregateResults(almond2.0m_AD50, 'Irr.app.total', mean, na.rm=TRUE)
almond_et <- AggregateResults(almond2.0m_AD50, 'ET.growing', mean, na.rm=TRUE)
almond_e <- AggregateResults(almond2.0m_AD50, 'E.growing', mean, na.rm=TRUE)
almond_points <- SetPointValues(almond_points, almond_gw_et, 'GW.ET.growing')
almond_points <- SetPointValues(almond_points, almond_e, 'E.growing')
almond_points <- SetPointValues(almond_points, almond_et, 'ET.growing')
almond_points <- SetPointValues(almond_points, almond_paw, 'paw_mm_2.0m')
almond_points$paw_mm <- 10*almond_points$z2.0m_cmH2O_modified_comp
summary(lm(almond_points$GW.ET.growing ~ almond_points$paw_mm_2.0m))





