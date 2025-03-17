fixMileage =
function(x, which = 1)
{
   as.numeric(sapply( strsplit(x, " / "), `[`, 1))
}



ff = "carRatings.csv"
if(file.exists(ff)) {
    tbl = read.csv(ff)
} else {
    library(XML)
    doc = htmlParse(readLines("https://greenercars.org/greenercars-ratings/"))
    tbl = readHTMLTable(doc, which = 1)
    v = c("Green Score", "Weight")
    tbl[v] = lapply(tbl[v], as.numeric)
    tbl$Hybrid = grepl("/", tbl$City)

    v = c("CityMPG", "CityMPKW", "HiwyMPG", "HiwyMPKW")
    tbl[v] = NA
    mpg = grepl("Diesel|Hydrogen|Gasoline", tbl$FuelType) & !tbl$Hybrid
    v$CityMPG[mpg] = as.numeric(tbl$City[mpg])
    v$HiwyMPG[mpg] = as.numeric(tbl$Hiwy[mpg])
    v$CityMPKW[!mpg] = as.numeric(tbl$City[!mpg])
    v$HiwyMPKW[!mpg] = as.numeric(tbl$Hiwy[!mpg])

    tmp1 = strsplit(tbl$City[tbl$Hybrid], " / ")
    tmp2 = strsplit(tbl$Hiwy[tbl$Hybrid], " / ")
    
    v$CityMPG[hy] = as.numeric(sapply(tmp1, `[`, 2))
    v$HiwyMPKW[hy] = as.numeric(sapply(tmp1, `[`, 1))
    
    
#    tbl$cityMPG = fixMileage(tbl$City)
#    tbl$cityMPKW = fixMileage(tbl$City)    
    write.csv(ff)
}
plot(density(tbl$Weight))
plot(tbl$Weight, tbl$"Green Score")
