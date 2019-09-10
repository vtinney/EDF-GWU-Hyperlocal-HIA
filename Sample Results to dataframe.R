library(raster)
library(rgdal)
library(glue)
library(dplyr)
library(plyr)
library(Rcpp)

# List all the directories with raster files for which you want to get cell statistics

dir <- c('/GWSPH/home/vtinney/results2/no2/all.cause/af/',
         '/GWSPH/home/vtinney/results2/no2/all.cause/paf/',
         '/GWSPH/home/vtinney/results2/no2/all.cause/mr/',
         '/GWSPH/home/vtinney/results2/no2/cvd/af/',
         '/GWSPH/home/vtinney/results2/no2/cvd/paf/',
         '/GWSPH/home/vtinney/results2/no2/cvd/mr/',
         '/GWSPH/home/vtinney/results2/no2/er/af/',
         '/GWSPH/home/vtinney/results2/no2/er/paf/',
         '/GWSPH/home/vtinney/results2/no2/er/mr/')

# This loop extracts the sum, mean, min, max and IQR for each raster
# and save it to a new csv file in the directory you specify

for (i in 1:length(dir)){
  print(dir[i])
  setwd(dir[i])
  files <- list.files(pattern = "\\.tif*", full.names=TRUE)
  for (k in 1:length(files)){
    print(files[k])
    p <- raster(paste(files[k]))
    p[p == 0] <- NA
    p.iqr <- quantile(p)
    p.iqr <- as.matrix(p.iqr)
    p.iqr <- t(p.iqr)
    p.sum <- cellStats(p, 'sum')
    p.mean <- cellStats(p, 'mean')
    paf <- cbind(p.sum, p.mean, p.iqr)
    fout = paste0('/GWSPH/home/vtinney/results2/no2/df/',files[k],'.csv',sep='')
    write.csv(paf, fout)
    rm(p)
  }}

# Now merge all of these dataframes into one large dataframe that can be 
# easily made into tables.

setwd('/GWSPH/home/vtinney/results2/no2/df/')
filenames <- list.files(path='/GWSPH/home/vtinney/results2/no2/df/',pattern="*.csv", full.names=TRUE)
dataset.names <- do.call("rbind",llply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")})) #get header names
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
d <- cbind(dataset, dataset.names)
write.csv(d, "no2.results.csv")