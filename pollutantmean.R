#### Programming assignment 1 ----

pollutantmean <- function(directory, pollutant, id){
  setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists")
  #obtaining a list of sensore files ----
  #ARGUMENT FOR FUNCTION 
  #directory <- c("specdata")  ## choosing the directory where the data files are stored (needs to be within the wd!) 
  #directory
  
  path <- setwd(paste(getwd(), c("/"),directory, sep= ""))  ## combining working directory and the directory of the csv files togehter
  #path
  
  # storing the file names in a list: 
  filename <- dir() ## list all the files within specdata
  storedcsv <- list(filename) ## store as list to subset for specified number
  #storedcsv
  #subsetting list of files ----
  #storedcsv_min <- storedcsv[[1]][id]
  
  # importing list of files into one dataframe  
  datalist <- lapply(storedcsv[[1]][id],read.csv)
  monitors_data <-  do.call(rbind,datalist)
  
  ## mean of pollutant across the files without NA
  mean(monitors_data[[pollutant]], na.rm = TRUE)
}

## test after creating this function
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)
#pollutantmean("specdata", "sulfate", 1:10)


# week test 
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
