## calculate a correlation between variables 

## path + import all files at once 
## use complete - function to calculate all complete cases of all files and return the data frame
## if ... else 
  # if the number of complete files is greater than the threshold - select all monitor IDs where this comes true 
  # and calculate correlation between sulfate and nitrate: return a vector with these correlations of these monitors 
  # else - in case no number of complete cases for any file is greater than the threshold: return numeric vector of length 0

corr <- function(directory, threshold = 0)  { ## zero as default value
  #set path 
  setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists")
  wd <- getwd()
  path <- setwd(paste(wd, c("/"),directory, sep= "")) 
  filename <- dir() ## list all the files within specdata
  storedcsv <- list(filename)
  x <- length(dir())
  # import all files, save only complete cases 
  datalist <- lapply(storedcsv[[1]][1:x],read.csv)  ## length(dir()) dynamically imports all files from first to last
  monitors_data <-  do.call(rbind,datalist)
  completerows <- monitors_data[complete.cases(monitors_data[,1:4]),]
  
  #create an empty data frame, because for every id selected, we need to add an new row - within the for loop
  rm(df_id_nobs)  # to make sure, only the 332 files are included, otherwise the for loops will always add up +332 files
  df_id_nobs <- data.frame(id=numeric(0), nobs=numeric(0))

  for (i in 1:x){
    nobs <- nrow(completerows[completerows$ID ==i,])
    df_id_nobs <- rbind(df_id_nobs, data.frame(id = i, nobs = nobs ))
  }
  #df_id_nobs
  
  check <- df_id_nobs[df_id_nobs$nobs > threshold,]
  #check
  
 
  if (nrow(check) == 0){
    vector()
      }  
  else {
        #put all ids into a vector, so that these ids can be subsetted from the original complete data set 
        monitor_ids <- check$id
        
        # create empty data frame, so that all correlation values are stored 
        rm(df_corr_output)
        df_corr_output <- data.frame(cor_value = numeric(0))
        
        for (i in monitor_ids){
                  #subset a new dataframe for each monitor
                  df_prepcorr <- completerows[completerows$ID == i,]
                  #calculate correlation between sulfate and nitrate 
                  cor_value <- cor(df_prepcorr$sulfate, df_prepcorr$nitrate)
                  df_corr_output <- rbind(df_corr_output, data.frame(cor_value))
              }
        df_corr_output$cor_value
      }
  }
