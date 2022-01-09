#### Programming assignment 1 - part b 

complete <- function(directory, id) {
  # set path, list all possible files // needs to be done for every monitor ID so doesnt need to be within the for loop
  setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists")
  wd <- getwd()
  path <- setwd(paste(wd, c("/"),directory, sep= "")) 
  filename <- dir() ## list all the files within specdata
  storedcsv <- list(filename)
  
  #create an empty data frame, because for every id selected, we need to add an new row - within the for loop
  output <- data.frame(id=numeric(0), nobs=numeric(0))
  
  ## for each id selected = i 
  for(i in id){
    
    # select a single file and import 
    file <- storedcsv[[1]][i]   # select only one file and import 
    data <- read.csv(file)
    #retrieve only complete cases 
    completerows <- data[complete.cases(data[,1:4]),]
    # count rows
    nobs <- nrow(completerows)
    # lastly, rbind each result to the empty data frame - needs to be done otherwise the for loop will always overwrite 
    # the previous result and would end up only showing the last "i"
    output <- rbind(output, data.frame(id = i, nobs = nobs ))
  }
  output
}

## testing 
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
complete("specdata", 3)
#complete("specdata", 1)


## week test part 2 ----
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])










