#----------------------------------------
# week one and two
#----------------------------------------

# 5 atomic classes of objects
vector () # building an empty vector

# numbers: 
# L for integer, NaN for not a nomber, Inf is a number = infinity and can be calculated with 

# attributes of R Objects: names, dimnames, dimensions, class, length , other 
attributes()

x <- matrix(1:6, 2,3)
x[1,2]
x <- 1:4
y <-6:9
rep(10,4)
class(x <- c(4, "a", TRUE))
x <- list(2, "a", "b", TRUE)
x[[2]] 
x <- c(3, 5, 1, 10, 12, 6)
x[x %in% 1:5]


setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists/Test/DataScience")
getwd()
week1 <- read.csv("hw1_data.csv")

week1[1:2,]
nrow(week1)

week1[152:153,]
week1[47,]

good <- complete.cases(week1)
bad <- is.na(week1$Ozone)
NA_Ozone <- week1[bad,1]
length(NA_Ozone)
class(NA_Ozone)

complete_week1 <- week1[good,]
mean(complete_week1$Ozone)

complete_week1

#Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
filtered <- complete_week1[complete_week1$Ozone > 31 & complete_week1$Temp >90, ]
filtered
mean(filtered$Solar.R)

#What is the mean of "Temp" when "Month" is equal to 6? 
month <- filtered[filtered$Month == 6, ]
mean(month$Temp)

filtered2 <- complete_week1[complete_week1$Month == 6, ]
mean(filtered2$Temp)


filtered3 <- complete_week1[complete_week1$Month==5, ]
max(filtered3$Ozone)


#Frage 6 If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by the expression rbind(x, y)?



x <- c(1,3, 5) 
y <- c(3, 2, 10)

rbind(x,y)

rm(x)

x <- 2

makepower <- function(n){
  pow <- function(x){
  x^n
  }
  
  pow
}

cube <- makepower(3)
cube(3)

environment(cube)
ls(environment(cube))

rm(y)

f <- function(x) {
  y <- 2
  y^2 +g(x)
}

g <- function(x){
  x*y
}

g(2)

f(3)

args(f)
ls(environment(f))


?ls()
f <- c("a")

rm(x)

x <- 1:10

if(x > 5) {
  x <- 0
}

#### Programming assignment 1a ----

pollutantmean <- function(directory, pollutant, id){
    setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists")
       #obtaining a list of sensore files ----
    wd <- getwd() ##setting working directory 
        #ARGUMENT FOR FUNCTION 
        #directory <- c("specdata")  ## choosing the directory where the data files are stored (needs to be within the wd!) 
        #directory
        
    path <- setwd(paste(wd, c("/"),"specdata", sep= ""))  ## combining working directory and the directory of the csv files togehter
      #path
    
       # storing the file names in a list: 
    filename <- dir() ## list all the files within specdata
    storedcsv <- list(filename) ## store as list to subset for specified number
        #storedcsv
        #subsetting list of files ----
        #storedcsv_min <- storedcsv[[1]][id]
        
    # importing list of files into one dataframe  
    datalist <- lapply(storedcsv[[1]][1:332],read.csv)
    monitors_data <-  do.call(rbind,datalist)
    
    ## mean of pollutant across the files without NA
    mean(monitors_data[[pollutant]], na.rm = TRUE)
}
# end function 

pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 1:10)

  
test1 <- read.csv("001.csv")
#test2 <- read.csv(storedcsv[[1]][2])
#test3 <- read.csv(storedcsv[[1]][3]

#merge123 <-rbind(test1, test2,test3)
#head(merge123)

df <- data.frame(A=1:10, B=2:11, C=3:12)
fun1 <- function(x, column){
  max(x[,column])
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


##### Programming assignment 1b ----
complete("specdata", 30:25)

complete <- function(directory, id){
  setwd("P:/Analytik/Organisatorisches/LGR/Sonstiges/R/Data Scientists")
  #obtaining a list of sensore files ----
  files<-  list.files("specdata")
  
  selected_files <- files[id]
  
  # importing subsetted list of files into one dataframe  
  datalist <- lapply(files[[1]][id],read.csv)
  monitors_data <-  do.call(rbind,datalist)

  output <- data.frame(id = id, nobs = c())
  
  for (i in id){
  subset <- monitors_data[monitors_data$ID == id,]   #subsetting all columns with id = 1 e.g. 
  subset <- subset[complete.cases(subset[,1:4]),]   #defining all complete cases 
  nobs <- nrow(subset)
  for_subset <- data.frame(id = i, nobs)
  output <- rbind(output, for_subset)
  }
  output 
}   #endfunction 


# importing list of files into one dataframe  
datalist <- lapply(storedcsv[[1]][id],read.csv)
monitors_data <-  do.call(rbind,datalist)

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


complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
complete("specdata", 1)



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



#### Programming assignment 1c ----

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
   
a <- vector()
length(a)

#test 
source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313

cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0


cr <- corr("specdata")
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000
length(cr)
## [1] 323

## week test part 3----
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


## week 3 -----
rm(list=ls())
swirl


library(datasets)
data(iris)

mean(iris$Sepal.Length)

colMeans(iris)

data(mtcars)
?mtcars
rm(mtcars)
head(mtcars)

mean_cyl <- tapply(mtcars$mpg, mtcars$cyl, mean)
unique(mtcars$cyl)

class(with(mtcars, tapply(mpg, cyl, mean)))

apply(iris[, 1:4], 1, mean)

sapply(mtcars, cyl, mean)

mean(mtcars$mpg, mtcars$cyl)


mean_cyl <- sapply(split(mtcars$mpg, mtcars$cyl), mean)
undebug(ls)
apply(mtcars, 2, mean)

mean_cyl
class(mean_cyl)
?debug
dim(mean_cyl)

as.data.frame(mean_cyl)

mean_cyl[,1]

undebug(ls)

head(iris)
apply(iris[, 1:4], 2, mean)

tapply(mtcars$cyl, mtcars$mpg, mean)

with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)

tapply(mtcars$mpg, mtcars$cyl, mean)
debug(ls)
ls
Q

mean
apply(iris[, 1:4], 2, mean)
?mtcars


# invisible

?invisible

c <- 1:1000
invisible(c)
c

test <- function(x){
  invisible(rep(x, 100))
  
}

test(c)
traceback()
c <- 2
test(c)


### programming assignemnt 2 ----

#break down of the examples: 

#mV - environment
    ## this function mV has it own environment, within containing other functions (nested functions)
makeVector <- function(x = numeric()) {         #default empty numeric vector important - otherwise error message 
                                                # x is an function argurment 
  m <- NULL                  ## object within the function, set to NULL - can be called later on only witin mV-envirnment
  
  # containts basic behaviours: accessors / getter and mutator/ setter
  # function 1 as first element in the list - behaves as setter - setting x and m for the parent envi
  set <- function(y) {   # argurment y can be named anyhow - except for x - x is reserved for mother function / parent envi
       x <<- y                     ## save y as x in the set() function 
       m <<- NULL                  ## save NULL as m in the set() function 
       }
  # function 2 as the second element in the list -  behaves as getter 
  get <- function() x    # lexical scoping: R takes x from partent environment - formal arg of makeVector - gets the value from the parent envi 
  
  ## function 3 as thrid element in the list - behaves as setter: defines the mean for m 
  setmean <- function(mean) m <<- mean    # sets m by the calculated mean value in the partent envi // important! mean is not calculated! the function is only passed to cachemean!
  
  ##function 4 stored as fourth element in the list 
   getmean <- function() m     # lexical scoping: uses mean from parent envi
   
   #return: list stores it within the makeVector function
  list(set = set, get = get,       # naming the elements allows us to use $ later on to subset this element
       setmean = setmean,
       getmean = getmean)
}
# this function requires cachemean function to work, in makeVector no mean is calculated!! 

str(makeVector)


## caching the mean -- it requires objects from makeVector to be able to run! 
# notice - x is not the same value in cachemean 

cachemean <- function(x, ...) {
  m <- x$getmean()      ## take the m from makeVector and check its condition - should be NULL!
  
  if(!is.null(m)) {       ## it it is NULL, the function starts to calculate the mean, which will be stored in partent envi
    message("getting cached data")   ## hint - to know later on if the mean was saved or newly retrieved! 
    return(m)
  }   # else 
  data <- x$get()  ## get x  and safe it as the data 
  m <- mean(data, ...)  # calculate mean from the data 
  x$setmean(m)  ## set the mean from makeVector
  m # return the value of m to the parent envi 
}

#To summarize, the lexical scoping assignment in R Programming takes advantage of lexical scoping and the fact that functions that 
#return objects of type list() also allow access to any other objects defined in the environment of the original function. 
#In the specific instance of makeVector() this means that subsequent code can access the values of x or m through the use of getters 
#and setters. This is how cachemean() is able to calculate and store the mean for the input argument if it is of type makeVector(). 
#Because list elements in makeVector() are defined with names, we can access these functions with the $ form of the extract operator.


#### testing out 

test <- matrix(1:4,2,2)
test
solve(test)

########### breaking down assignment 2

# Programming assignemnt 2
# with this assignemnt, I want to store a computed R object, in this case a matrix, as a cache. 
# This cache will be available as long the current R session is loaded.
# To cache the matrix, its necessary to set up two functions. 

# Just to clarify: what do I excpect as an output from this assignment? 
test_matrix <- matrix(1:4,2,2)
test_matrix
solve(test_matrix)      ## this is, what the cached matrix should look like in the end


## first function

## # The main objectiv of the first function called makeCachematrix, will store the matrix that will be inversed in the second function cacheSolve. 
# Importantly, the makeCachematrix will produce a list in the global environment, each element of the list is a certain function, that will serve the cacheSolve to get and set (or process) the matrix x in the makeCachematrix.

makeCachematrix <- function(x = matrix()){
  inverse <- NULL                 ## setting my variable to NULL which is supposed to story the cache matrix later on 
  
  ## function 1: setter behaviour
  set <- function(y){
    x <<- y                      ## saving x to the parent environment
    inverse <<- NULL             ## saving the variable which shall later on story my cache matrix to the partent environment
  }
  ## function 2: getter behaviour
  get <- function() x           ## a function that takes x from the partent environemnt 
  ## function 3: setter behaviour
  setinverse <- function(i) inverse <<- i   ## assign the input argument from the parent environment, later on pass it to the cacheSolve function
  ## function 4: getter behaviour
  getinverse <- function() inverse    ## a function that takes the local variable from the parent environment 
  
  ## store these function in a list, elements are named so that cacheSolve is able to call them by name, not by position 
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
  )
}

# second function
# The second function cacheSolve produces, when called the first time in the current R session, the inverse of the matrix which was called in makeCachematrix.
# If the inverse is already calculated, the Cachematrix function will store the output and return it. So that the inverse matrix is stored in the global environment for further processing.


cacheSolve <- function(y, ...){   ## y could also be called x, just to make sure the input is not the same as in makeCachematrix
  inverse <- y$getinverse()       # take the inverse variable from the makeCachematrix to process it
  
  # check its condition 
  if(!is.null(inverse)){
    message("getting cached data")  ## if inverse variable is NOT NULL, get and return the cached matrix
    return(inverse)
  }
  else{     ## if inverse variable is NULL, then:
    data <- y$get()         ## get the matrix from makeCachematrix 
    inverse <- solve(data, ...)   ## compute its inverse 
    y$setinverse(inverse)   # set the inverse of the makeCachematrix
  }
  inverse   ## return the cached matrix
}


# feel free to test it right away and see, if my expectations are met

#storedfunc <- makeCachematrix(test_matrix)
#cachedmatrix <- cachematrix(storedfunc)
#cachedmatrix


######## week 3


set.seed(1)
rpois(5,2)
?set.seed

swirl()


### Programming assigment 3 of week 4 ------

# first: Plot the 30-day mortality rates for heart attack
getwd()
setwd("C:/Users/l.grotenrath/Documents/R/R Programming")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow (outcome)
names(outcome) 

#To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#columns of interest:
#11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
#17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
#23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
      

#2: Finding the best hospital in a state


  best <- function(state, outcome){

 # rm(state, outcome )
#state <- c("TX")
#outcome <- c("heart failure")
    
  setwd("C:/Users/l.grotenrath/Documents/R/R Programming")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # subset a dataframe only containing relevant columns 
  subset <- data[, c(2,7,11,17,23)]
  colnames(subset) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  # setting outcome columns to numeric
  col <- 3:5
  subset[, col] <- apply(subset[, col], 2, function(x){as.numeric(x)})
  
  
  # check validity of the args 
  states <- unique(data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")

  if(state %in% states) {
        state
    } else {
    stop("invalid state")}

    if(outcome %in% outcomes) {
        outcome
    } else { stop("invalid outcome")}
  
  #head(subset)
  
  # from that subset only relevant state and valid entries on the outcome
  subset<- subset[subset$state == state, ] 
  subset <- subset[subset[[outcome]] != c("Not Available"),]
  subset <- subset[!is.na(subset[[outcome]]),]
  
  #subset[,outcome] <- as.numeric(subsetstate[,outcome])
 
 # subset <- subset[complete.cases(subset[,]),]  ## only valid numbers
  
  # select hopsital(s) that have the minimum rate 
  #hospital <- subset[ subset[, outcome] == min(subset[, outcome]), 1]
  
  hospital <- subset[subset[[outcome]] == min(subset[[outcome]]),1]
  
  # check : are there more than 1 hosptials? then order alphabetically and take first hospital
  if (length(hospital) != 1){
    x <- sort(hospital)
    print(x[1])
  } else {
    print(hospital)
  }
}
  

# testing:
best("TX", "heart attack") 
best("TX", "heart failure")
best("MD", "heart attack") 
best("MD", "pneumonia") 

best("BB", "heart attack")
best("NY", "hert attack")

### 

function(state, outcome, num="best"){
  
  #state <- c("TX")
  #outcome <- c("heart failure")
  #num <- 4
  # rm(state, outcome, num)
  
  setwd("C:/Users/l.grotenrath/Documents/R/R Programming")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # subset a dataframe only containing relevant columns 
  subset <- data[, c(2,7,11,17,23)]
  colnames(subset) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  # setting outcome columns to numeric
  col <- 3:5
  subset[, col] <- apply(subset[, col], 2, function(x){as.numeric(x)})
  
  
  # check validity of the args 
  states <- unique(data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(state %in% states) {
    state
  } else {
    stop("invalid state")}
  if(outcome %in% outcomes) {
    outcome
  } else { stop("invalid outcome")}
  
  
  # from that subset only relevant state and valid entries on the outcome
  subset<- subset[subset$state == state, ] 
  subset <- subset[subset[[outcome]] != c("Not Available"),]
  subset <- subset[!is.na(subset[[outcome]]),]
  
  hospital <- subset$hospital
  rate <- subset[[outcome]]
  
  newdf <- data.frame(hospital, rate)
  newdf<-  newdf[with(newdf, order(rate, hospital)),]
  
  rank <- 1:length(hospital)
  newdf <- data.frame(hospital = newdf$hospital, rate = newdf$rate, rank)
  
  if (num == c("best")){
    num <- 1
    num <- as.numeric(num)
  } else if (num == c("worst")) {
    num <- nrow(newdf)
    num <- as.numeric(num)
  } else if(num > nrow(newdf)){
    NA
  } else {
    num <- as.numeric(num)
  }
  newdf[num, 1]
}


#test
rankhospital("TX", "heart failure", 1)
rankhospital("MD", "heart attack", "worst") ## not working
rankhospital("TX", "heart failure")  ## not working
rankhospital("MN", "heart attack", 5000)


#### part 3

 rankall <- function(outcome, num="best"){
  
#state <- c("TX", "CA", "MD", "MN", "OR")
#outcome <- c("heart failure")
#num <- 20
#rm(state, outcome, num)
  
  setwd("C:/Users/l.grotenrath/Documents/R/R Programming")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # subset a dataframe only containing relevant columns 
  subset <- data[, c(2,7,11,17,23)]
  colnames(subset) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  # setting outcome columns to numeric
  col <- 3:5
  subset[, col] <- apply(subset[, col], 2, function(x){as.numeric(x)})
  # check validity of the args 
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes) {
    outcome
  } else { stop("invalid outcome")}
  # from that subset only valid entries on the outcome
  subset <- subset[subset[[outcome]] != c("Not Available"),]
  subset <- subset[!is.na(subset[[outcome]]),]
  #create new dataframe 
  hospital <- subset$hospital
  state <- subset$state
  rate <- subset[[outcome]]
  newdf <- data.frame(hospital, rate, state)
  newdf<-  newdf[with(newdf, order(rate, hospital)),] ### order data frame according to rate and hospital
  # for each state:
  list <- split(newdf, newdf$state)
  
  rm(output)
  output <- data.frame(hospital = character(0), state = character(0))
  
 for (i in unique(newdf$state)){
   df <- list[[i]]              ## store the element of the list as separate df 
   df <- df[with(df, order(rate,hospital)),]
   rank <- 1:nrow(df)
   df <- data.frame(df, rank = rank)
   
  #num <- 5 
  #nrow(df)
   
   # get only the hospital of num 
   
   if (num == "best"){
     hospital <- df[rank == min(df[,4]), 1]
   } else if (num == "worst"){
     hospital<- df[rank == max(df[,4]), 1]
   } else if(num > nrow(df)) {## ranking of state is smaller than the rank called 
    hospital <- NA
   } else if (num < nrow(df)){
     hospital <- df[rank == num, 1]
   }
   
   output_i <- data.frame(hospital, state = i)
    output <- rbind(output, output_i) 
   }
   output <- output[with(output, order(state, hospital)),]
 } 
 
 # endfunction 
rm(hospital)

## test 
head(rankall("heart attack", 20),10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

#### Programming Assignment test
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
