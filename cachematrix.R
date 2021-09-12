# Programming assignemnt 2
# with this assignemnt, I want to store a computed R object, in this case a matrix, as a cache. 
# This cache will be available as long the current R session is loaded.
# To cache the matrix, its necessary to set up two functions. 

# Just to clarify: what do I excpect as an output from this assignment? 
# test_matrix <- matrix(1:4,2,2)
# test_matrix
# solve(test_matrix)      ## this is, what the cached matrix should look like in the end


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