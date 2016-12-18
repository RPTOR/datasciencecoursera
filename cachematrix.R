## Original makeVector functions for reference and improve
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# Original cachemean function for reference and improve
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#
# Function: makeCacheMatrix 
# Date: 18 December 2016
# Revision: v0.1
#
# Arguments:
# x - Matrix to initialize
#
# Returns:
# Object
#
# Description:-
#
# Which is based on the prior MakeVector function. 
# Basically it initialize an object using a matrix argument and caches the matrices data within
# New object will consist of get(), set(), setinverse() and getinverse() functions
#
# (New makeCacheMatrix which is based on the prior MakeVector function)
#

makeCacheMatrix <- function(x = matrix()) {
  
  # Description and initialization of  variables 
  # 'origindata' to store original data (matrices)
  # 'inversedata' to store inversed data (matrices)
  
  origindata <- x # Origin data from the passing argument
  inversedata <- NULL # We dont have any inverse data yet, so null it goes !
  
  # 'Set' function - assignment of passing argument 'argdata' into the origindata variable
  set <- function(argdata) {
    origindata <<- argdata
    
    # Resetting the 'inversedata' variable to NULL as the 'origindata' had been initialized
    inversedata <<- NULL
  }
  
  # 'Get' function - returning of 'origindata' variable
  get <- function() origindata
  
  # 'SetInverse' function - Basically setting values into the 'inversedata' variable
  setinverse <- function(argdata) inversedata <<- argdata
  
  # 'GetInverse' function - Returning the inversed data
  getinverse <- function() inversedata
  
  # 'List' - listing of all available functions within the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#
# Function: CacheSolve 
# Date: 18 December 2016
# Revision: v0.1
#
# Arguments:
# x - Object created from makeCacheMatrix function
#
# Returns:
# Matrix - Inversed matrix
#
# Description:-
# 
# Basically it takes in the object which were created by the makeCacheMatrix function
# Checks if the inversedata had been populated (using getinverse() function)
# If it does, returns the inversedata (Returns as cached data - no need to recalculate)
# If it doesn't, calculates it using the SOLVE function and returns the data
#
# (New CacheSolve which is based on the prior CacheMean function)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Obtain the matrix by calling the getinverse function from makeCacheMatrix. 
  inv <- x$getinverse() 
  
  # Test out if there had been data inversed from prior calls (Returning a valid matrix)
  if(!is.null(inv)) {
    message("These data had been cached")
    return(inv)
  }
  
  message("These data had been generated using SOLVE function")
  # Okay, so we inverse the data and store it back using the function setinverse()
  # Obtain the original data and store into a variable tempdata
  tempdata <- x$get()
  
  # Calling solve function as instructed
  inv <- solve(tempdata)
  
  # Set the data as inverse
  x$setinverse(inv)
  
  # Returns the data set
  inv
}

