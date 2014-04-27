##########################################
## This file contains two functions:
##
## The first function, "makeCacheMatrix" creates a special "matrix", which is really a list containing a function to 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse
##
## The second function, "cacheSolve" calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
###########################################

## Creates an object storing a matrix and its inverse (with getters and setters for the matrix itself and for its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # when there is a new matrix, set it and erase the old inverse cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get x
  get <- function() x
  
  # set the inverse 
  setinverse <- function(inverse) i <<- inverse
  
  # get the inverse
  getinverse <- function() i
  
  # really necessary? probably assigning the functions to x?
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Returns the inverse of x (data should be set already). Uses cache if the inverse has already been computed,
## computes inverse and saves it to cache prior to returning it if the cache has been empty.
cacheSolve <- function(x, ...) {
  
  ## Get cache into i
  i <- x$getinverse()
  
  ## If the cache is empty...
  if(!is.null(i)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x'
    return(i)
  }
  
  ## Else get the data for inverse computation, compute it and save to cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
