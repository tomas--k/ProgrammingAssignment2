## @TODO: Write description!

## Creates an object storing a matrix and its inverse (with getters and setters for the matrix itself)
## and for its inverse
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


## Returns the inverse of x (data should be set already). Uses cache if the inverse has already computed,
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
