## These functions create an object that can contain a matrix and also its inverse, so you can calculate the 
## inverse once, cache it, and retrieve it again later. This only works on invertable matrices. If you give it
## a non-square matrix it will fail loudly.

## Makes a special matrix object that can store the matrix and its own inverse. It does not set the inverse
## until the inverse is asked for using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  
  # If there's already something cached, get rid of it
  myCache <- NULL
  
  # Set the value of the matrix, and since we're doing a new matrix, clear any cached inverse
  set <- function(y) {
    x <<- y
    myCache <<- NULL
  }
  
  # Retrieve the value of the matrix. So all this does is return x from when we made the matrix.
  get <- function () x
  
  #Set the inverse matrix
  setInverse <- function(theInverse) myCache <<- theInverse
  
  # Retrieve the inverse matrix
  getInverse <- function() myCache
  
  # Make a list to retrieve stuff from in other functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes an object made with makeCacheMatrix and returns the inverse from cache if it exists.
## If it does not exist, it calculates the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  #Is there something cached? If so, return it.
    if(!is.null(m)) {
      message("Getting cached data...")
      return(m)
    }
  
    # If there's nothing cached, calculate the inverse, cache it, and return it.
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    return(m)
}
