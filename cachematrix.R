## cachematrix.R
## Implements logic to allow for the caching of the calculation of an inverse of a matrix

## Set up the functional infrastructure for a cache-able matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(my_inv) inv <<- my_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Using the cacheMatrix previously set up, check to see if the result is already stored in cache
## If so, return the cached result.  If not, compute, cache, and return a new result
##
## This function will fail with the error "$ operator is invalid for atomic vectors" if fed
## a raw matrix rather than a cache matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
