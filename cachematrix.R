## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

# hold cached value / NULL if nothing is cached  
  cache <- NULL

# store matrix
  setMat <- function(y) { 
    x <<- y # matrix has new value
    cache <<- NULL # flush the cache
  }

# get stored matrix
  getMat <- function() x

# cache given value
  setInv <- function(solve) cache <<- solve

# get cached value
  getInv <- function() cache

# return list of functions
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}

##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function (x, ...) {

  # get cached value
  inv <- x$getInv()
  
# check if it exists. If so return it
  if(!is.null(inv)) {
    message('Getting cached data.')
    return(inv)
  }
  
# If not, get it...
  mat <- x$getMat()

# ... calculate its inverse...
  inv <- solve(mat, ...)

# and cache it
  x$setInv(inv)
  
# finally, return inverse
  inv
}
