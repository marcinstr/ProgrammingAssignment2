## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL

  setMat <- function(y) {
    x <<- y
    cache <<- NULL
  }
  getMat <- function() x
  setInv <- function(solve) cache <<- solve
  getInv <- function() cache

  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function (x, ...) {
  
  inv <- x$getInv()

  if(!is.null(inv)) {
    message('Getting cached data.')
    return(inv)
  }
  mat <- x$getMat()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
