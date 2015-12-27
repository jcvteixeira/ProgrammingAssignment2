## Cache the inverse of a matrix
## Adapted from the "cachemean" example
##
## Author: Joao Teixeira (jcvteixeira@gmail.com)
## Date: 2015-12-27
##
## Example calls:
## m=matrix(...)
## cm=makeCacheMatrix(m)
## m_inv=cacheSolve(cm)


## Create an object to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the inverse matrix:
## - first call: compute and store the inverse
## - other calls: return the cached data

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
