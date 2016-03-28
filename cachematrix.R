## Put comments here that give an overall description of what your
## functions do
## The following functions find the inverse of a given matrix, cache it, and either return the cahced inverse matrix on a subsequent call or calclulate an inverse matrix if it hadn't already done previously


## Write a short comment describing this function: this function caches inverse results

makeCacheMatrix <- function(x = matrix()) {
  thisInv <- NULL
  set <- function(y) {
    x <<- y
    thisInv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) thisInv <<- solve
  getinv <- function() thisInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function: this function checks and returns an inverse matrix of input
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  thisInv <- x$getinv()
  if(!is.null(thisInv)) {
    message("getting cached data")
    return(thisInv)
  }
  data <- x$get()
  thisInv <- solve(data, ...)
  x$setinv(thisInv)
  thisInv
}