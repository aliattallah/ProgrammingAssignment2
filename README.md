##This function is desired to get the matrix and inverse of the matrix
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## cache the inverse of the matrix
## this will be done through "makeCacheMatrix" and "cacheSolve
makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Retun a matrix that is the inverse of X

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
