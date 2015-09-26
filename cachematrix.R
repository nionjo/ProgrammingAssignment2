##  The following pair of functions caches the inverse of a matrix.

## makeCacheMatrix
# Creates a special "matrix", which is a list containing a function to
# -set the value of the matrix
# -get the value of the matrix
# -set the value of the inverse
# -get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) i <<- solve
  getSolve <- function() i
  list(set = set, get = get, 
       setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve
# First check to see if the inverse of the matrix has already been calculated
# If so, it gets the inverse from the cache and skips the computation 
# Otherwise, compute the inverse of the data and sets the value of the inverse 
# in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getSolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
}
