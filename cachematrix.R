## This script includes two funtions that computes an inverse of a matrix using caching

## makeCacheMatrix creates a list based on a invertible matrix with function 'set', 'get', 'setsolve' and 'getsolve', used to cache the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  # preallocation and function decleration
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  # return output list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve() computes the inverse of the matrix/list of function returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
 
  # get current value of 'm' from list
  m <- x$getsolve()
  
  # if 'm' is cached already, then return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if 'm' is not already cached, then calculate inverse of matrix, cache/set and return it
  data <- x$get()
  m <- solve(data, ...) # compute inverse matrix
  x$setsolve(m)
  m 
}
