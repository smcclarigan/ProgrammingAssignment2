## Homework Assignemnt 2 - Lexical Scoping


## This function, makeCacheMatrix creates a special "vector", which is really a list containing functions to
##  -- set the value of the matrix
##  -- get the value of the matrix
##  -- set the value of the inverse
##  -- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function, cacheSolve, gets the inverse of a matrix passed to it, but checks first to see
##  if the mean is already in memory.  If so, it skips the computation and returns value from cache.  
##  If not, it calculates.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(m)
  m
}
