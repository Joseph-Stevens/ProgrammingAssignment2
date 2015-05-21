## Use a matrix as input with the function makeCacheMatrix. 
## Assign the output to the function cacheSolve. If this is the first time,
## the inverse of the matrix is caculated, cached and returned. Afterwards,
## the cached result is returned with a message.
## 

## Returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Checks for an already calculated inverse. If there isn't one (null), then
## the inverse gets calulated and returned. If the inverse is there, the cached
## result is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}