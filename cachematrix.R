## These functions create a matrix object that can cache its inverse
## The first call to getinv() after any change to the matrix data will
## call the (expensive) solve function.  Subsequent calls to getinv 
## without any intervening changes to the data will return a cached result


## This function creates the matrix and returns a list of operator functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the cached inverse of the matrix (if available), 
## or the calculated inverse of the matrix (if a cached value is unavailable)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  data <- x$get()
  if ((!is.null(m))&&(data==x)) {
    return (m)
  }
  m <- solve(x)
  x$setinv(m)
  m
}
