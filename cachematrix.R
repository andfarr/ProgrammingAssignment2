## matrix inversion is a costly computation so there is benefit
## to caching the inverse of a matrix as opposed to repeatedly calculating it
## this is a pair of functions that cache the inverse of a matrix

## x is an invertible square matrix
## containing functions
##    1. set matrix
##    2. get matix
##    3. set inverse
##    4. get inverse
## used as input for cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invert) m <<- invert
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## returns inverse of matrix input in makeCacheMatrix()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
