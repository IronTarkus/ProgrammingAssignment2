## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" containing a list with four functions
## that will set the value of the matrix(function(y),line 11)
## and get the value of the matrix(line 15),
## set the value of the inverse(line 16) and
## get the value of the inverse of said matrix(line 17)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.If
## the inverse has already been calculcated(and the matrix hasn't changed), then the
## function will retrieve the inverse from the cache.
## Solve is a function that returns the inverse of a matrix(line 35)
## setinverse and getinverse will, respectively, set the value of the inverse and get the value of the inverse regarding x
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}        ## Return a matrix that is the inverse of 'x'
