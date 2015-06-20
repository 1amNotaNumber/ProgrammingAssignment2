
## This script creates a pair of functions that cache the inverse of a matrix.
## The purpose of the cache is to avoid repeated computation of the inverse
## when a matrix has not changed.


## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  x_inverse <- NULL
  set <- function (y) {
        x <<- y
        x_inverse <<- NULL
  }

  get <- function() x
  set_inverse <- function(new_inverse) x_inverse <<- new_inverse
  get_inverse <- function() x_inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$get_inverse()
  
  if(!is.null(x_inverse)){
        message("getting cached inverse")
        return(x_inverse)
  }
  
  data <- x$get()
  x_inverse <- solve(data)
  x$set_inverse(x_inverse)
  x_inverse
}
