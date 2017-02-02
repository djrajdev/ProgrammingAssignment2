## @Author: DJ Rajdev
## @Purpose: Coursera Programming Assignment2
## Using "<<-" operator to cache inverse of a matrix

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set matrix data and inverse=NULL in another env
  set <- function(y) { 
    x <<- y 
    inverse <<- NULL
  }
  get <- function() x
  # setter for Inverse
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function() inverse
  # Return getter-and setter functions for data, Inverse
  list(set= set, get= get, setInverse= setInverse, getInverse= getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  # Get the inverse
  inverse <- x$getInverse()
  # if not null return inverse
  if(!is.null(inverse)) {
    message('Getting cached inverse')
    return(inverse)
  }
  # code reaches here only if inverse is null, then get data, solve for inverse
  data <- x$get()
  #print(data)
  inverse <- solve(data,...)
  # Set the inverse and return value
  x$setInverse(inverse)
  inverse
}
