## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() and cacheSolve() can be used to solve a matrix
## returning the inverse and can cache this inverse

## Write a short comment describing this function
## makeCacheMatrix returns a list given a matrix object that can
## store the original matrix and its inverse using set and setsolve.
## It can return the inverse and the original using get and getsolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function() m <<- solve(x)
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function

## cacheSolve checks is the given special matrix object
## has a value for the inverse cached, if so it returns the
## inverse matrix from memory, else it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}