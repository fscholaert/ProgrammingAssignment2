## This file contains two different functions: 
## 'makeCacheMatrix' which can create a special 'matrix' object that can store its inverse,
## and 'cacheSolve' which can be invoked on a 'matrix' created via the makeCacheMatrix function and returns its (possibly cached) inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  
  # inverse will contain the inverse of our matrix. It's initialised to NULL.
  inverse <- NULL
  
  # the set function re-initialise the matrix with the argument of the set function, 
  # and also re-initialises its inverse to NULL.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # the get functon simply returns the original matrix
  get <- function() x
  
  
  # this function will be called by cacheSolve() in order to store for the first time the inverse of the matrix.
  setinverse <- function(solve) inverse <<- solve
  
  
  # this function will be called by cacheSolve() in order to retrieve the value of the inverse in case it has already been calculated before.
  getinverse <- function() inverse
  
  # a list of the internal functions of makeCacheMatrix()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function can be called on a special matrix object created via makeCacheMatrix.
## It will return the inverse of the original matrix. 
## If it already has been invoked before on the same object, it will return the cached value, otherwise it will calculate it via the 'solve' function
cacheSolve <- function(x, ...) {

  # retrieve the possibly already stored inverse of the original matrix
  inverse <- x$getinverse() 

  # if it is not 'NULL' (the inverse has been calculated before), then simply return the value
  if(!is.null(inverse)) { 
    message("getting cached data") 
    return (inverse)
  }
  

  # the value of 'inverse' was NULL, so it needs to be calculated first:

  # first obtain the original matrix via the get function of makeCacheMatrix,
  data <- x$get()

  # then calculate it's inverse via the 'solve' function,
  inverse <- solve(data, ...)

  # then invoke the 'setinverse' function of the makeCacheMatrix to store it in it's own 'inverse' variable
  x$setinverse(inverse)  

  # lastly return the inverse value that we have calculated in this function
  inverse
    
}
