## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y  
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ##print(x)
  
  ## set the value of the inverse matrix
  set.inv = function(inverse) inv <<- inverse
  
  ## get the value of the inverse matrix
  get.inv = function() inv
  list(set=set, get=get, set.inv=set.inv, get.inv=get.inv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inv()
  
  ## if inverse result is already available
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  ## else compute the inverse
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$set.inv(inv)
  return(inv)
}


