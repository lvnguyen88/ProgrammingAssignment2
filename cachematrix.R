## Put comments here that give an overall description of what your
## functions do

##This function is to create a cacheable matrix to input values

makeCacheMatrix <- function(x = matrix()) {
inverse.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() x
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


#This function computes the inverse of the cacheable matrix from function makeCacheMatrix(). If the inverse has already been calculated there will be no change.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted.matrix <- x$get.inverse()
  if(!is.null(inverted.matrix)){
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
  
  matrix.to.inverse <- x$get()
  inverted.matrix <- solve(matrix.to.inverse)
  x$set.inverse(inverted.matrix)
  inverted.matrix
}
