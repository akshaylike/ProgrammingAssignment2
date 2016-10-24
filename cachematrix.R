## Caching the Inverse of a Matrix:
## Matrix inversion is usually a time consuming computation 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches the inverse.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matr <<- inverse
  getinverse <- function() matr
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that computes the inverse of the special matrix
    
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  
  if(!is.null(inv)) {
    message("Cached Matrix Data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data,...)
  x$setinverse(inv)
  return(inv)
}
