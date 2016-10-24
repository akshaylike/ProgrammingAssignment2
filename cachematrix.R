## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
