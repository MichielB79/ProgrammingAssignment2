## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions cache the inverse of a matrix.

## The first function, "makeCacheMatrix", creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 3. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function, "cacheSolve", returns the inverse of the matrix. If a computation
## has already been done, it returns the result from the cache and skips the computation. If not,
## the function will compute the inverse and will set the value in the cache (using the setinverse function)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv       
}