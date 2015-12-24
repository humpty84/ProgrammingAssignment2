## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix which is a list of functions to first set the value of the matrix, get the value of the matrix, 
# set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## cacheSolve calculates the inverse of the matrix set by makeCacheMatrix function. It first checks to see if the inverse of the matrix has
# been calculated. If it already has, it uses the cached value, instead of re calculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
  }
