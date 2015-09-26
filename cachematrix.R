## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse


## makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to cache the input to inverse.

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL 
  setmatrix <- function(y) { 
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse,
         getinverse = getinverse)

}
## cacheSolve 
calculates the inverse matrix for a cacheMatrix object only if it has not cached it yet and it also returns the inverse.

cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m) 
    m  ## Return a matrix that is the inverse of 'x'
}
