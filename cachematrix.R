## Functions will calculate the inverse of a square matrix and cache the value
## assuming no changes have been made to underlying matrix

## makeCacheMatrix accepts a square matrix, defines the inner functions
## and returns a list to the inner functions

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() {
          x
     }
     
     setinverse <- function(inverse) {
          m <<- inverse
     }
     
     getinverse <- function(){ 
          m
     }
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}
     

## cacheSolve calculates the inverse of a square matrix;
## if the inverse of the matrix has been calculated, it returns the cached matrix,
## otherwise it calculates and returns the inverse matrix

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
