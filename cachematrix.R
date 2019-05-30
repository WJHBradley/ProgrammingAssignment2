## These functions store the inverse of a matrix in cache 
## 

## This function takes an input matrix for use with the cacheSolve function.
## It returns a list of four functions, which is required for input into cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      setMatrix <- function(y){
        x <<- y
        inv <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(solve) inv <<- solve
      getInverse <- function() inv
      list(setMatrix = setMatrix, getMatrix = getMatrix, 
           setInverse = setInverse, getInverse = getInverse)
}


## This function will cache and return an inverse of the matrix 'x'
## Similar to cachemean, it checks to see if the inverse is cached before
## solving and returning the inverse, inv.

## Also, the input 'x' must be a list output from makeCacheMatrix
## in order to function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        Mdata <- x$getMatrix()
        inv <- solve(Mdata,...)
        x$setInverse(inv)
        inv
      
}
