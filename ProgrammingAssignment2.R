## To Cache the Inverse of a Matrix(square):
## Matrix inversion is usually a tedious calculation and it may be useful 
## to cache the inverse of a matrix rather than calculate it repeatedly.
## Here is a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.



## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set=set, get=get,
          setInverse= setInverse,
          getInverse= getInverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse already exists (and the 
## matrix has'nt changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x,...) {
   m <- x$getInverse()
   if(!is.null(m)) {
        message("getting cached data")
        return(m)
   }
   data <- x$get()
   m <- solve(data,...)
   x$setInverse(m)
   m
}
