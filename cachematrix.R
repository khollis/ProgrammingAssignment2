## Below are the following functions:
##   1.makeCacheMatrix will create a special "matrix" object that can cache its inverse.
##   2.cacheSolve will compute the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## This function will create a the matrix that can cache its inverse

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


# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
     inv 
}

