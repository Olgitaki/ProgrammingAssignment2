## Working together, the following two fuctions cache the inverse of a matrix, which potentially takes long to compute.
## The first function create the initial matrix, and the second function computes its inverse.
##
## Example usage:
## > m = rbind(c(1,-0.25), c(-0.24,1))
## > cacheMatrix <- makeCacheMatrix()
## > cacheMatrix$set(m)
## > cacheSolve(cacheMatrix)
## [,1]      [,2]
## [1,] 1.0638298 0.2659574
## [2,] 0.2553191 1.0638298
## > cacheSolve(cacheMatrix)
## getting cached data
## [,1]      [,2]
## [1,] 1.0638298 0.2659574
## [2,] 0.2553191 1.0638298

## This function creates a special "matrix" object that can cache its inverse of the passed matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if( !is.null(inv) ){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
