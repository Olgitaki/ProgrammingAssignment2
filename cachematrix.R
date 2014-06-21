
## Below we create a special "matrix" object that can cache its inverse of the passed matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  ##start with a null
    set <- function(y){ ##define the set function
        x <<- y
        m <<- NULL
    }
    
    get <- function() x ##get back the x
    
    setinverse <- function(solve) m <<- solve ##inverse is used to compute the inverse
    getinverse <- function() m
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if( !is.null(m) ){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    m ##returns the inverse of the matrix
}



