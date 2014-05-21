## Matrix inversion can be a costly computation and thus it is useful to
## cache the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix  <- NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMatrix <<- inverse
        getinverse <- function() invMatrix
        l <<- list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        invMatrix  <- l$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- l$get()
        invMatrix <- solve(data,...)
        l$setinverse(invMatrix)
        invMatrix
}
