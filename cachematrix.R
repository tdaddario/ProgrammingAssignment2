## The following pair of functions computes and cache the inverse of a matrix.

## The first function creates a special "matrix" object: list of functions returned to parent environment.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The second function computes the inverse of the list returned by makeCacheMatrix above. CacheSolve retrives cached inverse matrix if it has been already calculated and matrix is not changed.

cacheSolve <- function(x, ...) {
    
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
