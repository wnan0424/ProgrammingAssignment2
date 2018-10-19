# Matrix inversion is usually a costly computation and there may 
# be some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly.

## gengerate a matrix object caching its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        bb <<- list(set = set, get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)

        
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
