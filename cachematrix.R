## Allows a matrix to be inverted then cached to potentially increase speed
## when recalling the inverted matrix. makeCacheMatrix gets and sets the
## matrix and its inverse, while cacheSolve checks if the inverse has beem
## computed already. If it has, it returns the cached value, and if it has not
## it computes the inverse then caches it.

## Create a "matrix" object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the "matrix" created by makeCacheMatrix. 
## If inverse was already calculated and the matrix did not change, 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
