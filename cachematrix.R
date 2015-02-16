## These functions calculate the inverse of a matrix using the 'solve' function
## and cache the value for future use

## 'makeCacheMatrix' creates a special vector using a source matrix which
## contains four functions to get and set the inverse of a matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## 'cacheSolve' takes the special vector from 'makeCacheMatrix' and either
## returns the cached inverse if stored, or calculates and caches the inverse 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
