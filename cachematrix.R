## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can store a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() {x}
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() {i}

        list(
                set = set,
                get = get,
                getinverse = getinverse,
                setinverse = setinverse
        )
}

## This function caches the inverse of a matrix if it has been inferred before. oterwise it infers it from scratch.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }

        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
