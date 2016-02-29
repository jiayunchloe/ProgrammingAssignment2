## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create an object that stores a matrix and caches its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Below is the function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated, it should retrieve the inverse from the cache.

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
