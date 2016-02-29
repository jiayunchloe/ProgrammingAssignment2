## Matrix inversion is usually a costly computation
## Some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create an object that stores a matrix and caches its inverse.

## SHA-1 269fe6199c9f17ea0f6a45b69c59cd7c6052fe6c

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inver <- NULL
   set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Below is the function computes the inverse of the matrix created by makeCacheMatrix above. 
## If the inverse has already been calculated, it will show "getting cached data".

## SHA-1 269fe6199c9f17ea0f6a45b69c59cd7c6052fe6c

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getInverse()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    new <- x$get()
    inver <- solve(new, ...)
    x$setInverse(inver)
    inver
}
