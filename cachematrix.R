## Caching the Inverse of a Matrix
## The following 2 functions are used to cache the inverse of a matrix.

## This function creates a special 'matrix' object, that can cache its inverse.
## It creates a special 'matrix' object, 'x', that is a list containing a function:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special 'matirx' returned by the function makeCacheMatrix (above).
## It checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise it computes the inverse and sets the value in the cache via the 'setinverse' function.
## It assumes the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
