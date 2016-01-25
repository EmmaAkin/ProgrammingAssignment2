## Put comments here that give an overall description of what your
## functions do

## Improve Matrix inverse computation time using Caching


## Matrix inversion is usually a costly computation. Caching the inverse
## helps reduce time of computation, especially when the inverse
## would be used often.

## Using the functions below to
## Create a special object that store the matrix
## Then cache the inverse

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)


}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by the function
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve function below would should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}

