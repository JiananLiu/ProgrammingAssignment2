## Put comments here that give an overall description of what your
## functions do

## These functions create a matrix object and return the inverse
## of this matrix in a way that
## if the inverse has already been calculated, then it 
## is retrived from the cache
## if the inverse has not been calculated, then it is computed

## Write a short comment describing this function

## The makeCacheMatrix function creates a special "matrix" object 
## and returns a list of four functions to facilitate caching the
## inverse 

makeCacheMatrix <- function(x = matrix()) {
        cachedMatrix <- NULL
        set <- function(y) {
                x <<- y
                cachedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedMatrix <<- inverse
        getInverse <- function() cachedMatrix
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

## The function cacheSolve takes the matrix created by 
## the makeCacheMatrix function above and retrives the inverse
## of this matrix from the cache and returns the inverse if it
## has already been calculated
## Otherwise it computes and returns the inverse

cacheSolve <- function(x, ...) {
       cachedMatrix <- x$getInverse()
       if(!is.null(cachedMatrix)) {
              message("getting cached data")
              return(cachedMatrix)
       }
       data <- x$get()
       cachedMatrix <- solve(data)
       x$setInverse(cachedMatrix)
       cachedMatrix
}
