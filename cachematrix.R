## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix data type which 
## has its inverse stored in cache. You can
## use its fuctions:
## set (set the given matrix),
## get (returns the current matrix), 
## set.inverse (set the given matrix as cached inverse),
## get.inverse (returns the cached inverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    set.inverse <- function(inv) {
        inverse <<- inv
    }
    get.inverse <- function() {
        inverse
    }
    list(set = set, 
         get = get, 
         set.inverse = set.inverse, 
         get.inverse = get.inverse)
}


## Returns the inverse of the given matrix,
## if the inverse is already cached in the 
## data it is returned, otherwise the inverse
## is computed and stored in cache

cacheSolve <- function(matrix, ...) {
    inverse <- matrix$get.inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(matrix$get(), ...)
    matrix$set.inverse(inverse)
    inverse
}
