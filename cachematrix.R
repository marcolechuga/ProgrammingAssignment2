## R Programming for Data Science Week 2
## - Caching the inverse of a matrix -


## Creates a list containing functions
## to set and get both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y){
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinv = setinv, getinv = getinv)
}


## Returns the inverse of the matrix created with makeCacheMatrix,
## if it already exists skips calculation and retrieves from cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
