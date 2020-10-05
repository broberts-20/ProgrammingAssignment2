## The following functions allow the user to cache the inverse of a matrix. This 
## ensures the inverse will not be computed repeatedly. It is assumed that the matrix
## is invertible. 

## this function creates a "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <-function(inverseMat) inv <<- inverseMat
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function computes the inverse of the "matrix" object returned by makeCacheMatrix.
## If the inverse has been previously calculated, and the matrix has not changed, this function
## will retrieve the inverse from its cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
