## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Set an empty inverse matrix
    ## Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## Get the matrix
    setinv <- function(inverse) inv <<- solve ## Set the inverse matrix
    getinv <- function() inv ## Get the inverse matrix
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## Return cached data if the inverse matrix is already calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get() ## Get the value of matrix
    inv <- solve(data, ...) ## Calculate the inverse
    x$setinv(inv) ## Set the inverse matrix and cache it
    inv ## Return the inverse matrix
}
