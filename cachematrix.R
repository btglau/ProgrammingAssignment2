## these pair of functions handle inverting a matrix, with
## the additional functionality of being able to refer to 
## old results if the inverse has already been calculated

## this function returns a list functions that operate on 
## the matrix x to:
## set a value of the matrix
## get the matrix
## and set/get the inverse of the matrix (but not actually solve for it)
makeCacheMatrix <- function(x = matrix()) {
    ## create the inverse matrix variable
    inv <- NULL
    
    ## define functions for this matrix object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        inv <<- inv
    }
    getinv <- function() {
        inv
    }
    
    ## return a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## this function takes the list object returned from the previous
## function and calculates the inverse, but uses a previously calculated
## value if one is already available
cacheSolve <- function(x, ...) {
    ## get the inverse from the matrix object x
    inv <- x$getinv()
    if(!is.null(inv)) {
        ## if the inverse already exists, just return it
        message("getting cached inverse")
        return(inv)
    }
    ## if not, get the matrix and invert it, then update the
    ## matrix object with the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
