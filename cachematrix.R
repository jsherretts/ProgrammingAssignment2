## Programming Assignment 2

## The following functions work to cache the inverse of a martix in order to 
## avoid repeat computation

## makeCacheMatrix is a fucntion that creates a special "matrix" object that 
## can cache its inverse. The following function that sets the value of a 
## special  "matrix, gets that value, sets the inverse of that matrix and gets 
## the  inverse of the matrix

makeCacheMatrix <- function(x = matrix) {
    i   <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        set_invrs   <- function(invrs) i <<- invrs
        get_invrs   <- function(i) 
            list(set = set, get = get, 
                 set_invrs = set_invrs, 
                 get_invrs = get_invrs)
}

##c acheSolve is a function that is designed to cache the matrix created by the 
## previous function makeCacheMatrix. The cacheSolve function should retreive
## the inverse of the special matrix from the cache if the inverse has already
## been calculated

cacheSolve <- function(x, ...) {
    i <- x$get_invrs()
        if(!is.null(i)) {
            message("getting cached matrix")
            return(i)
        }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
}



