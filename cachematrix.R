## Used together, the functions allow for the inverse computation of a 
## matrix to be cached. Matrix inversion can take a long time for large
## matrices, so an ability to cache computed inversions will save the
## extra computation if the inversion is called again.

## This function accepts a matrix as input, and returns a special matrix 
## object, which is a list containing 4 get/set functions to allow
## calling functions access to the internal matrix, x, and its inverse, i.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the special matrix object, and returns the inverse
## of the matrix. If the inverse has already been calculated and cached,
## the cached value is returned. Otherwise, the inverse is calculated
## and then cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
