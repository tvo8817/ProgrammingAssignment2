## The following functions compute the inverse of a square matrix.

## This function creates a special "matrix", which is really a list
## containing functions to set the matix, get the matrix, set the
## inverse of a square matrix, and get the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function(solve)
        m <<- solve
    getinverse <- function()
        m
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function calculates the inverse of the special "matrix" created
## with the above function. It first checks to see if the mean has already
## been calculated. If so, it gets the mean from the cache and skips the
## computation.  Otherwise, it calculates the inverse of the matrix and
## sets the value of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
