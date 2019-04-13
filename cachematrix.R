## makeCacheMatrix creates the basic paramters for using the cacheSolve function
## cacheSolve checks for a cached inverse and the generates it if its not there

##  Read in matrix type and set basic terminology used in calls
##  Sets value of matrix
##  Gets value of matrix
##  Sets value of inverse
##  Gets value of inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Use Solve function and check for a cache of the solve on the matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
