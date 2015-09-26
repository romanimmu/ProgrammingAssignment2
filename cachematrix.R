## Caching a matrix saves time when it comes to working with the same data again
## and again. Instead of computing in this example the inverse of a matrix for
## each time the inverse is needed, it is stored and used whenever the same 
## matrix is needed again.

## ----------------------------------------------------------------------------

## makeCacheMatrix provides a list that contains functions to set the value of 
# the matrix, get the value of the matrix, set the value of the inverse of the
## matrix and get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inver <<- inverse
        getinv <- function() inver
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## ----------------------------------------------------------------------------

## cacheSolve inverts the matrix. First, it checks if the matrix provided has
## already been computed. If this is the case it loads the stored result and
## does not compute the matrix again. If the matrix has not been compued before
## the inverse of the matrix is computed.

cacheSolve <- function(x, ...) {
        inver <- x$getinv()
        if(!is.null(inver)) {  # Check, whether matrix was already calculated
                print("Loading data from cache.") # if so, data is loaded and
                return(inver) # printed after notion that its loaded from cache
        }
        data <- x$get() # If inverse had not been calculated previously it is
        inver <- solve(data)    # done here
        x$setinv(inver) # and values are set in cache
        inver           # Output of calculated matrix.
}

## ----------------------------------------------------------------------------
## Test run of function

## Creating matrix and running makeCacheMatrix
## z <- rbind(c(1, -5/8), c(-1/4, 1)) 
## p <- makeCacheMatrix(z)
## p$get()
##       [,1]   [,2]
## [1,]  1.00 -0.625
## [2,] -0.25  1.000


## CacheSolve, computing in first run
## cacheSolve(p)
##      [,1]      [,2]
## [1,] 1.1851852 0.7407407
## [2,] 0.2962963 1.1851852

## CacheSolve, second run -> getting data from cache
## cacheSolve(p)
## [1] "Loading data from cache."
##      [,1]      [,2]
## [1,] 1.1851852 0.7407407
## [2,] 0.2962963 1.1851852

