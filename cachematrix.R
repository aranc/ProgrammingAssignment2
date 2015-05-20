## The following functions allow to compute the inverse of a matrix
## With the additional benefit of caching the result

## makeCacheMatrix returns a new "cached-inverse" matrix object
## Use it with cacheSolve to compute the inverse of matrices
## with cached results
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve computes the inverse of a matrix and cache the result
## can be called only with matrix objects returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
