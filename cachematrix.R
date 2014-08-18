## These functions create a special matrix that caches
## the calculated inverse of a matrix

## Creates a "special" matrix that can cache its inverse

makeCacheMatrix <- function(mx = matrix()) {
    
    # initialize inverse variable
    inv <- NULL
    
    # set matrix and clear inverse
    set <- function(y) { 
        mx <<- y
        inv <<- NULL
    }
    
    # get matrix
    get <- function() mx
    
    # set inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # get inverse
    getinverse <- function() inv
    
    # describe object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the "special" matrix returned by
## makeCacheMatrix.  If the inverse is already calculated (and
## the matrix has not changed), then retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## Check to see if inverse was found, if so, return
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## If not inverse was found, calculate and save
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    ## Return inverse
    inv
}
