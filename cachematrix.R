## Two functions to demonstrate lexical scoping rules via cached objects

##  makeCacheMatrix:  Creates a cache for a matrix and and its computed inverse, along 
##  with get() and set() functions for each.  Uses scoped assignment to maintain state across
##  closures. Does not compute the inverse automatically; it relies on the caller for that.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##  cacheSolve:  A quick function on an (already-invoked) cached matrix that
##    A.) Checks if the inverse has already been calcuated, returning it (exiting) if so 
##    B.) If not:
##      1.)  Calculate the inverse and set it in the cache
##      2.)  Return the inverse when the function began (i.e., NULL, else would never have
##           arrived here)
##   NOTE: the function will return NULL on first execution even though the inverse was 
##   calculated and cached! This is per requirements.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inv
}


