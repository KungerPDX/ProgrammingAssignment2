## Two functions to demonstrate lexical scoping rules via cached objects

##  makeCacheMatrix:  Creates a cache for a matrix and and its computed inverse, along 
##  with get() and set() functions for each.  Uses scoped assignment to maintain state across
##  closures. Does not compute the inverse automatically; it relies on the caller for that.

makeCacheMatrix <- function(x = matrix()) {
        
        # Lexically scoped inverse variable
        inv <- NULL
        
        # Set function for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get function for the matrix
        get <- function() x
        
        # Set function for the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get function for the inverse
        getinverse <- function() inv
        
        # List function
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
        
        # Inverse for the cached matrix
        inv <- x$getinverse()
        
        # If inverse is set, return it along with a message that it was in the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, return the data, calculate the inverse, set it in the cache, and
        # inverse variable from function entry
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inv
}


