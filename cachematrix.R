## Put comments here that give an overall description of what your
## functions do

## This function makes a cache of inverse for each matrix that is entered in the program


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
        
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## if the function has already been treated, the program just gets its inverse from the cache
## otherwise, calculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        
        ## this part from the cache
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## case if matrix having no cache, the inverse is calculated
        ## then set in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
