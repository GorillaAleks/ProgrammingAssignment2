## makeCacheMatrix caches the inverse of a matrix. cacheSolve calculates it.

## makeCacheMatrix creates a list, one element of which can cache the inverse
## of an input matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## the function has default values: one empty matrix, the inverse of 
        ## which is 'NULL'
        inv <- NULL
        
        ## set allows the user to set a new input matrix. Its default inverse
        ## is 'NULL'
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get returns the input matrix
        get <- function() x
        
        ## setinverse caches the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ## getinverse returns the inverse
        getinverse <- function() inv
        
        ## the functions above are stored in a list, which is returned by
        ## makeCacheMatrix. Its elements have the same names as the function. 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve returns the cached inverse, if available. Otherwise, it calculates
## it, caches it and returns the result.

cacheSolve <- function(x, ...) {
        
        ## The function firstly checks whether there is a cached inverse. In 
        ## that case it returns the value. 
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached inverse...")
                return(inv)
        }
        
        ## Here, the inverse is being calculated.
        data <- x$get()
        
        inv <- solve(data, ...)
        
        ## This caches the inverse
        x$setinverse(inv)
        
        ## The result is returned.
        inv
}
