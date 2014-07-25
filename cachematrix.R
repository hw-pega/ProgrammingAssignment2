## Provide functions to calculate matrix inversion
## Will cache the result when do 1st time calculation
## And just return the cached result when it's called next time to improve performance



## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {

        ## m is the object to store cache
        m <- NULL
        
        ## if set new matrix, reset m (chahe)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) m <<- inverse
        getsolve <- function() m
        
        ## create list contains functions of this object
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
                
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get cache from x, and if not null, return cache
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## inverse the matrix from x, and cache the result
        data <- x$get()        
        m <- solve(data, ...)
        x$setsolve(m)          
        m                        
}
