
## Object to store cached matrix inverse so it can be called from cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL  # reset cache           
        }
        
        # get current input matrix
        get <- function() inputMatrix
        
        # methods to return cached inverse of matrix
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        
        list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the makeCacheMatrix object if not already present in cache

cacheSolve <- function(x, ...) {
        cachedInverse <- x$getInverse() # cached matrix inverse 
                
        # if cached matrix and inverse aren't null AND input matrix equals cachedMatrix then return cached inverse
        if(!is.null(cachedInverse)) {
                message("Inverse of matrix in cache")
                return(cachedInverse)
        } 

        # inverse not cached, calculate inverse on input matrix and set on cache object
        message("Calculating inverse of matrix...")
        inputMatrix <- x$get() # current input matrix 
        cachedInverse <- solve(inputMatrix) # compute inverse
        x$setInverse(cachedInverse)
        cachedInverse
}
