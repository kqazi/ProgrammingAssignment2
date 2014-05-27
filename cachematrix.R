
## Object to store cached matrix and matrix inverse so it can be called from cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        cachedMatrix <- NULL
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedMatrix <<- NULL
                cachedInverse <<- NULL             
        }
        
        # get current input matrix
        get <- function() inputMatrix
        
        # methods to return cached matrix (prior to being inverted)
        setMatrix <- function(matrix) cachedMatrix <<- matrix
        getMatrix <- function() cachedMatrix
        
        # methods to return cached inverse of matrix
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        
        list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the makeCacheMatrix object if not already present in cache

cacheSolve <- function(x, ...) {
        cachedInverse <- x$getInverse() # cached matrix inverse 
        cachedMatrix <- x$getMatrix() # cached matrix 
        inputMatrix <- x$get() # current input matrix 
                
        # if input matrix is empty there's no inverse to compute or matrix to compare to 
        # else we create an inverse
        if(is.null(inputMatrix)) {
                stop("Input matrix is null.  Nothing to do")        
        } 

        # if cached matrix and inverse aren't null AND input matrix equals cachedMatrix then return cached inverse
        if(!is.null(cachedMatrix) && !is.null(cachedInverse) && identical(inputMatrix, cachedMatrix)) {
                message("Inverse of matrix in cache and has not changed")
                return(cachedInverse)
        } 

        # inverse not cached, calculate inverse on input matrix and set on cache object
        message("Calculating inverse of matrix...")
        x$setMatrix(inputMatrix)
        cachedInverse <- solve(inputMatrix)
        x$setInverse(cachedInverse)
        cachedInverse
}
