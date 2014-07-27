## Functions compute an inverse matrix and save results in cache.
## When the inverse matrix is needed, result is read from the cach to avoid
## costly computation.  

## makeCacheMatrix defines a set of functions to create 
## an inverse matrix and save result to cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  ## set x with the new matrix y and clears inverse matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                           ## return original matrix
    setinverse <- function(solve) inv <<- solve   ## set inversive matrix
    getinverse <- function() inv                  ## return inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## cacheSolve uses functions defined in makeCacheMatrix to return the 
## reverse matrix of X. If it was calculated before and stored in cache,
## then result is read from cache and is not calculated again.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()              ## request inverse matrix 
    if(!is.null(inv)) {                ## check if inverse matrix is stored in cache   
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                    ## if inverse matrix is not stored in cache, calculte invcerse matrix    
    inv <- solve(data, ...)
    x$setinverse(inv)                  ## save inverse matrix in cache    
    inv
}
