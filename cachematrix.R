## Calling makeCacheMatrix on a matrix 'x' creates a special cacheMatrix object 
## that can cache its inverse.
## Calling cacheSolve on this cacheMatrix object then returns the inverse of 'x'
## If the inverse has already been computed, it is simply read from the cache.
## Else, the inverse is computed and stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
        ## Takes a matrix 'x' as input, returning a list of 4 functions
        ## that can cache 'x' and its inverse.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list('set' = set, 'get' = get,
             'setinv' = setinv,
             'getinv' = getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If inverse is stored in cache, returns that without computing again
        ## If not, computes the inverse and stores it in the cache
        inv <- x$getinv()
        if (!is.null(inv)){
                message("accessing cached inverse...")
                return(inv)
        }
        matrix_ori <- x$get()
        inv <- solve(matrix_ori, ...)
        x$setinv(inv)
        inv
}