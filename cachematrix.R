
## These functions solve for the inverse of a matrix.  When used in a conjunction with one another, the
## 'CacheSolve' function will print the cached matrix from the first function.  Otherwise it will solve
## the inverse matrix.


## This function solves for the inverse of a matrix and stores that value which can be recalled later
## without recalculating.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y){
                x <<- y
                inv <<- NULL
                
        }
        
        get <- function()x
        setinv <- function(solve) inv <<- solve 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
        

}


## This function recalls the stored inverse matrix if it has been calculated or solves for the inverse 
## matrix if it has not.
cacheSolve <- function(x=matrix(), ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}