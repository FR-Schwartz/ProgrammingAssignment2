## These function perform matrix inversion via cacheing

## This function creates matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() {invmat}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        invmat <- x$getinverse
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        matrix <- x$get()
        invmat <- solve(matrix, ...)
        x$setinverse(invmat)
        invmat
}
