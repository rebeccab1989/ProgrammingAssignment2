## These functions store a matrix. Then they cache the inverse of the matrix.
##

## Here's a matrix that's cacheable.

makeCacheMatrix <- function(x = matrix()) {
        inverse_1 <- NULL
        set <- function(y) {
                x <<- y
                inverse_1 <<- NULL
        }
        get <- function() x
        setmyinverse <- function(inverse) inverse_1 <<- inverse
        getmyinverse <- function() inverse_1
        list(get=get,
             set=set,
             setmyinverse = setmyinverse,
             getmyinverse = getmyinverse)
}



## Cache the inverse of matrix that I made above.

cacheSolve <- function(x, ...) {
        inverse_2 <- x$getmyinverse()
        if (!is.null(inverse_2)) {
                message("getting cached data")
                return(inverse_2)
        }
        mat <- x$get()
        inverse_2 <- solve(mat, ...)
        x$setmyinverse(inverse_2)
        inverse_2
}
