## Matrix inversion is usually a costly computation and it will be beneficial to cache the inverse of a matrix rather than compute it again ##and again. This assignment is to write a set of functions that cache the inverse of a matrix.

## makeCacheMatrix function do a set of operation (set matrix, get matrix, set inverse matrix, get inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the matrix. If inverse already computed then it will return the cached matrix else it will store it for ##future use.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
