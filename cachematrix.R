#The first function, makeCacheMatrix creates a special "matrix", which is
#really a list containing a function to
#       - set the value of the matrix
#       - get the value of the matrix
#       - set the value of the inverse of the matrix
#       - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) xInv <<- Inverse
        getInverse <- function() xInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#The following function calculates the inverse of the special "matrix" created
#with the above function. However, it first checks to see if the mean has
#already been calculated. If so, it gets the mean from the cache and skips the
#computation. Otherwise, it calculates the mean of the data and sets the value
#of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        xInv <- x$getInverse()
        if(!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        data <- x$get()
        xInv <- solve(data, ...)
        print(xInv)
        x$setInverse(xInv)
        xInv
}