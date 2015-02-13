## Put comments here that give an overall description of what your
## functions do
## create maxtrix 
## x <- matrix(c(2, 4, 3, 1, 5, 7, 8, 9, 10), nrow=3, ncol=3)
## [,1] [,2] [,3]
##    2    1    8
##    4    5    9
##    3    7   10
## solve(x)
##     [,1]        [,2]        [,3]
## -0.2  0.70769231 -0.47692308
## -0.2 -0.06153846  0.21538462
##  0.2 -0.16923077  0.09230769
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
