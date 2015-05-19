# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# Below are two functions that cache the inverse of a matrix.

# The function, makeCacheMatrix creates a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse of the matrix
# 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# The function returns the inverse of the metrix created with the above function
# It first checks to see if the inverse of the matrix has already been created
# If so, it returns the result from the cache and skips the calculation
# If not, it calculates the inverse of the matrix and
#            sets the value in the cache via the setinv function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
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

## sample run:
## > a <- matrix(c(1, 3, 2, 4), 2, 2)
## > x <- makeCacheMatrix(a)
## > x$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## Not from the cache for the first run
## > cacheSolve(x)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## retrieve from the cache for the second run
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
