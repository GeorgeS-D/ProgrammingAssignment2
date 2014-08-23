# (matrix) -> matrix
# Precondition: the matrix must be square.
# cacheSolve() returns the inverse of its argument.
# Using makeCacheMatrix(), it caches potentially time-consuming computations.
# > a <- makeCacheMatrix(matrix(c(7,0,-3, 2,3,4, 1,-1,-2), 3, 3))
# > cacheSolve(a)
#      [,1] [,2] [,3]
# [1,]   -2    8   -5
# [2,]    3  -11    7
# [3,]    9  -34   21
# > cacheSolve(a)
# getting cached data
#      [,1] [,2] [,3]
# [1,]   -2    8   -5
# [2,]    3  -11    7
# [3,]    9  -34   21

# Set things up for cacheSolve by:
#       intializing 'm' to "NULL",
#       creating the 3 functions it uses, and
#       creating a list object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated and the
# matrix has not changed, then cacheSolve retrieves the inverse from the cache,
# notifies the user it is getting cached data, and returns it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
