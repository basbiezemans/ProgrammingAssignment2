#
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly. The functions below cache the inverse of a matrix.
#
# Example:
#
# > square_matrix <- matrix(c(4,3,3,2), nrow = 2)
# 
# > square_matrix
#      [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# 
# > cache_matrix <- makeCacheMatrix(square_matrix)
# 
# > cacheSolve(cache_matrix)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# 
# > cacheSolve(cache_matrix)
# getting cached inverse matrix
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4


# This function creates a special "matrix" object that can cache itself.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_cache <- function(z) m <<- z
    get_cache <- function() m
    list(set = set, get = get, 
         set_cache = set_cache, 
         get_cache = get_cache)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then cacheSolve retrieves the inverse
# from the cache.

cacheSolve <- function(x) {
    # Return a matrix that is the inverse of 'x'
    m <- x$get_cache()
    if (!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    m <- solve(x$get())
    x$set_cache(m) # Cache m, the inverse of 'x'
    m
}
