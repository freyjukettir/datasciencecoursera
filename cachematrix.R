#  A demonstration of the use of lexical scoping per "Programming Assignment 2."
#
#  The first function, makeCacheMatrix, creates a special "matrix", actually a 
#  list, the elements of which are four functions, to wit:
#    set_matrix:  sets the matrix provided as the argument to makeCacheMatrix()
#    get_matrix:  retrieves the the value of the argument
#    set_inverse: computes and sets the inverse of the argument
#    get_inverse: retrieves the the value of the inverse of the argument
#
#  Usage:  makeCacheMatrix(argument - must be of class "matrix")
#
#   Example with random matrix: 
#       > cache_test <- makeCacheMatrix(matrix(sample(1:100, 16), c(4,4)))
#
#   Examples with specified matrix:
#       1: > cache_test <- makeCacheMatrix(matrix(c(2, 1, 0, 4), c(2, 2)))
#       2: > mtx <- matrix(c(2, 1, 0, 4), c(2, 2))
#          > cache_test <- makeCacheMatrix(mtx) 
#
# 

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set_matrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inv) i <<- inv
    get_inverse <- function() i
    list(
        set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}

#  The second function, cacheSolve(), first checks for the existence of a
#  cached inverse of the the argument. 
#     If the cached inverse exists, the previously computed inverse is returned.
#     If the cached inverse does not exist, the inverse is computed, returned, 
#           and cached.
#
#   Usage: cacheSolve(x), wherein 'x' is the variable to which the result of 
#           makeCacheMatrix(matrix) above is assigned.
#
#     Example: cacheSolve(cache_test)


cacheSolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("Here's one I made earlier!")
        return(i)
    }
    m <- x$get_matrix()
    i <- solve(m, ...)
    x$set_inverse(i)
    i
}
