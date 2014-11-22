#
# cachematrix: functions to create and cache the inverse of a matrix
#
# Author: apyle@github.com
#
# Usage:
#       > my_matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
#       > my_matrix
#               [,1] [,2]
#       [1,]    1    3
#       [2,]    2    4
#
#       > my_cachematrix <- makeCacheMatrix(my_matrix)
#       > my_cachematrix$get()
#               [,1] [,2]
#       [1,]    1    3
#       [2,]    2    4
#
#       > my_cachematrix$getinverse()
#       NULL
#       > cacheSolve(my_cachematrix)
#               [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
#       > my_cachematrix$getinverse()
#               [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
#

# 
# makeCacheMatrix - caches the inverse of a square matrix
#
# Args: 
#       x: a matrix, assumed to be square and inversible
#
# Returns:
#       The matrix which can cache its inverse
#
# Functions:
#       get/set: assign and retrieve the matrix
#       getinverse: retrieve the inverse once calculated by cacheSolve.
#               NULL otherwise
#       setinverse: should only be called by cacheSolve and not clients
#

makeCacheMatrix <- function(x = matrix()) {

        # locally scoped variable. Note that if this is assigned a non-NULL 
        # value the cached inverse matrix will no longer be accessible to
        # calling functions
        i <- NULL

        # matrix getter and setter functions
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        # inverse matrix getter and setter functions
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        # expose list of makeCacheMatrix functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
} # makeCacheMatrix


#
# cacheSolve - computes the inverse of a square makeCacheMatrix
#
# Args: 
#       x: a makeCacheMatrix, assumed to be square and inversible
#
# Returns:
#       The inverse matrix of the matrix 'x'
#

cacheSolve <- function(x, ...) {
        
        # Check if the inverse has already been calculated and return it
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # otherwise compute the inverse, cache it in x, and return the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
} # cacheSolve
