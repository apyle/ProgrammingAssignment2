## TODO: Header comments
## Put comments here that give an overall description of what your
## functions do

## TODO: function comments
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        # cached inverse matrix
        i <- NULL

        # getter and setter functions
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        # getter and setter functions for inverse matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        # expose list of functions for makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## TODO: function comments
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
