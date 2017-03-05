## The following functions solve the inverse of a matrix 
## and caches the inverse for future use

## makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## returning the matrix, setting the matrix, returning its inverse and setting its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getinv <- function() inverse
    setinv <- function(inv) inverse <<- inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks if the inverse is stored in cache.
## if not it solves the inverse, stores it in cache and returns
## otherwise, it returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)){
        return(inverse)
    }
    inverse <- solve(x$get())
    x$setinv(inverse)
    inverse
}
