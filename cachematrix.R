## These functions allow calculating the inverse 
## of any invertible square matrix. The values are cached
## so that subsequent calculations on the same matrix will
## be stored in memory, reducing the computation time
##
## The functions makeCacheMatrix and cacheSolve can
## be used together as follows:

##     myMatrix <- c=rbind(c(1, -1/4), c(-1/4, 1))
##     myCacheMatrix <- makeCacheMatrix(myMatrix)
##     myInverse <- cacheSolve(myCacheMatrix)

## In this example, myMatrix is the matrix whose inverse is needed. 
## The first call to cacheSolve will calculate the inverse of the matrix. 
## The value is now stored in the cache, so subsequent calls to cacheSolve
## with the same matrix will fetch the value from the cache rather than
## computing the inverse again.


## makeCacheMatrix() creates a matrix with functions 
## to get and set its inverse.
## The function takes one argument, x, which must
## be a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    #function to set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # function to get the matrix
    get <- function() x
    
    # function to set the inverse
    setinverse <- function(solve) inverse <<- solve
    
    # function to get the inverse
    getinverse <- function() inverse
    
    # return these functions in a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will calculate the inverse of a square, 
## invertible matrix. 
## Calculated inverse values will be stored in a cache
## so subsequent calls will fetch the value
## from the cache rather than computing it again.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    ## See if the value is in the cache. If so, return it. 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    #If value is not in cache, calculate it and store it.
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
