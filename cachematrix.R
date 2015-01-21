#www.github.com/davidkrauza/ProgrammingAssingment2
#cacheMatrix.R
#Purpose:
#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of 
#the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#Create makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Create cacheSolve function
## Checks to see if the inverse has already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
