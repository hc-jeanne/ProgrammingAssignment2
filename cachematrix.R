## Cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL           # the inverse of the matrix 'x'
    
    set <- function(y) { # set the value of the matrix
        x <<- y          # the matrix has changed , then clear cache (the inverse)
        iv <<- NULL
    }

    get <- function() x  # get the value of the matrix
    setInverse <- function(inverse) iv <<- inverse  # set the value of the inverse
    getInverse <- function() iv                     # get the value of the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    iv <- x$getInverse()    # get the inverse in x (a makeCacheMatrix)
    
    # check if the inverse has calculated :
    if (!is.null(iv)) {     # the inverse has already been calculated
        message("getting cached data")
        return(iv)          # return the cached inverse
    }
    # the inverse has not been calculated yet : then compute it:
    data <- x$get()         # get the matrix
    iv <- solve(data)       # calculate the inverse of the matrix (a square matrix)
    x$setInverse(iv)        # cache the calculated inverse
    iv                      # Return the inverse (a matrix that is the inverse of 'x')
}
