## Assignment2
## Compute and Cache the inverse of a matrix:
## function makeCacheMatrix -- cache inverse
## function cacheSolve -- compute the inverse or retrieve the inverse from the cache
## 

## This function, creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL           # init : the inverse of the matrix 'x'
    
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

###
# Usage :
#
# > source("cachematrix.R")
# > m <- rbind(c(1, -1/4), c(-1/4, 1)) # an invertible square matrix
# > cm <- makeCacheMatrix(m)           # create a cache matrix
# > im <- cacheSolve(cm)               # inverse matrix : compute/retrieve the inverse from the cache
# > im
# > im %*% m                           # check the inverse
###
