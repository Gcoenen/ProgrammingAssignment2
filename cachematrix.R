makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## Below are two functions that cache the inverse of a matrix (to avoid computing the inverse of the 'same matrix' releatedly).

## The 'makeCacheMatrix'-function creates a special 'matrix' object that can cache its inverse. The functions has several functionalities:
## 1) sets the matrix, 2) gets the matrix, 3) sets the inverse of the matrix and 4) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {                                    # set the value of the matrix
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x                                     # get the value of the matrix
        setInverse <- function(inverse) invMatrix <<- inverse   # set the inverse of the matrix
        getInverse <- function() invMatrix                      # get the inverse of the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                           # everything is stored in a list
}

## The 'cacheSolve'-function generates the inverse of the special 'matrix' returned by the 'makeCacheMatrix'-function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Retrieves the inverse of the matrix from the 'makeCacheMatrix'-function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {               # if the inverse already exists (i.e. is not NULL):
                message("getting cached data")  # a message is generates stating that the inverse comes from the cached data
                return(invMatrix)               # and the inverse of the matrix is returned
        }
        dataMatrix <- x$get()                   # if the inverse does not exists (i.e. is NULL):
        invMatrix <- solve(dataMatrix, ...)     # the solve-function is applied to get the inverse of the matrix
        x$setInverse(invMatrix)                 # the inverse matrix is set
        return(invMatrix)                       # and the inverse matrix is returned
}