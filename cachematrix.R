# makeCacheMatrix creates a list of functions to do the following:
#                   1. set the value of the matrix
#                   2. get the value of the matrix
#                   3. set the value of inverse of the matrix
#                   4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # Check to make sure the input is a valid matrix
    if (!is.matrix(x)){
        message("Input Parameter needs to be a matrix")
    }
    inv <- NULL
    set <- function(y) {
        # Saves the matrix and initializes the cache globally
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    # Retutn the list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This  function applies the solve() function to invert an inputted square matrix
#  It first checks the cache to see if the computation has already been done
#  If the cache is available it will skip the calculation and return the cached value.
# If the cache is not available it will compute the inverse uisng the solve() function


# This function assumes that the matrix is always invertible (square).
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x = rbind(c(2, -3/4), c(-3/4, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  2.00 -0.75
## [2,] -0.75  2.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
##   [1,] 0.5818182 0.2181818
##   [2,] 0.2181818 0.5818182

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
##   [1,] 0.5818182 0.2181818
##   [2,] 0.2181818 0.5818182
## >