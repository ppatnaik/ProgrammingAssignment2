## This function expects a square matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## Check if matrix is a square matrix
    if(nrow(x) != ncol(x)) {
        message("function needs a square matrix")
        return (x)
    }
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## This function returns an inverse of a matrix using solve(x)
## function call. Solve is saved to a named location
## and is fetched if the function is called with the an unchanged matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mtx <- x$get()
    i <- solve(mtx)
    x$setInverse(i)
    i
}
