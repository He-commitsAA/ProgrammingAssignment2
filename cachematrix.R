## These two functions create and invert matrices, returning the inverse
## if the inverse has already been calculated.

## Makes matrices and can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                setVals <- function(y){
                        x <<- y
                        i <<- NULL
                }
                getVals <- function() x
                setInv <- function(inv){
                        i <<- inv
                }
                getInv <- function() i
                list(setVals = setVals, getVals = getVals, setInv = setInv,
                     getInv = getInv)
}


## Calculates or returns the inverse of a matrix depending on if 
## the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        if(!is.null(i)){
                message("getting cached inverse...")
                return(i)
        }
        data <- x$getVals()
        i <- solve(data,...)
        x$setInv(i)
        i
}
