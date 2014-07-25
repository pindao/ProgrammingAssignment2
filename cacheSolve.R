## This function calcaulates the inverse of the matrix returned by function 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()                      ## assign the value of inverse to
                                               ##  matrix i
        if(!is.null(i)) {                      ## return the inverse from the  
                                               ## cache if the inverse 
                                               ## has already been calculated
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)                  ## calcaulate the value of the 
                                               ## inverse and assign it to 
                                               ## matrix i
        x$setsolve(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
