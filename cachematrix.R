## The two functions return and cache the inverse of an input square matrix. If 
## the input matrix has not changed, i.e., the inverse has been already 
## calculated, the cached value of the inverse will be output.

## This function creates a matrix which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {      ## create the function
        i <- NULL                                ## set the original value of 
                                                 ## matrix i
        set <- function(y) {                     ## set the value of the input 
                                                 ## matrix
                x <<- y
                i <<- NULL                       ## reset matrix i which 
                                                 ## carries the inverse
        }
        get <- function() x                      ## get the value of the input 
                                                 ## matrix
        setsolve <- function(solve) i <<- solve  ## set the value of the inverse
        getsolve <- function() i                 ## get the value of the inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calcaulates the inverse of the matrix returned by function 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()                        ## assign the value of inverse 
                                                 ## to matrix i
        if(!is.null(i)) {                        ## return the inverse from the  
                                                 ## cache if the inverse 
                                                 ## has already been calculated
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)                    ## calcaulate the value of the 
                                                 ## inverse and assign it to 
                                                 ## matrix i
        x$setsolve(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
