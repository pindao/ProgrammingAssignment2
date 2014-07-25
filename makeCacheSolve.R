## This function creates a matrix which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {      ## create the function and 
                                                 ## assign its value
        i <- NULL                                ## the original value of 
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
