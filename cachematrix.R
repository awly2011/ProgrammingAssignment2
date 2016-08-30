
## makeCacheMatrix basically provides the "setter" and "getter" function
## set function is used for clear the cache if we have new matrix
## the final output is a list, which can be as a input for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inversed_cach <- NULL
        set <- function(y) {
                x <<- y
                inversed_cach <<- NULL
        }
        get <- function() x
        setcach <- function(cach) inversed_cach <<- cach
        getcach <- function() inversed_cach
        list(set = set, get = get,
             setcach = setcach,
             getcach = getcach)

}


## cacheSolve function is used to calculate the inverse of the matrix
## "if loop" evaluates cache value first, if exsits, return the cache value directly
## if the cache value doesn't exsit, the inversed matrix will be calculated
## finally, the calcluated value saved to cache, and the result is printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed_cach <- x$getcach()
        if(!is.null(inversed_cach)) {
                message("cached matrix exsits!")
                return(inversed_cach)
        }
        matrix_data <- x$get()
        inversed_cach <- solve(matrix_data, ...)
        x$setcach(inversed_cach)
        inversed_cach
}
