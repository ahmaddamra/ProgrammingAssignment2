## The following set of functions create a specialized matrix 
## where the inverse is pre-calculated and cached along with the matrix. 
## This provides the ability to speed up the process of returning
## the inverse of the matrix

## makeCacheMatrix creates a matrix x
## It defines the standard operations:
##    set(y)    assigns a matrix y to x
##    get()     returns the matrix stored in x
## It also creates a variable to store the inverse variable s.
## It defines operations to manipulate the inverse variable s:
##    setsolve(slve)   sets the value of the inverse variable s
##    getsolve()       returns the value stored in the inverse variable s
##
## Example usage: x <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(slve) s <<- slve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a function that returns the inverse value of a cached matrix x
## if will check if the inverse value has been cached then it returns it along with
## a message that the value is cached
## otherwise, it will calculate the inverse, store it in the cached matrix,
## and return the calculated inverse
## The first call to the function will calculate and cache the value
## subsequent calls will return the cached value
##
## Example usage: cacheSolve(x)


cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        ## Return a matrix that is the inverse of 'x'
        s
}
