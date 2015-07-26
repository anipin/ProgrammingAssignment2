## makeCacheMatrix() funtion creates a matrix vector
## it has 4 functions set(), get(), setinv() and getinv()
## get() function returns the matrix in the main function
## set() funtion updates the matrix with the argument passed
## getinv() function returns the inverse of the matrix 
## setinv() function sets the inverse with the argument passed

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set= set, get = get, setinv = setinv, getinv = getinv)

}


## CacheSolve() function checks if the inverse of the matrix exists, using the getinv() function
## If the inverse already exists, it returns the inverse
## If the inverse does not exist, it calculates the inverse using solve() function

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message(" getting inverse from cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
