## the function below initialises the value of x and m. Then the values of x and m are set and retrieved.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setm <- function(inv) m <<- inv
        getm <- function() m
        list(set = set, get = get,
             setm = setm,
             getm = getm)
}


## below function checks if m is null, then returns a matrix that is the inverse of 'x' using the above function.

cacheSolve <- function(x, ...) {
        m <-x$getm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setm(m)
        m
}