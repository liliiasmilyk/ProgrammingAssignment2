## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## My function creates matrix, that is inverse to matrix "x"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        com_in <- function(solve) m <<- solve
        sh_in <- function() m
        list(set = set, get = get,
             com_in = com_in,
             sh_in = sh_in)     
}


## Write a short comment describing this function
## cacheSolve checkes is there inverted matrix to "x". If yes, it returns result from cache,
## if no - it computes the inverse of  matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$sh_in()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$com_in(m)
        m
}
