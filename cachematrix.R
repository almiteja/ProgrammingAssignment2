## Put comments here that give an overall description of what your
## functions do

## This function creates an object containing a matrix the user passes as an 
## argument, and also defines functions that allow the user to change the 
## matrix, or to cache the inverse of the chosen matrix by applying the following
## function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ##when called, "set()" defines x to be a new
                x <<- y         ##matrix, and resets m to NULL in the environment
                m <<- NULL      ##outside the function.
        }
        get <- function() x     
        setinverse <- function(inverse) m <<- inverse ##can be called outside
        getinverse <- function() m                      ##the function to fo
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Once makeCacheMatrix has been called for a specific matrix and creates an 
## object containing the subject matrix, cachSolve retrieves the inverse of 
## the matrix, either as a cached value, or as a newly calculated matrix, depending
## on whether the inverse is already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##will set m to the inverse, if the inverse is cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)       ## returns cached m if it is available.
        }
        data <- x$get()         ## retrieves the specified matrix from the list created above
        m <- solve(data, ...)   ## inverts the matrix and stores it to m
        x$setinverse(m)         ## calls the set inverse function which writes m to the cache
        m                       ## prints the inverse
}
