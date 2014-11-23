## Put comments here that give an overall description of what your
## functions do

## This function creates an object containing a matrix the user passes as an 
## argument, and also defines functions that allow the user to change the 
## matrix, or to cache the inverse of the chosen matrix by applying the second function
## function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ##when called, "set()" defines x to be a new
                x <<- y         ##matrix, and resets m to NULL in the environment
                m <<- NULL      ##outside the function.
        }
        get <- function() x     
        setinverse <- function(inverse) m <<- inverse 
        ##can be called by another function. When it sets m to the value supplied as an argument in the other function, 
        ## m is available to the other function even though it was defined in this function.
        getinverse <- function() m      ##returns m when called in another function.
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
                message("getting cached data")  ##tells the user that m was cached, not solved again.
                return(m)       ## returns cached m if it is available.
        }
        data <- x$get()         ## if m is NULL, this line retrieves the specified matrix from the first function
        m <- solve(data, ...)   ## inverts the matrix and stores it to m using the built-in SOLVE() function
        x$setinverse(m)         ## calls the set inverse function which writes m to the cache
        m                       ## prints the inverse
}
