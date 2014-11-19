## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
