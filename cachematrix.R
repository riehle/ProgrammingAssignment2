## makeCacheMatrix is an object enabling the storage
## of the matrix inverse paired with a matrix object. 
##
## Upon construction or setting of the matrix value the
## inverse is initialized to NULL. Calls to setinverse 
## enable calling programs to store or update the cached 
## matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
    inverse_mat <- NULL
  
    # set mat and re-initialize inverse
    set <- function(y) {
        mat <<- y
        inverse_mat <<- NULL
    }  
  
    # getter for mat
    get <- function()  mat

    # getter/setter for inverse
    setinverse <- function(y) inverse_mat <<- y
    getinverse <- function() inverse_mat
  
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns the matrix inverse of an object
## created with makeCacheMatrix. The results are cached
## in the cacheMatrix object to speed repeated calls
## for the inverse operation.
cacheSolve <- function(x, ...) {

        ## check for cached inverse

        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }

        # if not found, compute the inverse and cache it

        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
