## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is an object enabling the storage
## of the matrix inverse together with a matrix object. 
## Upon construction, or setting the matrix value the
## inverse is set to NULL. Calls to setinverse enable
## calling programs to store or update the cached 
## matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMat <- NULL
  
  set <- function(y) {
    mat <<- y
    inverseMat <<- NULL
  }  
  
  get <- function()  mat
  setinverse <- function(y) inverseMat <<- y
  getinverse <- function() inverseMat
  
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
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}