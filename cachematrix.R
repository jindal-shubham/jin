## The 2 functions below create a special object to store a matrix and can be
## to find its inverse

## makeCacheMatrix function creates a special matrix object to store cached values

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## It finds the inverse of the matrix created by above function.
## If the inverse exists then it will get the inverse from the cache only.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}
