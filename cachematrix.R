## Calculate and cache the inverse of a matrix assuming that the matrix supplied by user is always invertible

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv<-function(solve) inv <<- solve
        getinv<-function() inv
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## Compute the inverse of the matrix object returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
               
        ## If the inverse has already been calculated
        ## then retrieve the inverse from the cache.
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)

        inv
}
