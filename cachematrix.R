

## The first function makeCacheMatrix creates a special matrix object...

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    } # Set the value of the matrix
    get <- function() x # Get the value of the matrix
    setinverse<- function(inverse) inver <<-inverse  # Set the value of the inverse
    getinverse <- function() inver # Get the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## ...while the second calculates its inverse. In case it has been already calculated, it will look for it
## in the cache, rather than calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
   
    if (!is.null(inver)) { # If the inverse already exists, retrieve it
        message("Returning the cached inverse matrix")
        return(inver)
    } 
    
    else {
        inver <- solve(x$get()) # If not, calculate it
        x$setinverse(inver)
        return(inver)
    }
}
