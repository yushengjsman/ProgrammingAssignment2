## The two functions below are almost the same with the two functions 
## in "Example: Caching the Mean of a Vector" of the introduction page.

## makeCacheMatrix creates a list of functions correspond to a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    

}


## cacheSolve: compute the inverse of a matrix. If the inverse has already
## been calculated, the cached data will be returned ,otherwise use solve()
## to calculate the inverse and put it into the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m    
}
