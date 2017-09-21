## The following script provides two functions, `makeCacheMatrix` and 
## `cacheSolve`. In combination they can be used to store a matrix
## as a special **cached** form that reduces costs in the computation 
## of matrix inversion.

## Create list which maps matrix and inverse to cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set.inv <- function(solve) m <<- solve
    get.inv <- function() m
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}


## Function check whether or not the inverse solution is cached and
## returns, if not the inverse is computed with the `solve` function.

cacheSolve <- function(x, ...) {
    m <- x$get.inv()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set.inv(m)
    m
}
