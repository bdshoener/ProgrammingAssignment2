## Taken together, these functions can be used to 
## cache and subsequently solve the inverse of a matrix.

## This function can be used to make and retrieve a matrix object.
## This function is also used by the second function when determining
## whether or not to calculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    Set <- function(y) {
        x   <<- y
        mat <<- NULL
    }
    SetMatrix <- function(m)
        mat <<- m
    Get <- function()
        x
    GetMatrix <- function()
        mat
    list(set = Set,
        get = Get,
        setmatrix = SetMatrix,
        getmatrix = GetMatrix)
}


## This function takes the matrix created by the function above, 
## calculates the inverse, and caches that value.
## If the inverse has already been calculated and is currently 
## cached, the function retrieves the cached value.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if (is.null(m)) {
        data <- x$get()
        m    <- solve(data, ...)
        x$setmatrix(m)
    } else {
        message('using cached value')
    }
    m
    ## Return a matrix that is the inverse of 'x'
}
