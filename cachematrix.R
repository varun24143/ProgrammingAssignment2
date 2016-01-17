# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
# invert will store the cached inverse matrix
    invert <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinverse <- function(inverse) invert <<- inverse
    # Getter for the inverse
    getinvverse <- function() invert

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: It computes the inverse of matrix. If the inverse is calculated before, it will return the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invert <- x$getinverse()
    if(!is.null(invert)) {
        message("getting cached data.")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data)
    x$setinverse(invert)
    invert
}
