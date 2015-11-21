## The makeCacheMatrix and cacheSolve functions allow the user to calculate the inverse of a matrix and store it (cache).
## This is beneficial when the matrix values remain the same and the inverse has to calculated several times. 
## The inverse can be obtained from the cache instead of re-calculating everytime


## The makeCacheMatrix stores the relevant matrix and the inverse, if it is available (shows NULL if not). 
## It does not calculate inverse of the matrix but stores the inverse calculated using the cacheSolve function or as defined by the user
## If a new matrix is introduced, it clears inverse and returns it as NULL 
## The matrix and the inverse can also be viewed using this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function is used identify the inverse of the matrix stored in the makeCacheMatrix.
## It recalls the inverse if it is available in makeCacheMatrix, if not it calculates the inverse for the matrix in makeCacheMatrix.
## If the inverse is calculated, it is then cached in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
