## These two functions take a pre-existing matrix, return and store its inverse, and then
## return a cached (instead of re-computed) inverse of the matrix if it is 
## called for by the second function more than once.  As indicated by the instructor,
## these two functions require the pre-existing matrix they receive as an input
## is invertible.

## The function makeCacheMatrix takes an invertible pre-existing matrix as its input and 
## returns a list of four functions that will be used by the function cacheSolve to 
## determine the matrix's inverse.  Once cacheSolve returns an inverse for a particular
## matrix, that inverse is cached within makeCacheMatrix (lines 21-23) and will be 
## called/returned by cacheSolve if the object supplied in cacheSolve has already been
## passed through makeCacheMatrix and cacheSolve, previously.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(cachedmatrix) {
                m <<- cachedmatrix
        }
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The function cacheSolve takes as an argument a list object produced by makeCacheMatrix
## and then queries line 24 of makeCacheMatrix to find a cached inverse for the matrix
## originally taken as an input in makeCacheMatrix.  If the cached value is NULL (empty),
## cacheSolve then executes line 20 of makeCacheMatrix to pull into cacheSolve the matrix
## originally fed to makeCacheMatrix.  cacheSolve then returns the inverse of the matrix
## and passes it back to makeCacheMatrix as a stored value within the setmatrix function
## (line 21) of makeCacheMatrix, which will be called/returned if the same list is passed
## to cacheSolve more than once.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix,...)
        x$setmatrix(m)
        m
}
