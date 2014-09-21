## Below are two functions: makeCacheMatrix and cacheSolve, that
## are used together to create a cacheable object containing a
## matrix and its inverse.

## The makeCacheMatrix function generates a list of functions with
## its own environment, where we can keep track of an arbitrary
## matrix called originalMatrix, as well as its inverseMatrix.
## When the makeCacheMatrix is first called or when the
## originalMatrix is updated (using the $makeCacheMatrix.get() function),
## the value of inverseMatrix is set to NULL, so the cacheSolve() function
## knows it is necessary to recalculate the inverse.

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inverseMatrix <- NULL
    set <- function(m) {
        originalMatrix <<- m
        inverseMatrix <<- NULL
    }
    get <- function() originalMatrix
    setInverseMatrix <- function(m) inverseMatrix <<- m
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get, setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## The cacheSolve function uses a special list of the type generated
## by the makeCacheMatrix function as an argument.
## It first checks if the inverseMatrix was already calculated, and
## if it was, it returns the cached inverseMatrix.
## If the value of inverseMatrix is NULL, then it means that it has
## not yet been calculated, and so the function solve() is used to do that.
## After using solve(), the inverseMatrix is saved in cache using the
## x$setInverseMatrix function.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)) {
        message('Getting cached inverse matrix')
        return (inverseMatrix)
    }
    originalMatrix <- x$get()
    inverseMatrix <- solve(originalMatrix)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}