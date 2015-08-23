##
## This R file contains the definitions for two functions, makeCacheMatrix and cacheSolve.
## The makeCacheMatrix function creates an object used to cache a square matrix and its inverse.
## The cacheSolve function returns an inverse matrix, cacheing if necessary.
##


##
## makeCacheMatrix(x=matrix()) - x is a square matrix (optional argument)
## 
## Description - Creates an object used to cache a square matrix and its inverse.
##    If a square matrix is passed as an argument, it is cached in the object.
##    The object includes four functions:
##       get() - returns the cached square matrix
##       set(y) - caches y, a square matrix
##       getinverse() - returns the cached inverse matrix
##       set(inverse) - caches the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {

        ## Check that x is a valid square matrix
        ## Since the Project guaranteed a solvable matrix, this isn't strictly necessary
        if (!(is.matrix(x) && (nrow(x) == ncol(x)))) {
                message ("Warning - Argument must be a square matrix, Using default value instead")
                x = matrix()
        }

        ## Initilize the inverse matrix to NULL
        i <- NULL

        ## Create the Set function
        set <- function(y) {

                ## Check that y is a valid square matrix
                if (!(is.matrix(y) && (nrow(y) == ncol(y)))) {
                      message ("Warning - Argument must be a square matrix, Using default value instead")
                      x = matrix()
                }

                x <<- y
                i <<- NULL
        }

        ## Create the Get function
        get <- function() x

        ## Create the SetInverse function
        setinverse <- function(inverse) i <<- inverse

        ## Create the GetInverse function
        getinverse <- function() i

        ## Return the object
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##
## cacheSolve(x,...) - x is square matrix, ... additional arguments passd to solve
## 
## Description - Takes a square matrix, returns the inverted matrix, cacheing if necessary.
##
cacheSolve <- function(x, ...) {

        ## Get the cached inverse matrix
        ## If it is NOT NULL, then the matrix hasn't changed and the cached inverse value is returned
        i <- x$getinverse()
        if (!is.null(i)) {
                message("Returning cached inverse matrix")
                return(i)
        }


        ## Otherwise retrieve the cached matrix and generate (solve)/cache the Inverse matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)

        ## Return the inverse
        i
}
