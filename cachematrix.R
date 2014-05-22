## Create a set a matrix inverse cache by calling makeCacheMatrix it wraps the matrix in a cache
## subsequent calls to cacheSolve will either call the solve(..) method to generate an inverse matrix
## or return a previous copy if the matrix in the cache is unchanged

## create a cached matrix 

makeCacheMatrix <- function(x = matrix()) {

## initialise the cache copy of inverse matrix to be empty
        inverseMatrix <- NULL

## set the underlying matrix
## this will clear the inverse in cache
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }

## get the underlying matrix
        get <- function() x

## set the inverse
        setInverseMatrix <- function(m) inverseMatrix <<- m

## get the inverse
        getInverseMatrix <- function() inverseMatrix

## retain in a list
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## solve the inverse of the matrix, getting the cached inverse if it is available

cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        # if a cached version exists return it
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }

        # otherwise calculate the inverse matrix, store & return it
        data <- x$get()
        m <- solve(data)
        x$setInverseMatrix(m)
        return(m)
}
