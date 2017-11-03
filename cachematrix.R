## Two functions that first cache a copy of a matrix, and when
## first executed also cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function interacts with our cahced matrix object.
## If it is a new matrix a new matrix object is created and
## its inverse calculated (and cached). If it is not a new
## matrix object then the cached inverse is recalled.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        
}

## Examples of running these functions
# Repreducible seed for 'random' matrices.
set.seed(1)

# Make a first matrix
mat1 <- makeCacheMatrix(matrix(sample(100,9),3))
# Get inverse
cacheSolve(mat1)
# Get inverse a second time (now uses cached value)
cacheSolve(mat1)


# Make a second matrix
mat1 <- makeCacheMatrix(matrix(sample(100,9),3))
# Get inverse
cacheSolve(mat1)
# Get inverse a second time (now uses cached value)
cacheSolve(mat1)
