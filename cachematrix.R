## Assignment 2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object (or list) that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## defines the inverse as NULL
        inv <- NULL
        ## Sets the matrix we defined
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Gets the matrix
        get <- function() x
        ## does the same for the inverse
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        ## Out a list
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x) {
        ## from your list sees if you have the inverse calculated
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## extracts the matrix to solve the inverse
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        ## returns the inverse
        inv
}
