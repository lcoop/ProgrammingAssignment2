## These functions return the inverse of a square matrix.
## The input matrix is stored in the parent environment.
## The cacheSolve function first checks if the matrix is already cached (in thep parent environment) and if it is, it will use it.
## Else it will overwrite the old cached matrix with what the new matrix.

## MakeCacheMatrix: This function takes a matrix 'x' and calculates the inverse of 'x'.
## It returns a list of 4 elements

makeCacheMatrix <- function(x = matrix()) {
    ## assign NULL to m
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## assign x to get
    get <- function() x
    ## calculate inverse of matrix
    setmatrix <- function(solve) m <<- solve
    ## assigns matrix to m
    getmatrix <- function() m
    ## provide the return values
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix) 
}


## cacheSolve checks if the inverse of matrix 'x' is cached (m != null), if it is, it gets cached data.
## if m = null, it then returns the new inverse of matrix 'x'.

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

