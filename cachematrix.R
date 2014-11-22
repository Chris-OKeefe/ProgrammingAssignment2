## Functions to cache and return the inverse of a matrix.

## makeCacheMatrix takes a matrix object and creates four additional functions to assist in 
## computing and caching the value of a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # m set to NULL
    set <- function(y) { # New function: "set", takes argument y.
        x <<- y # y is assigned to x in parent environment.
        m <<- NULL # NULL is assigned to m in parent environment 
    }
    get <- function() x #New function, "get", takes no arguments, defined as x.
    setinverse <- function(inv) m <<- inv #New function, "setminverse", takes "inv" as argument, assigns inv to m in parent environment.
    getinverse <- function() m # New function, "getinverse", takes no args, defined as m.
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse) # output of makeCacheMatrix is list of four functions.
    
}


## cacheSolve uses the functions in makeCacheMatrix to check whether the inverse has already been computed and cached
## If so, it calls the value from cache rather than recalculating.
cacheSolve <- function(x, ...) {
    m <- x$getinverse() #  x$getinverse function assigned to m. Value of x$getinverse assigned to m 
    if(!is.null(m)) { # if m is null, send a message and return m
        message("getting cached data")
        return(m)
    }
    data <- x$get() # calls x$get, gets x from makeCacheMatrix
    m <- solve(data, ...) # calcs inverse based on data, stores it in m
    x$setinverse(m) # send new value of m to m in parent env.
    m #retun m
}