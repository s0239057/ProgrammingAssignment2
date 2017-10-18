## Create a list containing functions to set matrix, get matrix, set inverse,
## get inverse (if stored in cache)..

## assume x is always square-invertable.. based on makeVector & cachemean examples

makeCacheMatrix <- function(x = matrix()) {

    mat_inv = NULL
    set = function(y) {
        x <<- y 
        # x is assigned a value y in a different environment
        mat_inv <<- NULL
    }
  
    get = function() x
    setinv = function(inv) mat_inv <<- inv 
    getinv = function() mat_inv
  
      list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}

## Use list of functions in makeCacheMatrix function to calculate matrix
## inverse, checking first if it has been previously calculated..

cacheSolve <- function(x, ...) {
        
    mat_inv = x$getinv()
  
    # if the inverse has already been calculated and stored
    if (!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  
    # if not previously stored, the inverse is calculated 
    mat.inv = x$get()
    mat_inv = solve(mat.inv, ...)
    # solve given with no RHS term so assumed to be identity matrix, therefore
    # solves 1/a i.e. the inverse of a..
  
    x$setinv(mat_inv)
  
    return(mat_inv)
}
