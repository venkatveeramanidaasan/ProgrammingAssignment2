## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mdata = matrix()) {
    cached_solve <- NULL
    
    set <- function(ndata) {
        mdata <<- ndata
        cached_solve <<- NULL
    }
    
    get <- function() {
        mdata
    } 
    
    setsolve <- function(inv_matrix) {
        cached_solve <<- inv_matrix
    }
    
    getsolve <- function() {
        cached_solve
    } 
    
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(mcm, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- mcm$getsolve()
            
    if (!is.null(mi)) {
        message("Catch hit")
        return(mi)
    }
    
    message("Cache miss")
    
    cdata <- mcm$get()
    
    message(cdata)
    mi <- solve(cdata, ...)
    
    mcm$setsolve(mi)
    
    mi 
}
