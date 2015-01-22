## Put comments here that give an overall description of what your
## functions do

## Matrix inversion on big matrix data is time consuming. 
## Caching the inversed matrix using lexical scoping techniques.
## Helps us to avoid from making a call to sovle function everytime we need the 
## same inverse data.
# Test Output:

## mat <- matrix(1:4, 2, 2)

## mcm <- makeCacheMatrix(mat)
## cacheSolve(mcm)
# Cache miss
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#Here we have reset the function. So, no cache
# mcm <- makeCacheMatrix(mat) 
# cacheSolve(mcm)
# Cache miss
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# cacheSolve(mcm)
# Catch hit
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# solve(cacheSolve(mcm))
# Catch hit
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## The makeCacheMatrix takes matrix data as its input. 
## By default it takes empty one. 
## It helps us maintaining the cached inversed matrix data.
## It returns the list of functions, Which will be consume by makeSolve function.
## function is a first class object in R. 
## So, we could pass function object to another function.
## The inner function takes the advantage of lexical scoping to read or write data
## into the varialbes declared outside its scope/parent environment using <<- operator
makeCacheMatrix <- function(mdata = matrix()) {
    cached_solve <- NULL
    
    set <- function(ndata) {
        mdata <<- ndata
        ## Whenever data changes. We have to invalidate it. 
        ## So, the next call to getSolve will return NULL.
        ## Then, the makeSolve will invoke on solve on new matrix data and 
        ## caches it using setsolve()
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


## This function takes the list returned by makeCacheMatrix function.

cacheSolve <- function(mcm, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- mcm$getsolve()
            
    if (!is.null(mi)) {
        message("Catch hit")
        return(mi)
    }
    
    message("Cache miss")
    
    cdata <- mcm$get()
    
    ## Inserve the matrix
    mi <- solve(cdata, ...)
    
    ## Cache it
    mcm$setsolve(mi)
    
    mi 
}
