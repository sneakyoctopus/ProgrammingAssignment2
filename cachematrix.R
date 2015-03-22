## cache matrix for cash money
## makeCacheMatrix creates functions used to cache a matrix (set), retrieve it from the cache (get),
## and to set/get the inverse of a matrix

## Note explaining use of the superassignment operator(<<-) in this function: the set and setMInv functions
## have to use this operator so that R will look outside the function's environment (to the parent environment) for cachedM. 
## This means that cachedM in all three cases will refer to the same address. 
## If the standard assignment operator (<-) had been used, cachedM would refer to three different variables.

makeCacheMatrix <- function(x = numeric()) {
    cachedM <- NULL

    ## Caches a matrix in the working environment 
    setM <- function(y) {
      x <<- y
      cachedM <<- NULL
    }

    ## Retrieves the cached matrix  
    getM <- function() x
  
    ## Caches the inverse of a matrix
    setmInv <- function(mInv) cachedM <<- mInv

    ## Retrieves the cached inverse 
    getmInv <- function() cachedM
  
    ## Returns the functions defined above to the working environment  
    list(setM = setM, getM = getM,
        setmInv = setmInv,
        getmInv = getmInv)
}

## cacheSolve returns the inverse of a matrix 
## If the inverse has already been cached, the cached inverse is returned
## If the inverse does not exist, it is computed, cached, and returned

cacheSolve <- function(x, ...) {
  
    ## Gets cached inverse 
    mEx <- x$getmInv() 
  
    ## Check if a cached inverse exists and if it does,
    ## let the user know 
    ## and then return the cached inverse
  
    if(!is.null(mEx)) {
      message("getting cached inverse")
      return(mEx)
    }
  
    ## If a cached inverse does not exist, get the matrix you want to invert from the cache
    ## compute the inverse of the matrix
    ## cache the inverse
    ## and then return it
  
    data <- x$getM()
    mInv <- solve(data, ...)
    x$setmInv(mInv)
    return(mInv)
}
