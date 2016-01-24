#returns a list with some methods which can be used for caching
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y){
        x <<- y
        invX <<- NULL
    }
    get <- function(){
        x
    }
    getInv <- function(){
        invX
    }
    setInv <- function(z){
        invX <<- z
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)    
}



##returns an inverse of the matrix from the cache if is is available in the cache. Else 
##creates the inverse, caches the inverse and returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invX <- x$getInv()
    if(!is.null(invX)){
        message('From cache..')
        return(invX)
    }
    invX <- solve(x$get())
    x$setInv(invX)
    invX
}
