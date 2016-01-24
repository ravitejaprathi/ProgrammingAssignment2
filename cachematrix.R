
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



## Write a short comment describing this function

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
