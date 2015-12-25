## These functions are used to create a special object that stores a matrix 
## and cache's its inverse. 


## makeCacheMatrix function creates a special vector, which is really a list 
## containing functions to:
## 1. set the values of the matrix and initialize its cache (set)
## 2. get the values of the matrix (get)
## 3. set the inverse of the matrix (setInv)
## 4. get the inverse of the matrix (getInv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInv <- function(inverse) {
                inv <<- inverse
        }
        getInv <- function() {
                inv
        }
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve function calculates the mean of the special "matrix" created 
## with makeCacheMatrix function. However it checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}