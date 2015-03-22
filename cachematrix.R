## Put comments here that give an overall description of what your
## functions do
## the tow functions create a a list object from an invertible square matrix
## then we can deal with this new special matrix to
## cache potentially time-consuming computations for computing the inverse
## by compute the inverse of the matrix for just one time(for each new matrix 
## or after changing the old one) and cache the coputed inverse 
## to get it when it is needed


## here we Create a an object of a special Type of Matrix as a list of 
## this list containing four functions:
## set the values of the matrix
## get the values of the matrix
## set the values of inverse matrix
## get the values of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- as.null(x)
        setX <- function(y) {
                x <<- y
                m <<- as.null(x)
        }
        getX <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(setX = setX, getX = getX,
             setInv = setInv,
             getInv = getInv)
}


## this one return the inverse of a the original matrix
##it takes the special matrix  that is created by makeCacheMatrix 
## and get the inverse from the cache if it is computed before
## elsewhere compute and cache it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getX()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
