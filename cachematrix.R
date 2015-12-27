## This script assumes the matrix is invertable
## This script contains two functions
## makeCacheMatrix and cacheSolve
##makeCacheMatrix has four functions
## 1) get 2) set 3) getinverse and 4) setinverse
#get returns the matrix which is passed to makeCacheMatrix
#set can be used to alter or reasign the value of the matrix
#getinverse retrieves the inverse of the matrix if assigned, otherwise returns null
#setinverse is used to assign the inverse value


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the matrix assigned to makeCacheMatrix and checks to see if the inverse has already been computed
## If it has, it returns the value of m by using the cached value.
## Otherwise (if it is null)  cacheSolve will compute the inverse using the solve function and will assign it using setinverse.

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
        

