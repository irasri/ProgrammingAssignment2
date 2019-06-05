## The makeCacheMatrix function creates a special matrix object whcih can cache it own inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <<- NULL
        set <- function(y){ # set the value of the matrix
                x<<-y
                m<<-NULL
        }
        get <- function() x # get the value of matrix
        setinverse <- function(inverse) i <<- inverse # set the value of the inverse of the matrix
        getinverse <- function() i # get the value of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'.It first checks to see if the inverse has already been calculated. If yes, then it gets the inverse 
##from chache and skips the computation. Otherwise, it calculates the inverse and stores in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse() #get the value of the inverse of the matrix
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get() #  get the value of matrix
        i <- solve(data, ...) 
        x$setinverse(i) #set the value of the inverse of the matrix just calculated
        i
}

