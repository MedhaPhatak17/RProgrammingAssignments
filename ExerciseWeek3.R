
makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) 
        inversematrix <<- inverse
        getinverse <- function() inversematrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("Cached data.")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data)
        x$setinverse(inversematrix)
        inversematrix
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
