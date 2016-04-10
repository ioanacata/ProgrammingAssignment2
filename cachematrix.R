## These functions were written for the Coursera Data Science: R Programming
## Week 3 Assignment; march 2016

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<-function(y) {
		x<<-y
            m<<-NULL
	}
	get<-function() x
	setsolve<-function(solve) m <<- solve
	getsolve<-function() m
	list(set=set, get=get,
		setmean=setmean,
		getmean=getmean)
}


 ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ## If the inverse has already been calculated (and the matrix has not changed),
 ## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m<-solve(data, ...)
	x$setsolve(m)
	m
}
}
