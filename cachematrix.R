## To provide cache function to get inverse matrix 
## Need library MASS


## utility to make sure required library is installed
pkgTest<-function(x) {
	if (!require(x, character.only=TRUE))
	{
	    install.packages(x,dep=TRUE)
	    if (!require(x, character.only=TRUE))
		stop("Package not found!")
	}
}

##  makeCacheMatrix to return a list of get/set/setinverse/getinverse
##  function to manage inverse matrix cache
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse <- function(inverse) m<<-inverse
	getinverse <- function() m
	list(set=set, get = get, setinverse=setinverse, getinverset=getinverse)
}


## cacheSolve is to return an inverse matrix by using makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached inverse matrix")
		return (i)
	}
	## make sure MASS package is installed
	pkgTest("MASS")
	data<-x$get()
	i <- ginv(data)
	x$setinverse(i)
	return (i)
}
