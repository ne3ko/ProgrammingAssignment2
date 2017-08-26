##	The purpose of this function is to create a special "matrix"
##	that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

##	##	I created the "m" object to refer to when caching the matrix.
##	##	Since, I do not want a default value to "m," I set it to NULL.
	m <- NULL

##	##	The "set" function sets the value of the Matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
##	##	The "get" function gets the value of the matrix
	get <- function()	x

##	##	The "setinv" function sets the value of the inverse matrix
	setinv <- function(solve)	m <<- solve	

##	##	The "getinv" function gets the value of the inverse matrix.
	getinv <- function()	m$get

##	##	The "list" function gives a list of the functions to
##	##	refer back to when using "makeCacheMatrix".
	list(set = set, get = get, setinv = setinv, 
	getinv = getinv)
}


##	The purpose of this function is to calculate the inverse of the matrix,
##	and set the value of the matrix to the inverse in the cache
cacheSolve <- function(x = matrix(), ...) {

##	##	I called the "getinv" function to get the value of m from
##	##	"makeCacheMatrix"
 	m <- x$getinv()

##	##	If the inverse has already been calculated, retun the matrix,
##	##	otherwise, compute the inverse of the matrix
	if(!is.null(m)) {
		return(m)
	}
	else{
##	##	Here we are setting the value of the matrix to "matrix"
		matrix <- x$get()

##	##	Here we are finding the inverse of the matrix and setting it to "m"
		m <- solve(matrix, ...)

##	##	We are now using "setinv" to set the value of the inverse matrix,
##	##	and then returning "m"
		x$setinv(m)
		return(m)
	}
}