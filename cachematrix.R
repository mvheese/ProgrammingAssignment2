## Matrix inversion is a costly computation and therefore caching the inverse
## of a matrix for reuse prevents then need to compute it repeatedly.        
## The following functions do so.
## Test the program with:
##	mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
##	mat2 <- makeCacheMatrix(mat)
##	cacheSolve(mat2)

## makeCacheMatrix: creates a matrix-object that can cache its inverse

## The matrix-object is a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL			## set m to NULL
	set <- function(y) {		## set value of the matrix
		x <<- y			## cache the inputted matrix
		m <<- NULL 		## set inverse matrix to NULL
		}
	get <- function() x		## function to return x
	setinverse <- function(inverse) m <<- inverse	## store inverse
	getinverse <- function() m	## get stored inverse		
	list(set = set,                 ## create list to store four functions
	     get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}

## cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix.

## If the inverse has already been calculated no computation is done and 
## the inverse is retreived from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() 		## get a n already calculated inverse
	if(!is.null(m)) {		## chek if cacheSolve has run before
		message("getting the cached inverse")
		return(m) 		## return inverse calculated before
	}
	## Inverse not in cache, so compute and set in cache
	data <- x$get()			
	m <- solve(data, ...)		## compute the inverse of input matrix
	x$setinverse(m) 		## cache the inversed matrix
	m				## return inversed matrix
}
