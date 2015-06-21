##REQUIREMENTS : to create following functions
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## The function makeCacheMatrix returns a list containing function to  do
##the following functionalities :
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 					## initaiaating inverse with the value 'NULL'
	set_matrix <- function(y) {			
		x <<- y 					## setting the matrix 'x'
		inverse <<- NULL
	}
	get_matrix <- function() x 				## returning matrix 'x'
	set_inverse <- function(solve) inverse <<- solve 	## caching  the value of the inverse 
	get_inverse <- function() inverse 			## returns the inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## The below function cacheSolve helps in calulating the inverse of the special"matrix" 
##  which is created using the function makeCacheMatrix.
## It first checks if inverse has already been created.
## If yes, the inverse is is got from the cache skipping the calculations.
## Else it will calculate the inverse of the data and sets the value of the inverse in the cache 
## using the 'set_inverse' function.

cacheSolve <- function(x, ...) {				## function to return the inverse of the matrix
	inverse <- x$get_inverse()				## to  get the inverse
	if(!is.null(inverse)) {					## checking if the inverse is already present or calculated
		message("getting cached data")			## displaying the message
		return(inverse)
	}
	data <- x$get_matrix()					## getting the matrix
	inverse <- solve(data, ...)				## computing inverse
	x$set_inverse(inverse)					## cache the inverse
	inverse 						## inverse is returned
}
