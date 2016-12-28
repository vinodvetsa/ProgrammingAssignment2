## Here, we are creating a special object named 'makeCachMatrix'
## This new object has to be used in conjunction with another function cacheSolve.
## makeCachMatrix has anonymous functions and will be called by cacheSolve
## cacheSolve --> lazyly creates inverse and assigns to environment of makeCachMatrix -- inverse


## This cacheSolve explicitly uses the function solve of the base library. function solve gives the inverse when passed single parameter 'a'
## So, we will not use the full capability of solve as the target is to get only inverse

##	usage >> is prompt e.g. 
##	>>  sampleInputPlainMatrix <- as.matrix(data.frame( col1 = c(3, 8, 29), col2 = c(42,86,9) , col3 = c(20, 86, 134)))
##	>>  sampleChachedMatrix  <- makeCacheMatrix( sampleInputPlainMatrix ) 
##	Now, this one can make feed this sampleCachedMatrix to cacheSolve
##      >> cacheSolve(sampleCachedMatrix) 
##	... one should see the inverse

## Assumptions: let all the errors be handled by solve method like inversible matrix and other issues if any !




makeCacheMatrix <- function(x = matrix()) {
	inverse <-NULL

	## caller resets the cached inverse matrix and sets the base matrix to new value	
	set <- function(y){
		x <<-y  ## interstingly changes the input variable of makeCacheMatrix
		inverse <<- NULL ## explained in makeVector!
	}
	## will be called to return base package matrix
	get <- function(){
		x
	}
	
	## completeley depends on caller of makeCacheMatrix to 'understand' the inverse
	setInverse <- function(inversed) {
		inverse <<- inversed
	}
	## returns the inverse
	getInverse <- function() inverse


	## everytime you create makeCacheMatrix, a new object with all below capabilities given out !
	list( set = set , get = get, setInverse = setInverse, getInverse = getInverse)


}


## Write a short comment describing this function
## here the argument x is of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getInverse()
	## if cached is available available, return 
	if( is.null(inverse)== FALSE ){

		message("cached inverse data, no need to call solve")
		return (inverse) 
	}

	## else create a inverse; set to the makeCacheMatrix and return 

	message("No cached inverse data, need to call solve")
	inverse <- solve(x$get())
	x$setInverse(inverse)

	return (inverse)


}
