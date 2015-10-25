## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL   					        #Variable where cache will be stored.
	set <- function(y)				        #function to CLEAR cache data.
	{
		x <<- y
		m <<- NULL
	}
	get<-function() x 				        #Function to get ACTUAL/ORIGINAL matrix.
	setinverse <- function(inv) m <<- inv	                #Function to REPLACE cache with new inverse.
	getinverse <- function() m			        #return inverse from cache.
	list(get=get,set=set,setinverse =setinverse ,getinverse =getinverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	m <- x$getinverse()				#get what is present in cache.
	if(!is.null(m)) 					#If returned data is not NULL then return data from CACHE.
	{
		message("Getting cache Data");
		return(m)
	}
	data <- x$get()					        #Otherwise, get ORIGINAL matrix.
	m <- solve(data)					#calculate INVERSE of the ORIGINAL matrix.
	x$setinverse(m)				        	#UPDATE cache with the latest Inverse.
	m							#return the calculated INVERSE.

}
 
