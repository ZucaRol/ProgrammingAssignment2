#In this function I used the `<<-` operator  to assign a value to 
#a matrix in an environment that is different from the current environment. 
#I used two functions to create a special object that stores 
#a  matrix and caches its inverse.

## Write a short comment describing this function
#The first function, makeCacheMatrix This function creates a special 
#"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	#1.  set the value of the matrix
	invset<-function(y){
		x<<-y
		inv<<-NULL
	}
	#2.  get the value of the matrix
	invget<-function() x
	#3.  set the value of the inverse
	setinv<-function(inversa)inv<<-inversa
	#4.  get the value of the inverse
	getinv<-function() inv
	list(invset=invset, invget=invget, 
	setinv=setinv,getinv=getinv)
}
## Write a short comment describing this function
#The following function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse
#as already been calculated then return the invese in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getinv()
	if(!is.null(inv)){
		massage("obteniendo datos en guardaods")
		return(inv)
	}
	datos<-x$invget()
	inv<-solve(datos,...)
	x$setinv(inv)
	inv
}
