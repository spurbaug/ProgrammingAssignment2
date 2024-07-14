
##Function makeCacheMatrix 
## Input : Matrix 
## Implements : Implements matrix object and maintain its inverse in cache . 
##         Implements functions to get / set  Matrix and its inverse :
##          functions : set  
##          functions : get  
##          functions : setInverse  
##          functions : getInverse  
##         
makeCacheMatrix <- function(x = matrix()) {
  
  ##inverseMatrix == NULL if no inverse is available 
  ##   else Inverse of matrix x
  inverseMatrix <- NULL
  
  ##set Method for matrix
  set <- function (matrix){
    m <<- matrix
    inverseMatrix <<- NULL
  }
  
  ##get Method for matrix
  get <- function() m
  
  ##setInverse : sets inverseMatrix
  setInverse <- function(inverse){
    inverseMatrix <<- inverse
  }
  
  ##getInverse : returns inverse of matrix 
  getInverse <- function (){
    inverseMatrix
  }
  
  ##Create list of methods 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##Function cacheSolve 
## Input : Matrix and its parameters 
## Implements : Inverse of a given matrix, if possible,
##              use inversed cached version.          
## Output : Inverse of a given matrix
##
cacheSolve <- function(x, ...) {
  
  ## return cached version, if available
  my_inverse <- x$getInverse()
  if(!is.null(my_inverse)) {
    message("getting cached inverse matrix")
    return(my_inverse)
  }
  
  ## Calculate inverse of matrix 
  ## and cache it for future.
  my_matrix <- x$get()
  my_inverse <- solve(my_matrix,...)
  x$setInverse(my_inverse)
  
  ##return inverse
  my_inverse
}
