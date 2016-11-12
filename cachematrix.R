## There are two functions:
## Function makeCacheMatrix creates a list of functions to do various activites to store value of the Inverse of a matrix
## Function cacheSolve checks the prev function makeCacheMatrix to see if Inverse has been calculated. If yes, the pulls
## data from cache else calculates it at runtime and returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Function makeCacheMatrix creates a special list containing 4 functions which are as follows
  ## setMatrix  -->  sets the value of the matrix
  ## getMatrix  -->  retrieves the value of the matrix
  ## setInv  -->  sets the Inverse of the matrix to a cache
  ## getInv  -->  retrieves the value of the Inverse of the matrix
  inv<-NULL
  
  setMatrix <- function(z) {
    z<<-x
    inv<<- NULL
  }
  
  getMatrix <- function(){
    x
  }
  
  
  setInv <- function(meanVal) {
    inv <<-meanVal
  }  
  
  getInv <- function(){
    inv
  }
  
  list(setmatrix = setMatrix, getmatrix = getMatrix, setinv = setInv , getinv = getInv)
  

}


## WThe function cacheSolve checks if Inverse is calculated. IF yes, it pulls from the cahce value (Inv) else calculates

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
##  message("The current matrix is ", x$getmatrix())
  
  if (!is.null(inv)) {
    message("Inverse of Matrix is calculated and cached. Wont Change till the Matrix is changed")
    return(inv)
  }
  
  df <- x$getmatrix()
  inv  <- solve(df)
  x$setinv(inv)
  
  inv
  
}
