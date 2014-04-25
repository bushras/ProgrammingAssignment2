## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  resInverse<-NULL
  
  setMatrix<-function(y){
    inputVal<<-y
    resInverse<<-NULL
  }
  
  getMatrix<-function()inputVal
  
  setMatrixInverse<-function(matrixInverse) resInverse<<-matrixInverse
  getMatrixInverse<-function() resInverse
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,
       setMatrixInverse=setMatrixInverse,
       getMatrixInverse=getMatrixInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(nrow(inputVal$getMatrix())==ncol(inputVal$getMatrix()) && det(inputVal$getMatrix())!=0){
    resInverse<-inputVal$getMatrixInverse()
    
    if(!is.null(resInverse)){
      print("Cached Inverse of a Matrix:")
      return(inputVal)
    }
    
    tempData<-inputVal$getMatrix()
    resInverse<-solve(tempData)
    inputVal$setMatrixInverse(resInverse)
    resInverse
  }
  else{
    if(nrow(inputVal$getMatrix())!=ncol(inputVal$getMatrix())){
      print("It is not a square Matrix. Please choose another Matrix.") 
    }
    else{print("Cannot compute Inverse of Matrix as determinant is zero. Please choose another Matrix.")}
  }
}
