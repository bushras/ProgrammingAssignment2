## Name: Bushra
#Peer Assignment #2
##There are two funtions defined below:
##  1) makeCacheMatrix
##  2) cacheSolve
## Purpose of first  function is to take a matrix, have functions within it 
## to perform various activities on matrix  and cache it.
## Purpose of second  function is to take matrix and provide its inverse.

## Beginning of script.

## makeCacheMatrix inline comments are provided:

makeCacheMatrix <- function(inputVal = matrix()) { ## Takes matrix as an input
  resInverse<-NULL ## Sets the inverse of cache matrix to NULL
  
  ## Four function are defined within makeCacheMatrix function:
  ## a) setMatrix begins:
  
  setMatrix<-function(y){ ## Takes matrix as an input for caching or changing whenever this function is called.
    inputVal<<-y  ## Assigning value passed to a variable.
    resInverse<<-NULL ## Resetting inverse of a matrix to NULL to assure only latest set matrix's inverse will be calculated.
  }
  ## setMatrix ends.
  
  ## b) getMatrix begins:
  getMatrix<-function() {inputVal} ## Calling function getMatrix to use value of matrix currently available.
  ## getMatrix ends.
  
  ## c) setMatrixInverse begins:  
  setMatrixInverse<-function(matrixInverse) {resInverse<<-matrixInverse} ## This will set result value equals to the matrix inverse of matrix as passed as an input.
  ## setMatrixInverse ends.  

  ## d) getMatrixInverse begins:  
  getMatrixInverse<-function() {resInverse} ## To extract current matrix inverse result.
  ## getMatrixInverse ends.  
  
  ## Finally defining names of to the functions and storing in a list:
  list(setMatrix=setMatrix,getMatrix=getMatrix,
       setMatrixInverse=setMatrixInverse,
       getMatrixInverse=getMatrixInverse)
  
}


## cacheSolve inline comments are provided:

cacheSolve <- function(inputVal) { ## Inputs matrix in the function.
        ## Return a matrix that is the inverse of 'inputVal'
  ## To get inverse of a matrix this 'if' condition check two things:
  ## a) Matrix is a square
  ## b) Matrix determinant is not zero so that inverse can be calculated.
  if(nrow(inputVal$getMatrix())==ncol(inputVal$getMatrix()) && det(inputVal$getMatrix())!=0){
    resInverse<-inputVal$getMatrixInverse()  ## This gets Inverse result of a matrix and stores in resInverse.
    
    if(!is.null(resInverse)){  ## This checks that inverse of matrix result variable is not empty
      print("Cached Inverse of a Matrix:")  
      return(inputVal)  ## In case result of matrix is already cached then, this will get inverse from cache
    }
    
    tempData<-inputVal$getMatrix()  ## To get matrix for first time in case cached result of a matrix inverse is not available.
    resInverse<-solve(tempData)  ## Solve is an in-built R-programming function to get inverse of a matrix.
    inputVal$setMatrixInverse(resInverse)  ## Setting the current inverse of a matrix in cache by calling 'set inverse' function 
    resInverse  ## To return current result inverse of a matrix
  }
  else{ ## In case matrix does not fulfill requirements to calculate inverse of a function them it prints:
    if(nrow(inputVal$getMatrix())!=ncol(inputVal$getMatrix())){  ## If matrix is not square
      print("It is not a square Matrix. Please choose another Matrix.")
    } ## If matrix's determinant is zero
    else{print("Cannot compute Inverse of Matrix as determinant is zero. Please choose another Matrix.")}
  }
}
## End of script.