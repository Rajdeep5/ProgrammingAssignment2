## ======================================================================================
## Course       : R Programming
## Session      : November 2015
## Assignment No: 2 - Lexical Scoping
## Description  : This assignment tests our understanding of Lexical scoping, Free Variables
##                and Super Assignment Operators ("<<-")
##                The scenario is based on a simple matrix (cacheMatrix)
##                We have 2 functions:
##                (1) makeCacheMatrix: Allows you to create a matrix and returns a matrix
##                                     object that allows you to call functions on the 
##                                     object.
##                (2) cacheSolve     : Based on the object returned from the makeCacheMatrix,
##                                     this function is used to determine the inverse. An 
##                                     attempt is made to see if the matrix inverse is currently 
##                                     cached. If so then this value is returned. If not then
##                                     we get the current cache matrix, calculate its inverse
##                                     and return this value.
##
## Execution Example
##            Input: myMatrix = makeCacheMatrix(x=matrix(c(1, 2, 3, 4), nrow=2, ncol=2) )
##                  : cacheSolve(myMatrix)
##   expected 1st time output: the calculated inverse matrix
##   subsequent output (calling cacheSolve(myMatrix) ) - the cache inverse matrix
##   (assuming "myMatrix" is not changed)
## ======================================================================================

## --------------------------------------------------------------------------------------
## Function: makeCacheMatrix
## Input   : A matrix.
## Output  : makeCacheMatrix object that provides access to additional functions.
## Purpose : Creates a Matrix object and returns this object in addition to a list of 
##           functions that allow you to further analyze the created matrix. Those 
##           functions are: (1) setCacheMatrix - set the cache matrix
##                          (2) getCacheMatrix - get the cache matrix
##                          (3) setMatrixInverse - set the inverse of the cache matrix.
##                               Assumption: matrix is a square matrix
##                          (4) getMatrixInverse - get the inverse of the cache matrix
##          
## --------------------------------------------------------------------------------------
makeCacheMatrix <- function (x = matrix()){
  
    ## Set the Matrix Inverse to NULL
    ## Everytime a new makeCacheMatrix object is created or changed the inverse
    ## is set to NULL so that should the inverse be required then the calculation
    ## is performed.
    inv <- NULL
  
    ## Function declaration - setCacheMatrix. Sets the Matrix
    setCacheMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
  
    ## Function declaration - getCacheMatrix. Gets the Matrix
    getCacheMatrix <- function() x
  
    ## Function declaration - setMatrixInverse. Sets the Inverse of the matrix
    ## Note: Ideally a check should be performed for a square matrix, however an
    ## assumption here is made.
    setMatrixInverse <- function(y) inv <<- y

    ## Function declaration - getMatrixInverse. Gets the matrix inverse
    getMatrixInverse <- function () inv
  
    ## set the Cache Matrix and return a list of functions to the makeCacheMatrix
    list( setCacheMatrix   = setCacheMatrix, 
          getCacheMatrix   = getCacheMatrix, 
          setMatrixInverse = setMatrixInverse, 
          getMatrixInverse = getMatrixInverse )
}

## --------------------------------------------------------------------------------------
## Function: cacheSolve
## Input   : makeCacheMatrix Object
## Output  : Inverse of the special matrix
## Purpose : Computes the Inverse of the special matrix returned by makeCacheMatrix.
##           If the inverse has already been calculated (and matrix not changed) then
##           this function retrieves the inverse of the cache, otherwise the inverse
##           calculation is performed.
## --------------------------------------------------------------------------------------
cacheSolve <- function (x, ...){

    ## Get the inverse of the cache matrix (without any calculation)
    inv <-x$getMatrixInverse()

    ## If the inverse of the cache matrix exists then return this value, otherwise continue.
    if(!is.null(inv)) {
        message("getting cache data")
        return (inv)
    }
  
    ## Note: Only here if inverse of the cache matrix did not exist!
    
    ##Get the current cache matrix
    curr_matrix <- x$getCacheMatrix()
  
    ##Calculate the inverse by calling the inverse function
    inv <- solve(curr_matrix)
  
    ##set the new calculated inverse
    x$setMatrixInverse(inv)
  
    ##return the inverse matrix
    inv
}