
#### Caching the Inverse of a Matrix ######



makeCacheMatrix <- function(x = matrix()) {
  Invmat <- NULL    ## setting inverse matrix as null
  set <- function(y){
    x <<- y
    Invmat <<- NULL ## Assigning Inverse of matrix as null
  }
  get <- function()x                                      # gettting the value of the Matrix
  setInverse <- function(Inverse) Invmat <<- Inverse       #setting the value of the invertible matrix
  getInverse <- function() Invmat                         #getting the value of the invertible matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' ##
  ## Get the value of the invertable matrix from the makeCacheMatrix function
  Invmat <- x$getInverse()
  if(!is.null(Invmat)){                        #if inverse matrix is not NULL
    message("getting cached data")        #Message Type: Getting a Cached Invertible Matrix
    return(Invmat)                        #returning to the invertible matrix
  }
  Origin_mat <- x$get()                          #getting the the original Matrix Data
  Invmat <- solve(Origin_mat,...)                #getting the original Matrix Data
  x$setInverse(Invmat)                            # setting the invertible matrix
  return(Invmat)                                  ##returning to the invertible matrix
  
}