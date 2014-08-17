## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix as argument (assumed to be square i.e. same number of rows and columns)
## also assumed (and not further tested ) to be invertible -i.e. another matrix can be found which
## when matrix mutiplied returns the Identity matrix of the same dimension. 


makeCacheMatrix <- function(X = matrix()) {
  INV <- NULL  # inintialise by setting INV (the variable storing the Inverse matrix to NULL)
  
  #the first function 'sets' the value of the matrix, and at the same time resets the Inverse
  set <- function(Y) {
    X <<- Y
    INV <<- NULL
  }
  get <- function() {X} #1-line function - just returns the value of the matrix
  setINV <- function(invmatrix) INV <<- invmatrix ## sets the value of the inverse into the cache
  getINV <- function() INV ## returns the value of the inverse matrix
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)
# the last line of the function - returns a list of the four functions 
# these will then be 'attached' to an input object when called. 
# for instance ; 
# mat2<-makeCacheMatrix(mat)
# will result in mat2 containing a list of functions. 
# mat2$get() will return the original matrix 'mat' 
# mat2$set(rbind(c(1,2),c(3,4))) will change the original matrix

}

## Write a short comment describing this function
# cacheSolve is designed to take advantage of the functions returned from the companion function makeCacheMatrix
# This takes the function list returned by the companion function and returns the inverted matrix 
# which is stored within that list (accessed by $get() )
# first it checks to see if the inverted matrix is already stored in memory "cached"

cacheSolve <- function(X, ...) {
  INV <- X$getINV() # first check what is stored in memory 
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  } #if there is something there, then return that
  data <- X$get() #otherwise, take the matrix
  INV <- solve(data, ...) # and invert it using the solve() function with default parameters
  X$setINV(INV) # and set the cache for next time
  INV # lastly return the inverted matrix
}
