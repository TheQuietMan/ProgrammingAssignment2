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

#Test case : 
# here the matrix P14.10 is as given in FORTRAN 77 by Donald Monro (Puub Edward Arnold, London 1982)
# the solution given in cacheSolve matches that given by Monro
# and a matrix multiplication using the %*% operator returns the identity matrix (with rounding errors)
#> source('~/Documents/RProg006/ProgrammingAssignment2/cachematrix.R')
#> P14.10<-rbind(c(10,7,3,5),c(-6,8,-1,-4),c(3,1,4,11),c(5,-9,-2,4))
#> P14.10List<-makeCacheMatrix(P14.10)
#> P14.10Inv<-cacheSolve(P14.10List)
#> P14.10Inv
#[,1]        [,2]         [,3]        [,4]
#[1,]  0.08310334 -0.03894511 -0.060410917  0.02330573
#[2,]  0.05121128  0.11622202  0.007053051  0.03281202
#[3,] -0.04477154 -0.35909230  0.017785955 -0.35203925
#[4,] -0.01103956  0.13063477  0.100275989  0.11867525
#> P14.10Inv%*%P14.10
#[,1]          [,2]          [,3]          [,4]
#[1,]  1.000000e+00 -3.816392e-17 -7.632783e-17  0.000000e+00
#[2,]  2.151057e-16  1.000000e+00  4.163336e-17  8.326673e-17
#[3,] -5.551115e-16  5.551115e-16  1.000000e+00 -2.220446e-16
#[4,]  1.387779e-16 -1.387779e-16  5.551115e-17  1.000000e+00


