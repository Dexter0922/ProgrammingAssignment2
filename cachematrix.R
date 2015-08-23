##  title: "Cache Matrix Package" V04
##  author: "Robert Hadow"
##  date: "August 18, 2015"
##  output: R code
##
## ABSTRACT
##
# The code below allows a user to calculate the inverse of a matrix. The first 
# time she calls it, solve() will calculate the inverse matrix.  After that it
# will pull it from a cache, saving a lot of machine time.
# 
# The code is an example of storage of similarly-named objects in multiple 
# environments.  In the makeCacheMatrix routine, i is defined and bound by an
# instance of makeCacheMatrix in the second line.  The same object is referenced
# from within solveCache.  There is also an example of function masking (get).
# 
# HOW TO USE THESE TWO FUNCTIONS
# 
# When you want to invert a matrix, use am <-makeCacheMatrix(m) where m is the
# matrix you wish to invert. To retrieve the inverse use cacheSolve(am) for the
# first and subsequent retrievals of the inverse. the first time will be slow,
# after that, fast.  Examples may be found in the test cases following the code.
# 
#
# cacheSolve PSEUDOCODE
# Go to the protected environment associated with the command list returned by 
# makeCacheMatrix in which a square invertible matrix was created, and look it
# up. If the inverse exists, retrieve it and return it.  If it does not exist,
# use solve() to calculate the inverse.  Store the inverse in the protected 
# environment and return the inverse.
#
# This function uses three functions defined in makeCacheMatrix.  It reads
# values for the original matrix and its inverse in the environment created by
# makeCacheMatrix and writes values using the super assignment operator <<-
# because that is where the functions were defined. It is important to note that
# these objects differ because each time makeCacheMatrix is called a new
# instance and a new environment is created.
# 
#
# makeCacheMatrix PSEUDOCODE 
# makeCacheMatrixMatrix is a function.  If a matrix is passed as a formal
# argument store it locally in x. If no argument is given, create a matrix of 0 
# rows. Store locally in x.  Matrix x is available to a calling function as
# x diferentiated by environment. Create three child functions, get, setInverse,
# and getInverse. Return list of child functions, along with implied environment
# pointer.
# 
# Each time makeCacheMatrix is called, it creates a new environment. The return
# object from makeCacheMatrix refers only to this environment.  When the return
# from makeCacheMatrix is used as an argument to cacheSolve, the objects
# produced are stored only in the environment associated with the return object,
# (the argument to solveCache). In this way, R keeps separate every calculation
# of the inverse of a matrix.
# 
# 
# SCOPE AND PRACTICAL LIMITATIONS
# The PC is practically limited to square matrices of 10,000 rows. Each one of
# that size requires 768 Mb working memory and disk.  A more advanced 
# implementation could store solutions to disk, but that is out of scope for
# this project. A large (nrows == 10,000) matrix requires 20 min calculation.
#
#
## CODE #
## Create a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = numeric()) {
  ## initialize m
  m <- NULL 
  ## set value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get value of the vector
  get <- function() x
  ## set value of the mean
  setmean <- function(solve) m <<- solve 
  ## get value of the mean
  getmean <- function() m
  ## create a list object which "holds" all the functions
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## cacheSolve: compute the inverse of the special "matrix" returned by makeCacheMatrix
cachemean <- function(x, ...) {
  m <- x$getmean()
  ## if the inverse of a square matrix has been calculated, use cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## calculate cached value
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}