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
makeCacheMatrix <- function(x = matrix(data=numeric()))  { 
        # tell function to expect a matrix x, if none received, make one 0x0
        i <- NULL
        get <- function() x  # function to retrieve original matrix
        setInverse <- function(solve) i <<- solve  
        # function to calculate inverse and write to proper environment
        getInverse <- function() i   # function to retrieve inverse from cache
        list(get = get, setInverse = setInverse, getInverse = getInverse)
        # deliver list of functions as return (with environment id)
}

cacheSolve <- function(x , ...) { # expect a list of functions and environment
        i <- x$getInverse()  # retrieve inverse object from environment
        if(!is.null(i)) {  # if it's valid, return it
                message("getting cached data")
                return(i)
        }
        message("calculating fresh")
        data <- x$get()  # otherwise retrieve the cached matrix
        i <- solve(data, ...)  # calculate inverse
        x$setInverse(i)   # store it in the environment
        i  # and return it
}

## TEST CASES

c2    <- matrix(c(1+1i, 2-2i, 3+1i, -4+4i), nrow=2)
n     <- 10000  # n is dimension of a matrix  
set.seed(104729)
n10   <- matrix(rnorm(n^2, 0, 60), nrow=n) 
i2    <- matrix(c(6L, 0L, 6L, -1L), nrow=2)

ac2   <- makeCacheMatrix(c2)
an10  <- makeCacheMatrix(n10)
ic2   <- cacheSolve(ac2)
ic2   <- cacheSolve(ac2)
ai2   <- makeCacheMatrix(i2)
ii2   <- cacheSolve(ai2)
ii2   <- cacheSolve(ai2)
in10  <- cacheSolve(an10)
str(cacheSolve(an10))
all.equal((n10 %*% in10), diag( , n, n))   # verify matrix x inverse is I
