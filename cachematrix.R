##  title: "Cache Matrix Package" V02
##  author: "Robert Hadow"
##  date: "August 11, 2015"
##  output: R code

# The code below allows a user to calculate and use the inverse of a matrix many
# times in succession. The first time she calls it, R will calculate the inverse
# matrix.  After that it will pull it from a cache, saving a lot of machine
# time.
# 
# The code is an example of storage of variables in multiple environments in 
# order to create a mutable state. In the makeCache routine, m is defined and
# bound within makeCache in the second line.  Another m is described but its
# binding in the global environment occurs later.  There is also an example
# of function masking (get).
# 
# HOW DOES IT WORK?
# Each time cacheSolve is called, it creates a new environment. When it 
# 
# Test cases and stubs are found in another file.
# 
# 
# 
# 
# 
#
# Scope and Practical Limitations
# The PC is practically limited to square matrices of dim 10,000.
# Each one requires 768 Mb working memory and disk.  A more advanced
# implementation could store solutions to disk, but that is out of scope
# for this project. A large (dim = 10,000) matrix requires 20 min calcuation.
#



#
# 
# set USE CASE
# 
#
# cacheSolve USE CASE
# Take a square invertible matrix, go to a protected environemnt and look it up
# in a previously created object.  If it exists in that object, retrieve its
# inverse from that object and return it.  If it does not exist, use solve(x)
# to calculate the inverse.  Store the original matrix and its inverse in the 
# protected object and return the inverse.
#
# protectedLibrary USE CASE
# In a protected environment (not accessible to a user) establish an object or
# objects that allow for the storage of a countable number of sqaure matrices.
# Relate each to its inverse. Keep in mind inversion is commutative. To the 
# extent it is possible, segregate matrices by class (integer, numeric, 
# imaginary) and by size (square matrices have only one useful dimension)
#
# cacheSolveTestStub USE CASE
# check dimensions of m and mi. If they are not the same, return false
# check classes of m and mi. If they are not the same, return false.
# multiply two square matrices, m and mi.  the results must be the identity
# matrix of the same dimension of m and mi, If it is not, return false.
# Include as part of code, creations of the matrices required for testing,
#
#





# HOW ENCAPSULATION IS ACCOMPLISHED
# makeCacheMatrix() and cacheSolve() are to be available to a user in their own
# workspace.  Other required functions and all data are to be hidden and
# inaccessible. This design assumes that "::", ":::", "<<-", and "->>" are not 
# available to the user.
# 



# makeCacheMatrix PSEUDOCODE
#  makeCacheMatrix is a function 
# If a matrix is passed as a formal argument store it locally in x. 
# If no argument is given, create a matrix of 0 dimension. Store locally in x.
# Matrix x is available to calling function in x.
# Create four child functions, set, get, setinverse, and getinverse
# Return list of child functions. 
# 
# 
#
# set PSEUDOCODE
# set operates inside the makeCacheMatrix environemnt
# it takes one formal argument and writes it in makeCacheMatrix
# It writes NULL to object x in 
# 
# get PSEUDOCODE
# get is a mask for the {base} function. It retrieves and returns the value of
# x (the matrix to be inverted) from where it was stored in makeCacheMatrix.
# 
# 
# 
# 
# cacheSolve PSEUDOCODE
# This function uses four functions defined in makeCacheMatrix. It reads and
# writes a value for m in makeCacheMatrix using the super assignment operator
# <<- becasue that is where the functions were defined. It is imporant to note
# that these m objects differ because each time cachSolve is called is a new
# instance and a new envoronment. 
# 
# 

## Write a short comment describing this function

## CODE
## 
makeCacheMatrix <- function(x = matrix(data=numeric()))  {
        # environment(0x00000000146ec240)
        m <- NULL
        set <- function()  {
                x <<- x
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}



cacheSolve <- function(x , ...) {
        message("x ", x)
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting_cached_data")
                return(m)
        }
        message("calculating fresh")
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

