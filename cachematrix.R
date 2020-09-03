################################################################################
##                                                                            ##
##          PROGRAMMING ASSIGNMENT 2nd FOR R PROGRAMMING ON COURSERA          ##
##                              by Bruno Blasi                                ##
##                                                                            ##
################################################################################
##                                                                            ##
##  This code is meant to be able to solve the Programming Assignment 2nd for ##
##  R Programming on Coursera, witch goal is to caching the inverse of a      ##
##  matrix.                                                                   ##
##                                                                            ## 
##  1. ABOUT THE CODE:                                                        ##
##                                                                            ##
##  The code is based on the Example code provided in the 2nd Programming     ##
##  Assignment's Instructions, written by Roger D. Peng, PhD for the R        ##
##  Programming Course on Coursera offered by Johns Hopkins University:       ##
##                                                                            ##
##  /***********************************************************************  ##
##  *    Title: Repository for Programming Assignment 2 for R Programming     ##
##  *           on Coursera                                                   ##
##  *    Author: Peng, R.D.                                                   ##
##  *    Date: April 22, 2014                                                 ##
##  *    Code version: GitHUB SHA-1 7f657dd22ac20d22698c53b23f0057e1a12c09b7  ##
##  *    Availability: https://github.com/rdpeng/ProgrammingAssignment2       ##
##  ***********************************************************************/  ##
##                                                                            ##
##  Consulted: September 3, 2020                                              ##
##                                                                            ##
##  2. ABOUT THE FUNCTIONS:                                                   ##
##                                                                            ##
##  This two functions are meant to be used to, given a square nonsingular    ##
##  matrix, find the inverse matrix and store the result in cache for time    ##
##  saving computation.                                                       ##
##                                                                            ##
##  makeCacheMatrix : takes a matrix 'x' as argument and returns a list       ##
##  object of length 4 cointaining four functions: (1) 'set', for setting     ##
##  the matrix which is going to get invered, (2) 'get', for getting the      ##
##  matrix which is going to get invered, (3) 'setsolve', for storing the     ##
##  result of the 'solve' functions (called by the 'chaceSolve' function)     ##
##  for future usage, (4) 'getsolve', for retriving the inversed matrix       ##
##  stored before.                                                            ##
##                                                                            ##
##  cacheSolve : takes a 'makeCacheMatrix' list as main argument and          ##
##  returns a the inverse matrix of 'x' (from the 'makeCahceMatrix'), if      ##
##  possible. If there is a result stored, it is going to return that matrix, ##
##  otherwise is going to calculate the inverse variable, if possible. The    ##
##  functions uses the 'solve' function to inverse the matrix. The remaining  ##
##  accepted arguments are from the 'solve' function. Be careful with this    ##
##  usage because it might return unexpected values, not corresponding with   ##
##  the inverse of 'x'. For more details, please consult the 'solve' function ##
##  documentation.                                                            ##
##                                                                            ##
##  Take into account that, for this assignment, I "assume that the matrix    ## 
##  supplied is always invertible", as indicated in the assignment            ##
##  instructions.                                                             ##
##                                                                            ##
##    2.1. VALID MATRICES                                                     ##
##                                                                            ##
##    The matrix served as argument must be a square nonsingular matrix.      ##
##    Otherwise 'cacheSolve' is going to jump the 'solve' function errors for ##
##    non square matrices or for square singular matrices, as appropriate.    ##
##                                                                            ##
##  3. ARGUMENTS:                                                             ##
##                                                                            ##
##  makeCacheMatrix :                                                         ##
##                                                                            ##
##    x         a square numeric or complex matrix containing the             ##
##              coefficients of the linear system. Other types are not going  ##
##              to rise an error here, but they will rise on 'cacheSolve'.    ##
##                                                                            ##
##  cacheSolve :                                                              ##
##                                                                            ##
##    x         a list of length from 'makeCacheMatrix'                       ##
##                                                                            ##
##    ...       other arguments form 'solve' function. (Not recommended, for  ##
##              more details consult the 'solve' function documentation.      ##
##                                                                            ##
##  4. HOW TO USE - AN EXAMPLE:                                               ##
##                                                                            ##
##  > A <- matrix(c(3, 2, 5, 2, 3, 2, 5, 2, 4),3,3)                           ##
##  > A_inv <- A_inv <- makeCacheMatrix(A)                                    ##
##  > cacheSolve(A_inv)                                                       ##
##              [,1]        [,2]       [,3]                                   ##
##  [1,] -0.29629630 -0.07407407  0.4074074                                   ##
##  [2,] -0.07407407  0.48148148 -0.1481481                                   ##
##  [3,]  0.40740741 -0.14814815 -0.1851852                                   ##
##  > cacheSolve(A_inv)                                                       ##
##  Getting cached data:                                                      ##
##              [,1]        [,2]       [,3]                                   ##
##  [1,] -0.29629630 -0.07407407  0.4074074                                   ##
##  [2,] -0.07407407  0.48148148 -0.1481481                                   ##
##  [3,]  0.40740741 -0.14814815 -0.1851852                                   ##
################################################################################

## 'makeCacheMatrix' takes a matrix 'x' as argument and returns a list       
##  object of length 4 cointaining four functions (to be used by 'cacheSolve')
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##  cacheSolve : takes a 'makeCacheMatrix' list as main argument and returns a
##  the inverse matrix of 'x' (from the 'makeCahceMatrix'), if  possible, form 
##  cache or calculating, as appropiate.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting cached data:")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}