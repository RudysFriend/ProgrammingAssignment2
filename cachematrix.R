## 
## 
## Author: Larry Deters
## Function: makeCacheMatrix 
## Description: "makeCacheMatrix"  is a function used in to create an object containing a matrix.
## Properties are provided for setting and getting the saved matrix and the the cached
## inverse of the matrix. 
## Usage:
## >c=rbind(c(1, -1/4), c(-1/4, 1))
## >myMatrix <- makeCacheMatrix(c) 
##
## See "cacheSolve" to learn how it is used in conjuncyion with "makeCacheMatrix"	
##
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solveit) s <<- solveit
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Function: cacheSolve
## Description: Works in conjunction with "makeCacheMatrix" to return the inverse
## of a matrix. If the inverted matrix has been previously produced the results
## will be retrieved from the cache.
## Usage:
## >c=rbind(c(1, -1/4), c(-1/4, 1))
## >myMatrix <- makeCacheMatrix(c) 
## > solvedMatrix <- cacheSolve(myMatrix)
## > solvedMatrix
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > solvedMatrix2 <- cacheSolve(myMatrix)
## getting cached data
## > solvedMatrix2
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setSolve(s)
        s

}
