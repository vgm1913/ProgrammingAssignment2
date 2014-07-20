## makeCacheMatrix creates a new type of a matrix that can cache it's values
## testCacheMatrix checks to make sure the object of type matrix (list) is indeed a cache matrix
## cacheSolve used the makeCacheMatrix and testCacheMatrix to invert a matrix if it is not already inverted and stored as a cached matrix

## function: makeCacheMatrix
## author: VGM
## created: JUL-20-2014
## coursera: https://class.coursera.org/rprog-005
#' makeCacheMatrix provides a set/list of functions to cache a matrix for faster access later by other functions or callers.
#'
#' This function creates a specail list of function to cache a matrix and returns the resulting list of function to caller.
#' This list of functions include: set, get, setmatrix, getmatrix calls to store and access cached matrix values to another function or caller.
#'
#' @param x matrix to store values in cache
#' @keywords matrix cache
#' @export
#' @examples
#' >myM <- cbind(c(1,2), c(2,1))
#' class(myM)
#' [1] "matrix"
#' >myCachedM <- makeCacheMatrix(myM)
#' >class(myM)
#' [1] "list"
#' >names(myCachedM)
#' [1] "set"       "get"       "setmatrix" "getmatrix"
#' >m <- myCachedM$get()
#' >y <- solve(m) ## or perform some other matrix manipulation or transformation action
#' >myCachedM$setmatrix(y)
#' >myCachedM$get()
#' > myX$get()
#'      [,1] [,2]
#' [1,]    1    2
#' [2,]    2    1
#' > myX$getmatrix()
#'      [,1]       [,2]
#' [1,] -0.3333333  0.6666667
#' [2,]  0.6666667 -0.3333333
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## function: isCacheMatrix
## author: VGM
## created: JUL-20-2014
## coursera: https://class.coursera.org/rprog-005
#' isCacheMatrix returns a TRUE or FALSE by checking to see if parameter: 'x' is indeed a list of functions that correspond to cached matrix standard.
#'
#' This function checks 'x' to make sure it is a list that contains functions with names: set, get, setmatrix, getmatrix.
#' This is a supporting function, please see: \code{\link{makeCacheMatrix}} and \code{\link{cacheSolve}} for further information.
#'
#' @param x list of functions
#' @keywords matrix cache makeCacheMatrix
#' @export
#' @examples
#' >myM <- cbind(c(1,2), c(2,1))
#' >myCachedM <- makeCacheMatrix(myM)
#' >isCacheMatrix(myM)
#' [1] FALSE
#' >isCacheMatrix(myCachedM)
#' [1] TRUE
isCacheMatrix <- function(x) {
    finalAnswer <- FALSE
	if (is.list(x)) {
		prop <- attributes(x)$names
		if (('set' %in% prop)
		& ('get' %in% prop)
		& ('setmatrix' %in% prop)
		& ('getmatrix' %in% prop))
			finalAnswer <- TRUE
		if (finalAnswer) {
			# OK now that we know the list contains all the right names - let's make sure the elements are all functions
			for (i in x) {
				if (finalAnswer & !is.function(i)) {
					finalAnswer <- FALSE
				}
			}
		}
	}
	finalAnswer
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## make sure we got a cache matrix
	if (!isCacheMatrix(x)) {
		message("passed matrix param: x is not a cached matrix; use: makeCacheMatrix(x) prior to calling cacheSolve()")
		message("returning the inverse of x without caching the result")
		## Return a matrix that is the inverse of 'x'
		solve(x)
	}
	else {
		## now first see if we have the inverse of the matrix cached
		m <- x$getmatrix()
		if (!is.null(m)) {
			message("getting cached inverted matrix")
		}
		else {
			myM <- x$get()
			m <- solve(myM, ...)
			x$setmatrix(m)
		}
		## Return a matrix that is the inverse of 'x'
		m
	}
}
