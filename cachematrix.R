#The functions below are meant to compute the inverse of a matrix.
#In order to save time and avoid redundant (and therefore useless) computations, 
#if the inverse of a matrix has already been computed and stored in cache, it will be retrieved from 
#cache, rather than going through all the computations again.
####################################################################################################
#The first function, makeCacheMatrix, creates a special "vector",
#which is really a list containing 4 functions, in order to...
#...set the matrix in cache;
#...get the matrix from cache;
#...set the inverse of the matrix in cache; 
#...get the inverse of the matrix from cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                   #function to set the matrix in cache
                x <<- y                       
                i <<- NULL
        }
        get <- function() x                    #function to get the matrix from cache
        setInverse <- function(inv) i <<- inv  #function to set the inverse of the matrix in cache
        getInverse <- function() i             #function to get the inverse of the matrix from cache
        
        #the function returns a list of 4 functions
        list(set = set,                        #function to set the matrix in cache
             get = get,                        #function to get the matrix from cache
             setInverse = setInverse,          #function to set the inverse of the matrix in cache
             getInverse = getInverse)          #function to get the inverse of the matrix from cache
}
####################################################################################################
#The second function, cacheSolve, will be used to actually compute the inverse of the matrix.
#First, it looks for the inverse of the matrix in cache.
#If the inverse of the matrix is found, the function will return it from cache.
#If the inverse of the matrix is not found, the function will compute it on the spot, using the "solve" function.
#The newly computed inverse of the matrix will be stored in cache and only then will it be returned.


cacheSolve <- function(x, ...) {
        i <- x$getInverse()                    #trying to get the inverse of the matrix from cache...
        
        if(!is.null(i)) {                      #if the inverse of the matrix has already been computed,
                message("getting cached data") #print specific message
                return(i)                      #and retrieve it from cache
        }
        #else, so if the inverse of the matrix hasn't already been computed...
        data <- x$get()       #get the matrix from cache
        i <- solve(data, ...) #use the "solve" function to obtain the inverse of the matrix
        x$setInverse(i)       #set the inverse of the matrix in cache
        i                     #return the inverse of the matrix 
}
###################################################################################################
#Example on how to call the functions above:
#x<-matrix(1:4,2,2)
#x

#y<-makeCacheMatrix(x)
#cacheSolve(y)
#cacheSolve(y) #"getting cached data"
###################################################################################################




