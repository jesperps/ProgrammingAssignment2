## The functions makeCacheMatrix and cacheSolve can be used together 
## to save (cache) the solution of a matrix in memory.
## Solving large matrices can be time-consuming 
## therefor caching the solution of a allready solved matrix can save a lot of time.


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix takes a matrix as a argument and returns a list of functions.
## Since the cache is initialized every time the function is called 
## the result of makeCacheMatrix has to be stored for the function to be effective
## e.g 
##      m<-matrix(c(1,1,1,3,4,3,3,3,4),ncol=3,nrow=3)
##      mcm<-makeCacheMatrix(m)
## The functions returned by makeCacheMatrix can then be used to determain 
## if a matrix already has been solved and cache it if it hasn't
## i.e 
##      mcm$setmatrix(<any matrix>) will add the matrix to the cache 
##      and mcm$getmatrix() will return the cached matrix.
## makeCacheMatrix can be used to cache any matrix but 
## the caching only makes sense if the creation of the matrix is resource intensive - 
## like solutions of large matrices (solved with the solve()-function)
## The get()-fuction returned by makeCacheMatrix can be used to get the matrix 
## which was used calling the makeCacheMatrix
## e.g mcm$get()
        
        #initialize m-variable to be shure that m hasn't been used before
        m<-NULL
        
        #returns the matrix used in the call of makeCacheMatrix 
        getoriginalm<-function() x
        
        #recievs a matrix and sets m-variable in the parent environment 
        #making it posible to save the value of m for later use with other functions
        setmatrix<-function(anymatrix) m<<- anymatrix
        
        #gets the value of the m-variable 
        getmatrix<-function() m
        
        #returns the list of functions
        list(getoriginalm=getoriginalm,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

cacheSolve <- function(x, ...) {
## cacheSolve takes a result of makeCacheMatrix
## implaments the functions returned by the makeCacheMatrix to 
## return a matrix that is the inverse of the original matrix 
## used to call makeCacheMatrix.
## Usage exemple:
##      m<-matrix(c(1,1,1,3,4,3,3,3,4),ncol=3,nrow=3)
##      mcm<-makeCacheMatrix(m)
##      cacheSolve(mcm) #computes and caches the inverse of m-matrix and returns the result
##      cacheSolve(mcm) #returnes the previously cached result

        #Checks if getmatrix()-function has a cached value, if so returning the value
        if(!is.null(x$getmatrix())){
                message("getting cached data")
                return(x$getmatrix())
        } else { #computes the inverse and caches the computation with setmatrix()-function
                s<-solve(x$getoriginalm())
                x$setmatrix(s)
                s   #returns the computation   
        }      
}
