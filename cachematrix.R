#Create a makeCacheMatrix
#To run this you have to pass in a matricx e.g. A = matrix(c(2, 4, 3, 1, 5, 7,1,2,1),nrow=3,ncol=3,byrow=TRUE)
#then parse the makeCacheMatrix(A) with the defined matrix A above into cacheSolve()
#cacheSolve(makeCacheMatrix(A)) to get the inverse whether it is cached or not.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL   #intialise the inverse matrix as a Null matrix called m
        set<-function(y){  
                x<<-y  #Create a set function to initialise the matrix
                m<<-NULL #Create a set function to initialise the inverse matrix to NULL 
        }
        print(x) #print the initial matrix 
        get<-function() x  #Create a get function to retrieve the value of the initial matrix
        setmatrix<-function(solve) m<<- solve  #Set the matrix to it's inverse
        getmatrix<-function() m  #Retrieve the inverse matrix
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) #Create a list of functions that return the matrices that is inputted and reversed
        
}

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix() #Get the inverse matrix from makeCacheMatrix
        if(!is.null(m)){  #Check if m, the inverse matrix is not Null, then retrieve from the inverse from the cache
                message("getting cached data")
                return(m) #Retrieve the inverted matrix
        }
        matrix<-x$get() #If m is null, i.e. not cached, then get the  matrix from makeCacheMatrix
        m<-solve(matrix, ...) #Invert the matrix
        x$setmatrix(m) #Set the inverted matrix to m
        m #Print the inverted matrix
        
}