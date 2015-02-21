#Create a makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL   #set the inverse matrix as a Null matrix called m
        set<-function(y){  
                x<<-y  #Create a set function to initialise the matrix
                m<<-NULL #Create a set function to initialise the inverse matrix to NULL 
        }
        print(x)
        get<-function() x  #Create a get function to retrieve the value of the matrix
        setmatrix<-function(solve) m<<- solve  #Set the matrix to it's inverse
        getmatrix<-function() m  #Retrieve the inverse matrix
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) #Create a list of matrices that is inputted and reversed
        
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