## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m<<-NULL                   ## assigning m to NULL       
  y<-solve(x)
  get<-function() x          ## this function gets the value of the matrix we provide
  setin<-function(y){        ## this function changes and assigns m the value of inverse of the matrix
    m<<-y                     
  }
  getin<-function() m
  list(get=get,setin=setin,getin=getin)
}


## the following function computes the inverse of the matrix and cache 
## if it's inverse has already been calculated and matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  z<-makeCacheMatrix(x)
  m<-z$getin()
  if(!is.null(m)){                 ## checking if inverse has been calculated before or not
    messege("getting cached data")
    return(m)
  }
  data<-z$get()
  m<-solve(data)       ## if the matrix is changed then calculate the inverse
  z$setin(m)           
  return(m)
}
