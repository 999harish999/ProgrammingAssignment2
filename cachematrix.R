## Put comments here that give an overall description of what your
## functions do

## The function makes a special matrix, which have bellow functions
## returns a list of function which can be operated on the mateix supplied
## set a new matrix using set()
## get the matrix which was previously stores using get()
## get Inverse of matrix supplied using setInverse()
## set the new Inverse of matrix using getInverse()

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-NULL
  
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function(){
    x
  }
  
  setInverse<-function(inv){
    inverse<<-inv
  }
  
  getInverse<-function(){
    inverse
  
  }
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## The function returns inverse of matrix (special matrix)
## if the inverse was already calculated and stored and matrix is not changed
## it returns the value, or if its not available it computes inverse and store 
## it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse<-x$getInverse()
  
  if(!is.null(inverse)){
    
    print("getting Inverse of matrix")
    return(inverse)
  }
  
  matrixData<-x$get()
  inverse<-solve(matrixData)
  x$setInverse(inverse)
  print("calculating Inverse of matrix")
  inverse
  
}
