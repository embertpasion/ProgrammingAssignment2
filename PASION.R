## Pasion, Embert P. 
## MATH144-A

PASION <- function(a = matrix()) {

  kabaliktaran <- NULL 
  samahan <- function(b){
    a <<- b
    kabaliktaran <<- NULL
  }
  G <- function() {a}
  SI <- function(inverse) {kabaliktaran <<- inverse}
  GI <- function() { kabaliktaran }
  list(samahan=samahan, G=G, SI=SI, GI=GI)
}

SOLVE_CACHE_PASION <- function(a, ... ) {
  
  kabaliktaran <-  a$GI()
  
  if (!is.null (kabaliktaran)) {
    message("invalid.")
    message("cacheing data.")
    
    return(kabaliktaran)
  }
  
  hakdog <- a$G()
  kabaliktaran <- solve(hakdog, ...)
  
  a$SI(kabaliktaran)
  kabaliktaran
}

