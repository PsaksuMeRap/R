fib <- function(n) {
  
  # Handle "vectors" by element
  if (length(n) > 1) {
    return(sapply(n, fib))
  }
  
  # Base cases
  if (n == 0) 
    return(0)
  if (n == 1) 
    return(1)
  
  # Check to see if n is an integer Do not use is.integer as that is very
  # strict
  if (round(n, 0) != n) 
    return(NA)
  
  # Negative numbers
  if (n < 0) 
    return(fib(-1 * n) * ((-1)^((n + 1)%%2)))
  
  # Everything else
  return(fib(n - 1) + fib(n - 2))
  
}
fib(3)

system.time(fib(20))
# user  system elapsed 
#0 .034   0.003   0.036 

system.time(fib(25))
# user  system elapsed 
# 0.359   0.001   0.361 

system.time(fib(30))
# user  system elapsed 
# 4.196   0.019   4.218 

pkgs<- install.packages()[,1]
pkgs.need<- "memoise"
pkgs.missing<- pkgs.need[!pkgs.need %in% pkgs] 
if (length (pkgs.missing)>0) {
  install.packages(pkgs.missing, dep = TRUE)
}
library(memoise)

fibM <- (function() {
  
  # The code here related to the cache *mostly* comes from the memoise
  # package's object new_cache.
  
  cache <- NULL
  
  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
    cache_set('0', 0)
    cache_set('1', 1)
  }
  
  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
  }
  
  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }
  
  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }
  
  # Initialize the cache
  cache_reset()
  
  
  # This is the function that gets returned by the anonymous function and
  # becomes fibM.
  function(n) {
    
    nc <- as.character(n)
    
    # Handle "vectors" by element
    if (length(n) > 1) {
      return(sapply(n, fibM))
    }
    
    # Check to see if n is an integer Do not use is.integer as that is very
    # strict
    if (round(n, 0) != n) 
      return(NA)
    
    # Cached cases
    if (cache_has_key(nc)) 
      return(cache_get(nc))
    
    # Negative numbers
    if (n < 0) 
      return(fibM(-1 * n) * ((-1)^((n + 1)%%2)))
    
    # Everything else
    out <- fibM(n - 1) + fibM(n - 2)
    cache_set(nc, out)
    return(out)
    
  }
  
})()

system.time(fibM(20))
