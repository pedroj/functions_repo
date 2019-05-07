# Just tell me how big
# If you don't know if something is a vector or not, and want to see how 
# big it is regardless, use size(). It will also recurse through lists, 
# giving the size of each element.
size <- function(x, recurse=TRUE) {	
  if(is.recursive(x) & recurse) lapply(x, size)
  else { if(is.vector(x)) length(x) else dim(x) }
}
