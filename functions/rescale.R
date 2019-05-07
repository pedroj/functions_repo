# Function to rescale a vector, x, so that the minimum value = smallest and 
# the maximum = largest
rescale <- function(x, smallest, largest){
	y <- (x - min(x))/(max(x)-min(x))*(largest-smallest)+smallest
	return(y)
}
