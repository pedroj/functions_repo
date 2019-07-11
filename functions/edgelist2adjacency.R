### Script for converting edgelist to adjacency matrix -------------------------
# Read the edgelist
# dat <- read.csv(file="edgelist_numbered.csv",header=T, as.is=T) 
# Edge list as numeric values ID nodes. 

edgelist2adjacency <- function(dat) {
  require(Matrix)
  # Define the network data
  dat<- as.matrix(dat)
  
  # Row to be lower-level (e.g., plants); columns to be upper-level (e.g., animals)
  A <- spMatrix(nrow= length(unique(dat[,1])),
                ncol= length(unique(dat[,2])),
                i= as.numeric(factor(dat[,1])),
                j= as.numeric(factor(dat[,2])),
                x= rep(1, length(as.numeric(dat[,1]))) )
  row.names(A) <- levels(factor(dat[,1]))
  colnames(A)  <- levels(factor(dat[,2]))
  A
  
  summary(A)
  matrix <- as.matrix(A)
  
  # Save the new file (adjacency matrix)
  write.csv(matrix, "2modematrix.csv")
}
