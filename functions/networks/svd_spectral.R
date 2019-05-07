# -----------------------------------------------------------------------
# [Title]: svd.spectral
# [Date]:      [Loc]: 
# Pedro Jordano.
# -----------------------------------------------------------------------
# Function to estimate the spectral profile of adjacency matrices
# Here it is adapted for the eigenvalues of the adjacency matrix 
# of a network. For binary matrices.
# I'm using function svd.
# -----------------------------------------------------------------------
## First version 21 March 2013. Revised DATE
# -----------------------------------------------------------------------
# FUNCTION  --------
svd.spectral <- function(mat, nperm=1000)  {
    require(boot)
    require(ggplot2)
    # Requires sourcing the multiplot function for ggplot2
    source('~/Documents/Working/~RCode/MyRCode/functions/startup/multiplot.R')    
    # Requires sourcing the bdiag function
    source('~/Documents/Working/~RCode/MyRCode/functions/networks/bdiag.R')    
#    ml <- list(mat,t(mat))
#    ml1 <- bdiag(ml)    # OK
    
    # Eigenvalues for non-square matrices with singular 
    # value decomposition.
    eig <- svd(mat)             # Values stored in $d
    zz <- data.frame(append(as.vector(eig$d),as.vector(-eig$d)))  
    # For spectral analysis
    colnames(zz)<- "values"
 
    # Eigenvalues histogram ---------------------------------------------
    p1 <- ggplot(zz, aes(x=values))
    p1<- p1 + geom_histogram(aes(y = ..density..),
                             binwidth = 0.5, fill="red",alpha=0.3) +
        geom_density(alpha=0.3)
    x<-c(1:length(eig$d))
    y<-eig$d
    gg<-data.frame(cbind(x,y))
    p2<-ggplot(gg, aes(x=x, y=y)) + 
        geom_line() + geom_point() 
    multiplot(p2, p1, cols=2)
}
