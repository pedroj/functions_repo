# ----------------------------------------------------------------------
# [Title]: Function to compare the eigenvalues distribution of adjacency
# matrices based on bootstraped SE estimation, from a singular value 
# decomposition.
# [Date]: 12 Jan 2013     [Loc]: Sevilla
# Pedro Jordano.
# ......................................................................
# NOTE: # Requires sourcing the multiplot function for ggplot2
source('~/Documents/Working/~RCode/MyRCode/functions_repo/functions/startup/multiplot.R')
# Requires library(nFactors)
## First version DATE. Revised DATE  4 Mar 2013
# ----------------------------------------------------------------------
# m1 should be the Pre-Apis and m2 the Apis, adjacency matrices for the 
# networks in the two periods to be compared. Or any two matrices to be 
# compared. Then give a title (char string) for the graph (optional).
eigen.boot.comp <- function (m1,m2,my_title) {
    require(nFactors)
# Assign the matrixes --------------------------------------------------
eig1<-  svd(m1)      
eig1 <- eig1$d
eig2<- svd(m2)
eig2<- eig2$d
# ----------------------------------------------------------------------
# Bootstrap distributions study of the eigenvalues
eigBoot1<-eigenBootParallel(m1, quantile=0.95, nboot=1000,
                              option="bootstrap",
                              cor=F, model="components")
eigBoot2<-eigenBootParallel(m2, quantile=0.95, nboot=1000,
                               option="bootstrap",
                               cor=F, model="components")
# Plots with ggplot2. ---------------------------------------------------
# We first bind the index of the eigenvalues with their observed value.
# We trimm eigenvalues <0.
# For pre-Apis period.
zz1 <- data.frame(cbind(x= c(1:length(eig1[eig1>=0])), 
                        y= eig1[eig1>=0]))
zz1 <- data.frame(cbind(zz1,mean=eigBoot1$mean[1:dim(zz1)[1]],
                        sd= eigBoot1$mean[1:dim(zz1)[1]]))
# For Apis period.
zz2 <- data.frame(cbind(x= c(1:length(eig2[eig2>=0])), 
                        y= eig2[eig2>=0]))
zz2 <- data.frame(cbind(zz2,mean=eigBoot2$mean[1:dim(zz2)[1]],
                        sd= eigBoot2$mean[1:dim(zz2)[1]]))
# Plots. Rank order of eigenvalues with bootstraped CI's. ---------------
# Pre-Apis Observed eigenvalues
p1 <- ggplot(data= zz1, aes(x= x, y= y)) + 
    geom_point(colour="blue") +
    geom_line(colour="blue") +
    geom_errorbar(aes(ymin=y-sd/sqrt(100), 
                      ymax=y+sd/sqrt(100)), 
                      colour="blue", width=0.5) +
    geom_point(aes(ymin=y-sd/sqrt(100), 
                   ymax=y+sd/sqrt(100)),
               size=2, shape=21, fill="blue",colour="blue") + 
    xlab("Order of eigenvalues") +
    ylab("Eigenvalues") +
    labs(title=my_title)  +  
    # Apis Observed eigenvalues
    geom_point(data= zz2, aes(x= x, y= y), colour="red") +
    geom_line(data= zz2, aes(x= x, y= y), colour="red") +
    geom_errorbar(data= zz2, aes(ymin=y-sd/sqrt(100), 
                                 ymax=y+sd/sqrt(100)), 
                  colour="red", width=0.5) +
    geom_point(data= zz2, aes(ymin=y-sd/sqrt(100), 
                              ymax=y+sd/sqrt(100)),
               size=2, shape=21, fill="red",colour="red") 

# Spectral distribution of the two matrices. ----------------------------
z1 <- data.frame(append(as.vector(eig1),as.vector(-eig1)))  
colnames(z1)<- "values"
z2 <- data.frame(append(as.vector(eig2),as.vector(-eig2)))  
colnames(z2)<- "values"
# Eigenvalues histogram -------------------------------------------------
p2 <- ggplot(z1, aes(x=values)) +
    # geom_histogram(aes(y = ..density..),
    geom_density(alpha=0.1, colour="blue", fill="blue") +
    geom_density(data= z2, aes(x=values, y = ..density..),
                 alpha=0.1, colour="red", fill="red") +
    #   xlim(-15, 15)
    xlim(min(min(z1),min(z2))-1, max(max(z1),max(z2))+1)
#........................................................................
# Requires sourcing the multiplot function for ggplot2
source('~/Documents/Working/~RCode/MyRCode/functions_repo/functions/startup/multiplot.R')    
multiplot(p1, p2, cols=2)
}
# -----------------------------------------------------------------------
