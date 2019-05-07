eigenboot <- function(mat, nperm=10000)  {
#-----------------------------------------------------------------------
# Function to estimate the igenvalues of random matrices
## Bootstrapping the eigenvalues
#	also estimate the 95th percentile   
#	of the bootstrap dist'n of the mean, and
#	its jackknife-after-bootstrap  standard error  
require(boot)
require(ca)
require(vegan)
N<-dim(mat)[1]+dim(mat)[2]
mat.cor<-ca(mat)
cacoord<-rbind(mat.cor$rowcoord,mat.cor$colcoord)
xdata<-t(cacoord)
ev<-eigen(vegdist(xdata))
boot.eig <- function(x,i){
 	(eigen(vegdist(xdata[i,]), symmetric = T, only.values = T)$values)
  	}
### COMPLETE MATRIX
cm<-boot(xdata,boot.eig,R=nperm,parallel="multicore")
# cm$t will contain the bootstraped values (rows) for each 
# eigenvalue (column).
# Plot with error bar ranges
x<-c(1:length(ev$values))
y<-ev$values
ucl<-ev$values+(sd(ev$values)/sqrt(N))
lcl<-ev$values-(sd(ev$values)/sqrt(N))
plot(x,y, xlim=c(1,16), ylim=c(-400,400),
     pch=19,type="o",xlab="Rank",ylab="Eigenvalues")
arrows(x,ucl,x,lcl,length=.05,angle=90,code=3) 
}
