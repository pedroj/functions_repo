#-------------------------------------------------------------------------
# Randomization
netdiff <- function(mat, rndmz=500)
# Obtain basic parameters from an interaction matrix given sample data for 
# individual plants in matrix form (mat).
# rndmz is the required number of randomizations.
# From library(bipartite) functions.
{
# Inits
require(bipartite)
diffs<-NULL   # To store the differences between pre and apis parameters
rndmz<-500     # Number of randomizations required
#-------------------------------------------------------------------------
# Get parameters
params <- function(mat)
# Obtain basic parameters from an interaction matrix
# From library(bipartite) functions.
{networklevel(mat, #tapis
    index=c("connectance", #1-C
            "web asymmetry", #2-WA
#           "links per species",
#           "number of compartments",
            "weighted cluster coefficient", #3-WCC
            "nestedness", #4-N
            "weighted NODF", #5-wNODF 
            "linkage density", #6-LD
#           "Fisher alpha",
            "interaction evenness", #7-IE
            "diversity"), #8-H
    ISAmethod="Bascompte", SAmethod = "log", CCfun=median, 
    normalise=TRUE, empty.web=TRUE, logbase="e", intereven="sum")
}
#-------------------------------------------------------------------------
for (i in 1:rndmz) {
# Iterate trough gen list to get object subsets and insert a randomized period variable to reshuffle "pre" and "apis".
# Initialize the containers
tpre<-matrix(nrow=dim(mat)[2]-2, ncol=0) # To store the resampled matrix
tapis<-matrix(nrow=dim(mat)[2]-2, ncol=0)
for (i in seq_along(gen)) {
     tt<-mat[mat$species==gen[i],]     # Subset a genus
     n<-dim(tt)[1]            # Get the number of cases for genus i
     # Vector of random period labels
     rperiod<- sample(c("pre","apis"),n,replace = T)
     label<-gen[i]            # Label of the i genus
# cbind subset and new random period labels
     tt<-cbind(tt,rperiod)
# Average no. visits by randomized period for this genus and append
     ttpre<-as.matrix(apply(tt[tt$rperiod=="pre",c(3:66)],2,mean))
     tpre<-cbind(tpre,ttpre)
     ttapis<-as.matrix(apply(tt[tt$rperiod=="apis",c(3:66)],2,mean))
     tapis<-cbind(tapis,ttapis)
}
     colnames(tpre)<-gen
     colnames(tapis)<-gen

pre<-params(tpre)  # Parameters for randomized pre
pos<-params(tapis) # Parameters for randomized apis
tdiffs<-pre-pos # This stores the difference between pre and apis parameters
diffs<-append(diffs,tdiffs)
}
#-------------------------------------
# Outputs parameter differences values as dataframe.
df <- data.frame(matrix(unlist(diffs), nrow=rndmz, byrow=T))
colnames(df)<-c("C","WA","WCC","N","wNODF","LD","IE","H")
names<-c("C","WA","WCC","N","wNODF","LD","IE","H")
mnames<-c("Connectance","Web asymmetry","Weighted cluster coeff.",
          "Nestedness","wt NODF","Linkage density",
          "Interaction evenness","Shannon diversity")

# Parameters for observed pre and apis matrices
# AVERAGE data matrices
obs.pre<-params(tei09avg[tei09avg$period=="pre",c(4:71)])
obs.apis<-params(tei09avg[tei09avg$period=="apis",c(4:71)])
obs.diffs<-obs.pre-obs.apis
obs.diffs <- data.frame(matrix(unlist(obs.diffs), nrow=1, byrow=T))
colnames(obs.diffs)<-c("C","WA","WCC","N","wNODF","LD","IE","H")

# RESULTS. Differences in network parameters for PRE-APIS comparison.
obs.diffs    # Values for observed networks
summary(df)  # Values for randomizations

# Histograms
par(mfrow=c(2,4))
for (i in 1:length(obs.diffs)) {hist(df[,i],main=names[i],xlab=mnames[i],
	                 col="lightblue2",border="lightblue3")
	            abline(v=obs.diffs[,i],col="red")}
# P values
z<-(obs.diffs-apply(df,2,mean))/(apply(df,2,sd)/sqrt(rndmz))
for (i in 1:8) {p<-2*pnorm(-abs(z[,i])); cat(p," ")}
#-------------------------------------------------------------------------
}

