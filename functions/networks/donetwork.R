# Creates a netwrok object from an incidence matrix, as network
donetwork <- function (net) {
# ewt<-vectorize(net)  
mat1<-network.initialize(dim(net)[1]+dim(net)[2],
                         bipartite=c(dim(net)[1]),directed=F)
mat1<-network.bipartite(as.matrix(net),mat1)
return(mat1)
}

