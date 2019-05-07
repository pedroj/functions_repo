freqplot<- function(mat){
    mat1<-network.initialize(dim(mat)[1]+dim(mat)[2],
                             bipartite=c(dim(mat)[1]),directed=F)
    mat1<-network.bipartite(as.matrix(mat),mat1)
    #-------------------------------------------------------------------------
    # Degree
    # Lower level
    # Lower level (LL) species are in rows. Higher level (HL) species are 
    # in columns.
    na<-length(mat[,1])	            # The number of lower-level species
    ka<-as.vector(rowSums(mat))		# Vector of degree values for LL
    #
    # Higher level
    np<-length(t(mat)[,1])  	        # the number of plant species
    kp<-as.vector(colSums(mat))     # Vector of degree values for plants
    #-------------------------------------------------------------------------
    par(mfrow=c(1,2))
    loglog.plot(ka,xlab="k",ylab="Number of species",main="Lower level",pch=19)
    loglog.plot(kp,xlab="k",ylab="Number of species",main="Higher level",pch=16)
}
