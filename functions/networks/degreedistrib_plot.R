degreedistrib<- function(mat){
    # I convert to 0-1 network
    mat <- (mat > 0) * 1
    # I transpose the matrix for ER code
    mat<-t(mat)             ### Just in case we need transposing the matrix
    ### for Rows to be Lower level (LL); Columns to be Higher level (HL)
    #------------------------------------------------------------------------
    # Higher level (HL)
    na<-length(t(mat)[,1])	# the number of animal species
    ka<-as.vector(colSums(mat))		# Vector of degree values for animals
    Pka <- sapply(sort(ka), function(x) sum(ka >= 
                                                x))
    Pkadat <- cbind.data.frame(k = sort(ka), P = Pka/max(Pka))
    if (nrow(Pkadat) < 5) 
        warning("Too few data points (< 5)! Fitting makes no sense!")
    #------------------------------------------------------------------------
    # Lower level (LL)
    np<-length(mat[,1])     # the number of plant species
    kp<-rowSums(mat) 		# Vector of degree values for plants
    Pkp <- sapply(sort(kp), function(x) sum(kp >= 
                                                x))
    Pkpdat <- cbind.data.frame(k = sort(kp), P = Pkp/max(Pkp))
    if (max(Pkpdat) < 5) 
        warning("Too few data levels of degrees (< 5)! Fitting makes no sense!")
    #------------------------------------------------------------------------
    ### Plots
    par(mfrow=c(2,2))
    ### ANIMALS
    ax<-Pkadat$k
    ay<-Pkadat$P
    hist(ka,xlab="Degree (k), Higher level", ylab="Number of species",
         main=NULL,col=1,border=8,breaks=c(0:max(ka)),plot=T)
    
    ### Plots
    xv<-seq(1,max(ka),0.1)
    plot(ax,ay,log="xy",xlim=c(1,na),ylim=c(0.0001,1), 
         xlab="Number of links (k, Higher level)", 
         ylab="Mean cumulative distribution P(k)",pch=16)
    #lines(ax, ay, col='blue')
    ### Plants
    hist(kp,xlab="Degree (k), Lower level", ylab="Number of species",
         main=NULL,col=1,border=8,breaks=c(0:max(kp)),plot=T)
    px<-Pkpdat$k
    py<-Pkpdat$P
    plot(px,py,log="xy",xlim=c(1,np),ylim=c(0.0001,1),
         xlab="Number of links (k, Lower level)",
         ylab="Mean cumulative distribution P(k)",pch=16)
}