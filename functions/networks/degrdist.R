#
# Degree distribution
#
# Assign dataset as, e.g., degreedistrib(mat), where mat is any binary matrix
# with no row/col labels.
#
degreedistrib<-function(data, fit, ...) {
# For plants (higher trophic level)
    ddhigher <- colSums(mat)     # Plants
    cm<-max(colSums(mat)) # Maximum degree for plants
    csh <- sapply(sort(ddhigher), function(x) sum(ddhigher >= 
        x))
    hih <- hist(csh, -1:max(csh), plot = FALSE)$intensities
    resh <- rev(cumsum(rev(hih)))

# For animals (lower trophic level)
    ddlower <- rowSums(mat)      # Animals
    rm<-max(rowSums(mat))
    csl <- sapply(sort(ddlower), function(x) sum(ddlower >= 
        x))
    hih <- hist(csl, -1:max(csl), plot = FALSE)$intensities
    resl <- rev(cumsum(rev(hih)))
# Plots
plot(resh, log="xy", xlab="Degree", ylab="Cumulative frequency",
     col=1, main="Nonlinear preferential attachment",type="n",
     xlim=c(1,max(cm,rm)+50))
     points(resh, cex = 1, col = "dark green")
     points(resl, cex = 1, col = "dark red")
}

