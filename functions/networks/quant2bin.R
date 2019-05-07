# Transformation of quantitative matrix into binary
# Original R code from Diego Vazquez.
# source("~/R/diego/R/quant2bin.R")

quant2bin<-function(matr)
{
  cn=colnames(matr)
  rn=rownames(matr)
  ij<-which(matr!=0,arr.ind=T)
  matrb<-matrix(0,dim(matr)[1],dim(matr)[2])
  matrb[ij]<-1
  colnames(matrb)=cn
  rownames(matrb)=rn
  return(matrb)
}
