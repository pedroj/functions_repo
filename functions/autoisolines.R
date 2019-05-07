# ----------------------------------------------------------------
# autoisolines: Code for automatically plotting isolines of
# effectiveness landscapes.
# Based on code for plotting effectiveness landscapes by Pedro 
# Jordano and code for automatic calculation of isolines 
# by Bernardo Santos.
# 3 December 2013. UNESP, Rio Claro, Brazil. Pedro Jordano.
# ----------------------------------------------------------------
## First version 12 Jan 2009. Revised 3 December 2013
# ----------------------------------------------------------------
# DESCRIPTION:
# The script plots effectiveness landscapes as described in
# Schupp, E. W., Jordano, P. and Gomez, J.M. 2010. Seed dispersal
# effectiveness revisited: a conceptual review. New Phytologist
# 188: 333-353.
# ----------------------------------------------------------------
# Notes to do
# - Implementing an option for setting the number of desired 
# isolines.
# ----------------------------------------------------------------
autoisolines <- function(qty, qlty, nlines=10)  {
# 
nlines <- 10 # number of isolines wanted
alfa <- max(qlty)/max(qty) # slope of a straight line linking (left,bottom) to (right,above) corners of the graphic
xval <- seq(0, max(qty), length.out=(nlines+1))[2:(nlines+1)] # sequence of (nlines) regular spaced x values for the isoclines
isoc <- (xval*xval*alfa) # values of the isoclines

vis1<-seq(0,2,length.out=1000)
for(i in 1:nlines)
{
  lines(vis1, isoc[i]/vis1)
  text(0.9*max(qty), 
       isoc[i]/(0.9*max(qty)), 
	   paste("QC = ", 
	   round(isoc[i], 
	   digits=1)), 
	   col="red")
}
