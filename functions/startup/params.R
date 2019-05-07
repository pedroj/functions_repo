# Parameters ------------------------------------------------------------
# Function to estimate network parameters.
require(bipartite)
params <- function(mat)
# Obtain basic parameters from an interaction matrix
# From library(bipartite) functions. We are getting just 8 indexes.
{networklevel(mat, #tapis
    index=c(# "species",
            "connectance", #1-C
            # "web asymmetry", #2-WA
            # "links per species",
            # "number of compartments",
            # "compartment diversity",
            # "cluster coefficient",
            # "weighted cluster coefficient", #2-WCC
            # "degree distribution",
            # "mean number of shared partners",
            # "togetherness",
            # "C score",
            # "V ratio",
            # "discrepancy",
            "nestedness", #2-N
            # "weighted nestedness",
            "weighted NODF", #3-wNODF 
            # "extinction slope",
            # "robustness",
            "ISA", #4-ISA
            # "SA",
            # "niche overlap",
            # "generality,vulnerability",
            "linkage density", #5-LD
            # "Fisher alpha",
            # "mean interaction diversity",
            # "interaction evenness", #7-IE
            # "Alatalo interaction evenness",
            "Shannon diversity"), #6-H
            # "H2"
    ISAmethod="Bascompte", SAmethod = "log", CCfun=median, 
    normalise=TRUE, empty.web=TRUE, logbase="e", intereven="sum")
}
#------------------------------------------------------------------------
