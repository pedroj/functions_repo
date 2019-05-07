# Parameters. SPECIES LEVEL ---------------------------------------------
# Function to estimate network parameters for species level.
# Obtain basic parameters from an interaction matrix
# From library(bipartite) functions. We are getting just 8 indexes.
require(bipartite)
params_sp <- function(mat)
{specieslevel(mat, #tapis
    index=c(# "species",
            "degree", #1-k
            # "normalised degree", #2-Nk
            # "dependence",
            "species strength", #2-St
            "interaction push/pull", #3-Ipp
            # "PDI",
            # "resource range",
            "species specificity", #4-SpSp
            "PSI", #5-PSI
            # "node specialisation index",
            # "betweeness",
            # "weighted betweenness",
            # "closeness", 
            # "weighted closeness",
            # "Fisher alpha",
            "diversity") #6-H
            # "effective partners",
            # "proportional generality",
            # "proportional similarity",
            # "d"
            )
}
#------------------------------------------------------------------------
