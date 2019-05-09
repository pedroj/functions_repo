# Function to parse an adjacency matrix into a graph
# Weighted data
get.igraph.wt <- function (mat) {
    # Creating the graph from a weighted incidence data with names:
    graph.incidence(mat, directed=F, weighted=T, add.names=NULL)
}
