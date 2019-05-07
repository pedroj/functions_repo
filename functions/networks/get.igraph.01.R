# Function to parse an adjacency matrix into a graph
# Weighted data
get.igraph.01 <- function (mat) {
    # Creating the graph from a weighted incidence data with names:
    graph.incidence(mat, directed=F, weighted=F, add.names=NULL)
}
