# Function to parse an adjacency matrix into a graph
# Weighted data, directed.
# If matrix is not symmetric it draws all the reciprocal links.
get.igraph.wtd <- function (mat) {
    # Creating the graph from a weighted incidence data with names:
    graph.incidence(mat, directed=TRUE, weighted=TRUE, add.names=NULL)
}
