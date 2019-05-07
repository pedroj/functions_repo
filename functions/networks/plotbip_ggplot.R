#-------------------------------------------------------------------------
# Draft code for plotting a bipartite network in ggplot2
#.........................................................................
require(network)
require(ggplot2)
require(sna)
require(ergm)
plotg <- function(net, value=NULL) {
    m <- as.matrix.network.adjacency(net) # get sociomatrix
    # get coordinates from Fruchterman and Reingold's force-directed placement algorithm.
    plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL)) 
    # or get it them from Kamada-Kawai's algorithm: 
    # plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL)) 
    colnames(plotcord) = c("X1","X2")
    edglist <- as.matrix.network.edgelist(net)
    edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
    colnames(edges) <-  c("X1","Y1","X2","Y2")
    edges$midX  <- (edges$X1 + edges$X2) / 2
    edges$midY  <- (edges$Y1 + edges$Y2) / 2
    # plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
    pnet <- ggplot()  + 
        geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), 
                     data=edges, colour="grey") +
        geom_point(aes(X1, X2, size=6, 
                       color=c(rep("grey90",dim(m)[1]),
                               rep("grey60",dim(m)[2]))), 
                   data=plotcord) +
        scale_colour_brewer(palette="Set1") +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        # discard default grid + titles in ggplot2 
        theme(panel.background = element_blank()) + 
        theme(legend.position="none") +
        theme(axis.title.x = element_blank(), 
              axis.title.y = element_blank()) +
        theme(legend.background = element_rect(colour = NA)) + 
        theme(panel.background = element_rect(fill = "white", 
                                            colour = NA)) + 
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())
    return(print(pnet))
}

