igraph2tnet<- function (mynet) {
    # Extract edgelist, and add a column of 1's
    # Extract edgelist and a 1 to the node id's as igraph starts 
    # from 0 while tnet 1.
    net <- cbind(get.edgelist(mynet, names=FALSE), E(mynet)$weight)
    
    # For undirected networks, the symmetrise function must be run
    if(!is.directed(mynet))
        net <- symmetrise_w(net)
    
    # Ensure that it conforms to the tnet standard
    net <- as.tnet(net, type="weighted two-mode tnet")
}
