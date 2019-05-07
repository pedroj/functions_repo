modularity <- function (mat) {
    require(bipartite)
    (res <- computeModules(mat)) # takes several minutes!
    printoutModuleInformation(res)
    res@likelihood
    plotModuleWeb(res,labsize=0.8)
    # Null model test of modularity
    #     nulls <- nullmodel(mat, N=10, method="r2d")
    #     modules.nulls <- sapply(nulls, computeModules)
    #     like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
    #     z <- (mod@likelihood - mean(like.nulls))/sd(like.nulls)
    #     z
}
