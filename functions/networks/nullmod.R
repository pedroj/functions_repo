nullmod <- function (mat) {
    require(bipartite)
    nulls <- nullmodel(mat, N=100, method="r2d")
    modules.nulls <- sapply(nulls, computeModules)
    like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
    z <- (mod@likelihood - mean(like.nulls))/sd(like.nulls)
    z
}
