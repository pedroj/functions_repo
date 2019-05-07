dd<- function (web, plot.it = TRUE, pure.call = TRUE) 
{
    web <- (web > 0) * 1
    k <- sum(web)
    S <- sum(dim(web))
    ddlower <- rowSums(web)
    ddhigher <- colSums(web)
    Plo <- sapply(sort(ddlower), function(x) sum(ddlower >= 
        x))
    Plower <- cbind.data.frame(k = sort(ddlower), P = Plo/max(Plo))
    if (nrow(Plower) < 5) 
        warning("Too few data points (< 5)! Fitting makes no sense!")
    Phi <- sapply(sort(ddhigher), function(x) sum(ddlower >= 
        x))
    Phigher <- cbind.data.frame(k = sort(ddhigher), P = Phi/max(Phi))
    if (max(Phigher) < 5)
        warning("Too few data levels of degrees (< 5)! Fitting makes no sense!")
    fitdd <- function(...) {
        EXP <- try(nls(P ~ exp(-gamma * k), start = list(gamma = 1), 
            ...))
        PL <- try(nls(P ~ k^(-gamma), start = list(gamma = 1), 
            ...))
        TPL <- try(nls(P ~ (k^(-gamma)) * exp(-k/kx), start = list(gamma = 1, 
            kx = 1), ...))
        list(EXP, PL, TPL)
    }
    fitl <- fitdd(data = Plower)
    fith <- fitdd(data = Phigher)
    indexl <- which(sapply(fitl, function(x) !inherits(x, "try-error")) != 
        0)
    fitnew <- fitl[indexl]
    indexh <- which(sapply(fith, function(x) !inherits(x, "try-error")) != 
        0)
    fithnew <- fith[indexh]
    if (plot.it) {
        plotfit <- function(data, fit, ...) {
            plot(data$P ~ data$k, log = "xy", pch = 16, xlab = "Number of links (k)", 
                ylab = "Cumulative distribution P(k)", ...)
            for (i in 1:length(fit)) {
                lines(seq(1, max(ddlower), by = 0.1), predict(fit[[i]], 
                  newdata = data.frame(k = seq(1, max(ddlower), 
                    by = 0.1))), col = paste("grey", i * 20, 
                  sep = ""), ...)
            }
            abline(h = 1, lty = 2)
        }
        if (pure.call) 
            par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
        plotfit(data = Plower, fit = fitnew, lwd = 2, cex = 1.5, 
            cex.lab = 1.8, ylim = c(0.001, 1), main = "Animals")
        plotfit(data = Phigher, fit = fithnew, lwd = 2, cex = 1.5, 
            cex.lab = 1.8, ylim = c(0.001, 1), main = "Plants")
    }
    res.out <- matrix(ncol = 5, nrow = 3)
    rownames(res.out) <- c("Exponential", "Power law", "Trunc. power law [slope]")
    colnames(res.out) <- c("Estimate", "Std. Error", "Pr(>|t|)", 
        "R2", "AIC")
    resl.out <- res.out
    resl2 <- t(sapply(fitl[indexl], function(mod) try(c(R2 = cor(eval(mod$data)$P, 
        predict(mod)), AIC = AIC(mod)))))
    resl.out[indexl, 4:5] <- resl2
    resl1 <- t(sapply(fitl[indexl], function(mod) coef(summary(mod))[1, 
        c(1, 2, 4)]))
    resl.out[indexl, 1:3] <- resl1
    resh.out <- res.out
    resh2 <- t(sapply(fith[indexh], function(mod) try(c(R2 = cor(eval(mod$data)$P, 
        predict(mod)), AIC = AIC(mod)))))
    resh.out[indexh, 4:5] <- resh2
    resh1 <- t(sapply(fith[indexh], function(mod) coef(summary(mod))[1, 
        c(1, 2, 4)]))
    resh.out[indexh, 1:3] <- resh1
    list("Animals degree distr. fits" = resl.out, "Plants degree distr. fits" = resh.out)
}
