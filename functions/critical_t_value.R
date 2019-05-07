# Critical t value
critical.t <- function(){
    cat("\n","\bEnter Alpha Level","\n")
    alpha<-scan(n=1,what = double(0),quiet=T)
    cat("\n","\b1 Tailed or 2 Tailed:\nEnter either 1 or 2","\n")
    tt <- scan(n=1,what = double(0),quiet=T)
    cat("\n","\bEnter Number of Observations","\n")
    n <- scan(n=1,what = double(0),quiet=T)
    cat("\n\nCritical Value =",qt(1-(alpha/tt), n-2), "\n")
}
critical.t()
