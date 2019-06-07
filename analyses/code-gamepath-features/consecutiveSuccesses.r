consecutiveSuccesses <- function(x, n, cut = 0.5)
{
    if (length(x) < n)
        return(rep(0,length(x)))
    M <- sapply(1:(n-1), function(z)
        c(rep(NA,z), head(x, -z)), simplify = T)
    M <- cbind(x,M)

    M <- M > cut

    return(as.numeric(rowSums(M) >= n))
}