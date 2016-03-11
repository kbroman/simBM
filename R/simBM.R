# Simulate 2d Brownian motion
simBM <-
    function(n, sigma=1)
{
    stopifnot(n >= 2)
    stopifnot(sigma >= 0)

    x <- matrix(rnorm(2*n, 0, sigma), ncol=2)
    colnames(x) <- c("x", "y")

    apply(x, 2, cumsum)
}


# Plot 2d Brownian motion
plotBM <-
    function(x, pointcolor=c("slateblue", "violetred"), type="l", ...)
{
    stopifnot(is.matrix(x) && ncol(x)==2 && nrow(x) > 2)

    plot(x, type=type, ...)
    if(!is.null(pointcolor))
        points(x[c(1, nrow(x)),], pch=21, bg=pointcolor)
}
