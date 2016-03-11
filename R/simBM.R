#' Simulate 2d Brownian motion
#'
#' Simulate 2d Brownian motion
#'
#' @param n sample size
#' @param sigma standard deviation of steps
#'
#' @return a two column matrix with n rows
#'
#' @export
#'
#' @examples
#' x <- simBM(1000)
#' plotBM(x)
simBM <-
    function(n, sigma=1)
{
    stopifnot(n >= 2)
    stopifnot(sigma >= 0)

    x <- matrix(rnorm(2*n, 0, sigma), ncol=2)
    colnames(x) <- c("x", "y")

    apply(x, 2, cumsum)
}


#' Plot 2d Brownian motion
#'
#' Plot 2d Brownian motion
#'
#' @param x matrix with 2 columns
#' @param pointcolor color of points at start and end; If NULL
#     points are omitted
#' @param type passed to plot()
#' @param ...  passed to plot()
#'
#' @export
#'
#' @examples
#' x <- simBM(1000)
#' plotBM(x)
plotBM <-
    function(x, pointcolor=c("slateblue", "violetred"), type="l", ...)
{
    stopifnot(is.matrix(x) && ncol(x)==2 && nrow(x) > 2)

    plot(x, type=type, ...)
    if(!is.null(pointcolor))
        points(x[c(1, nrow(x)),], pch=21, bg=pointcolor)
}
