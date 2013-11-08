#' Plot autocorrelation functions for cross-sectional time series
#'
#' Plot ACF or PACF.
#'
#' @export
#' @param data a data frame carrying an \code{\link{xtdata}} attribute.
#' @param variable the variable to plot.
#' @param name the label for ACF > 0 [TODO: add real documentation here].
#' @return a half-baked plot
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @keywords xt ts graphics
xtacf <- function(data, variable, name = "acf > 0", type = "correlation") {
  try_require(c("ggplot2"))
  stopifnot(xtdata(data))
  stopifnot(variable %in% names(data))

  p.acf = sapply(unique(data[, xt(data)$data[1]]), function(cty, type = type) { 
    exp = qs[ == cty, variable]
    if(length(na.omit(exp)) > 1)
      acf(na.omit(exp), plot = FALSE, type = type)$acf
    else
      NA
  })
  
  p.acf = as.data.frame(p.acf)
  p.acf$t = as.numeric(rownames(p.acf))
  p.acf = melt(p.acf, id = "t")
  names(p.acf) = c("lag", "group", "acf")
  
  qplot(data = p.acf, y = acf, yend = 0, x = lag, xend = lag, 
        colour = acf > 0, geom = "segment") + 
    geom_point() +
    geom_hline(y = 0) +
    scale_colour_discrete(name = name) +
    facet_wrap(~ group) +
    theme_minimal()
  
}
