#' Plot availablity of panel data
#'
#' Function to plot data availability in time series cross-sectional (TSCS) data. 
#' Requires the \code{ggplot2} package.
#'
#' @export
#' @param data a TSCS dataset.
#' @param variable the variable to plot.
#' @param id the variable to label the groups.
#' @param time the variable that codes for the time period.
#' @details The auto-detection of time limits is buggy.
#' @value a \code{ggplot2} object
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Load QOG Standard time series data.
#' QOG = qogdata(tempfile(fileext = ".dta"), version = "bas", format = "ts", 
#'               variables = c("ccodealp", "year", "wdi_gdpc", "ihme_nm"))
#' # Availablity of gross domestic product per capita.
#' panel(QOG, "wdi_gdpc")
#' # Availablity of neonatal mortality rate.
#' panel(QOG, "ihme_nm")

panel <- function(data = NULL, variable = NULL, id = "ccodealp", time = "year") {
  stopifnot(!is.null(variable))
  stopifnot(variable %in% names(data))
  x = !is.na(data[, variable])
  r = aggregate(as.numeric(x) ~ data[, time], mean, data = NULL)
  r$percent = 100 * r[, 2]
  r = subset(r, percent > 0)[, 1]
  r = range(r)
  if(require(ggplot2)) {
    r = ggplot(data = NULL, aes(
      x = data[, time], 
      y = reorder(data[, id], as.numeric(x), mean), 
      fill = x)) +
      geom_tile(size = 6) + 
      scale_fill_discrete("", labels = c("missing", "nonmissing")) + 
      labs(y = "Country", x = NULL) + 
      theme(legend.position = "bottom") + 
      xlim(r)    
  }
  return(r)
}
