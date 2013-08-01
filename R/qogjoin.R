
#' Join historical and recent states in QOG Standard time series data
#'
#' Function to plot maps of Quality of Government (QOG) data. Requires the \code{ggplot2} and \code{maps} packages.
#'
#' @export
#' @param data a QOG Standard time series dataset, or any data frame with \code{cname} (country) and \code{year} information coded as in the QOG Standard time series dataset.
#' @param country the country name to join data over. Requires the \code{cname} variable.
#' @param out what to return. If set to \code{"data"}, the entire dataset is returned, stripped of the older observations and update to the new series. See 'Details'.
#' @details The function will try to find two series of country-year observations that both match the \code{country} argument. Within the QOG Standard time series dataset, this will match historical states like "France (-1962)" to modern states like "France (1963-)". The function will then create a new variable out of both series, joined at their separation years, and set its country code attributes to the most recent ones. See Appendix A of the \emph{QOG Standard Codebook} for details on historical states in the QOG Standard time series dataset.
#' @value a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren Holmberg, 
#' Bo Rothstein, Petrus Sundin & Richard Svensson. 2013. 
#' \emph{The Quality of Government Dataset}, version 15May13. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \url{http://www.qog.pol.gu.se}.
#' @examples
#' # Load QOG Standard time series data.
#' QOG = qogdata(tempfile(fileext = ".dta"), format = "ts", 
#'               variables = c("cname", "wdi_gdp", "wdi_pop"))
#' # By default, the function returns the joined data.
#' head(qogjoin(QOG, "France"))
#' # Set out to "data" to get the modified dataset.
#' QOG <- qogjoin(QOG, "Pakistan", out = "data")
#' QOG <- qogjoin(QOG, "Ethiopia", out = "data")
#' QOG <- qogjoin(QOG, "Malaysia", out = "data")

qogjoin <- function(data, country = NULL, out = "variable") {
  stopifnot("cname" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(is.character(country))
  x = data$cname[grepl(country, QOG$cname)]
  x = unique(x)
  if(length(x) == 0)
    stop("No country match for ", country)
  else if(length(x) == 1)
    stop("Single country match for ", country)
  else if(length(x) > 2)
    stop("More than two country matches: ", paste0(x, collapse = ", "))
  
  y = as.numeric(gsub("\\D", "", x))
  min = x[order(y) == 1]
  max = x[order(y) == 2]
  message("Joining ", min, " to ", max)
  
  one = data[data$cname == min & data$year <= y[order(y) == 1], ]
  two = data[data$cname == max & data$year >= y[order(y) == 2], ]
  new = rbind(one, two)
  
  new$cname = country

  # country codes
  if("ccode" %in% names(data))
    new$ccode = unique(data$ccode[data$cname == max])
  if("ccodealp" %in% names(data))
    new$ccodealp = unique(data$ccodealp[data$cname == max])
  if("cname_year" %in% names(data))
    new$cname_year = paste(new$cname, new$year)
  if("ccodealp_year" %in% names(data))
    new$ccodealp_year = paste(new$ccodealp, new$year)
  if("ccodewb" %in% names(data))
    new$ccodewb = unique(data$ccodewb[data$cname == max])
  
  message(country, " now runs over ", nrow(new), " country-year observations, ",
          min(new$year), "-", max(new$year))
  if(out == "data") {
    data[data$cname == max, ] = new
    data = subset(data, cname != min)
  }
  else {
    return(new)
  }
}
