
#' Lag or lead a panel data variable
#' 
#' Function to lag/lead a panel variable in a data frame with an 
#' \code{\link{xtdata}} attribute. Based on the \code{shift} function 
#' by TszKin Julian Chan.
#' 
#' You might want to use the \code{xtlag} and \code{xtlead} convenience wrappers:
#' \itemize{
#'   \item \code{xtlag} will perform a negative shift of \code{k} lags
#'   \item \code{xtlag} will perform a positive shift of \code{k} leads
#' }
#' 
#' @name xtshift
#' @aliases xtlag xtlead
#' @export
#' @param data a data frame carrying an \code{\link{xtdata}} attribute.
#' @param variable the variable to lag/lead.
#' @param k the number of lags/leads.
#' @seealso \code{\link{xtdata}}
#' @references Christopher Gandrud, "Slide: one function for lag/lead variables 
#' in data frames, including time-series cross-sectional data", 
#' \url{http://christophergandrud.blogspot.com/2013/05/slide-one-function-for-laglead.html}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Subset to a short series.
#' QOG = xtsubset(qog.ts.demo, year %in% 2008:2012 & cname == "United States")
#' # Lag by one time period.
#' QOG$L1.wdi_gdpc = xtshift(QOG, "wdi_gdpc", -1)
#' # Lead by two time periods.
#' QOG$F2.wdi_gdpc = xtshift(QOG, "wdi_gdpc", 2)
#' # Check results.
#' QOG[, c("year", "wdi_gdpc", "L1.wdi_gdpc", "F2.wdi_gdpc")]
#' @keywords xt ts
xtshift <- function(data, variable, k = 1) {
  stopifnot(xtdata(data))
  stopifnot(variable %in% names(data))
  ccode = xt(data)$data[1]
  
  v = tapply(data[, variable], data[, ccode], shift, k)
  v = unlist(v)
  return(v)
}

#' @export
xtlag <- function(data, variable, k = 1) {
  xtshift(data, variable, - abs(k))
}

#' @export
xtlead <- function(data, variable, k = 1) {
  xtshift(data, variable, abs(k))
}

#' Linear decay
#'
#' Linear decay function adapted from the \code{\link[doBy]{doBy}} package by 
#' Zachary M. Jones, and modified to understand the \code{\link{xtdata}} 
#' attribute.
#' 
#' @export
#' @param data a data frame with the \code{\link{xtdata}} attribute.
#' @param x the variable for which to compute time since event.
#' @param cutpoint the decay cut-point.
#' @author Zachary M. Jones
#' @source Zachary M. Jones, 
#' "Some Time-Series Functions for Panels with Missingness", 
#' \url{http://www.zmjones.com/panel-ts.html}
#' @seealso \code{\link[doBy]{doBy}}
#' @keywords xt events
xtdecay <- function(data, x, cutpoint) {
  data = tapply(data[, x], data[, xt(data)$data[1]], panel.tse, d = cutpoint)
  return(data)
}

#' Time since event
#'
#' Time since event function adapted from the \code{\link[doBy]{doBy}} package by 
#' Zachary M. Jones, and modified to understand the \code{\link{xtdata}} 
#' attribute.
#' 
#' @export
#' @param data a data frame with the \code{\link{xtdata}} attribute.
#' @param x the variable for which to compute time since event.
#' @author Zachary M. Jones
#' @source Zachary M. Jones, 
#' "Some Time-Series Functions for Panels with Missingness", 
#' \url{http://www.zmjones.com/panel-ts.html}
#' @seealso \code{\link[doBy]{doBy}}
#' @keywords xt events
xttse <- function(data, x) {
  data = tapply(data[, x], data[, xt(data)$data[1]], panel.tse)
  return(data)
}
