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
#' @keywords xt
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
#' @keywords xt
xttse <- function(data, x) {
  data = tapply(data[, x], data[, xt(data)$data[1]], panel.tse)
  return(data)
}
