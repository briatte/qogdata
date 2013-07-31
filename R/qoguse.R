#' qogdata - Import Quality of Government data into R
#'
#' Alias for the \code{qogdata} function, for maximum resemblance to Christoph Thewes' \code{QOG} package for Stata.
#'
#' @export
#' @param ... arguments supplied to the \code{qogdata} function.
#' @seealso \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}

qoguse <- function( ...) {
  qogdata(...)
}