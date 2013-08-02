#' Add and merge UDS data to QOG Standard time series data
#'
#' Function to download the Unified Democracy Scores (UDS) by Pemstein, Meserve and Melton (2010). The result might be simultaneously merged to QOG Standard time series data. Please visit \url{http://www.unified-democracy-scores.org/} for details on the UDS research project.
#'
#' @export
#' @param data a QOG Standard time series dataset, or any data frame with \code{cname} (country) and \code{year} information coded as in the QOG Standard time series dataset.
#' @param id the country variable to join data over. Defaults to \code{ccodecow}, the Correlate of War numeric country code used in the Unified Democracy Scores dataset.
#' @param year the year variable to join data over. Defaults to \code{"year"}.
#' @param out what to return. If set to \code{"data"}, the joined QOG/UDS dataset is returned. Defaults to \code{"uds"}, which only returns the UDS dataset.
#' @param ... other parameters passed to \code{merge}. Set \code{out} to \code{"data"} for the function to return the joined QOG/UDS dataset.
#' @value a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Pemstein, Daniel, Stephen A. Meserve & James Melton. 2010. 
#' "Democratic Compromise: A Latent Variable Analysis of Ten Measures of 
#' Regime Type." \emph{Political Analysis} 18(4): 426-449. 
#' @examples
#' # By default, the function downloads the UDS dataset.
#' head(UDS <- merge_uds())
#' # Basic visualization of average scores in the 2000s.
#' if(require(countrycode)) {
#'   UDS$ccode = countrycode(UDS$ccodecow, "cown", "iso3n")
#'   qogmap(aggregate(uds_mean ~ ccode, mean, data = subset(UDS, year > 2000)), 
#'          "uds_mean", quantize = 5, continent = c("Africa", "Asia")) + 
#'     scale_fill_brewer("Mean UDS since 2000", palette = "RdYlGn")
#' }

merge_uds <- function(data = NULL, id = "ccodecow", year = "year", 
                      out = "uds", ...) {
  url = "http://www.unified-democracy-scores.org/files/uds_summary.csv.gz"
  y = tempfile(fileext = ".csv.gz")
  download.file(url, y, quiet = TRUE)
  # exclude country names
  y = read.csv(gzfile(y), sep = ",")[, -1]
  # rename variables
  names(y) = c("year", id, paste0("uds_", names(y)[-2:-1]))
  # merge
  if(out == "data")
    y = merge(data, y, by = c(id, year), ...)
  return(y)
}
