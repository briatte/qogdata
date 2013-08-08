\name{get_uds}
\alias{get_uds}
\title{Get UDS data in \code{xtdata} format}
\usage{
  get_uds()
}
\value{
  a data frame with country-year observations
}
\description{
  Function to download the Unified Democracy Scores (UDS)
  by Pemstein, Meserve and Melton (2010). The result
  carries an \code{\link{xtdata}} attribute that can be
  passed to the \code{\link{xtmerge}} panel data method.
}
\examples{
# By default, the function downloads the UDS dataset.
head(UDS <- get_uds())
# Basic visualization of average scores in the 2000s.
if(require(countrycode) & require(ggplot2)) {
  UDS$ccode = countrycode(UDS$ccodecow, "cown", "iso3n")
  xtmap(aggregate(uds_mean ~ ccode, mean, data = subset(UDS, year > 2000)),
        "uds_mean", quantize = 5, continent = c("Africa", "Asia"),
        iso3n = "ccode") +
    scale_fill_brewer("Mean UDS since 2000", palette = "RdYlGn")
}
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Pemstein, Daniel, Stephen A. Meserve & James Melton.
  2010. "Democratic Compromise: A Latent Variable Analysis
  of Ten Measures of Regime Type." \emph{Political
  Analysis} 18(4): 426-449.
  \url{http://www.unified-democracy-scores.org/}
}

