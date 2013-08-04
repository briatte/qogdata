\name{get_uds}
\alias{get_uds}
\title{Get UDS data in \code{xtdata} format}
\usage{
  get_uds()
}
\arguments{
  \item{data}{a QOG Standard time series dataset, or any
  data frame with \code{cname} (country) and \code{year}
  information coded as in the QOG Standard time series
  dataset.}

  \item{id}{the country variable to join data over.
  Defaults to \code{ccodecow}, the Correlate of War numeric
  country code used in the Unified Democracy Scores
  dataset.}

  \item{year}{the year variable to join data over. Defaults
  to \code{"year"}.}

  \item{out}{what to return. If set to \code{"data"}, the
  joined QOG/UDS dataset is returned. Defaults to
  \code{"uds"}, which only returns the UDS dataset.}

  \item{...}{other parameters passed to \code{merge}. Set
  \code{out} to \code{"data"} for the function to return
  the joined QOG/UDS dataset.}
}
\description{
  Function to download the Unified Democracy Scores (UDS)
  by Pemstein, Meserve and Melton (2010). The result
  carries an \code{xdata} attribute that can be passed to
  the \code{\link{xtmerge}} panel data method.
}
\examples{
# By default, the function downloads the UDS dataset.
head(UDS <- merge_uds())
# Basic visualization of average scores in the 2000s.
if(require(countrycode)) {
  UDS$ccode = countrycode(UDS$ccodecow, "cown", "iso3n")
  qogmap(aggregate(uds_mean ~ ccode, mean, data = subset(UDS, year > 2000)),
         "uds_mean", quantize = 5, continent = c("Africa", "Asia")) +
    scale_fill_brewer("Mean UDS since 2000", palette = "RdYlGn")
}
## Inner merge to QOG (not run).
# QOG <- qogdata(file = TRUE, format = "ts")
# UDS <- get_uds()
# data <- xtmerge(QOG, UDS)
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

