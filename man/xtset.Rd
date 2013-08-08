\name{xtset}
\alias{xtset}
\title{Set panel data properties}
\usage{
  xtset(dataset = NULL,
    data = c("ccode", "year", "ccodealp", "cname"),
    spec = c("iso3n", "year", "iso3c", "country.name"),
    type = "country",
    name = "Quality of Government, time series data",
    url = "http://www.qog.pol.gu.se/", quiet = FALSE)
}
\arguments{
  \item{dataset}{the panel data frame to set the attributes
  to.}

  \item{data}{the \code{data} parameters to pass to the
  data: unique identifier and time period, optionally
  followed by short and long observation names. Defaults to
  QOG country-year data settings. See 'Details'.}

  \item{spec}{the \code{spec} parameters to pass to the
  data: formats of unique identifier and time period,
  optionally followed by formats of short and long
  observation names. Defaults to QOG country-year data
  settings. See 'Details'.}

  \item{type}{the type of observations in the panel data.
  Defaults to \code{"country"}, which will read the
  \code{spec} parameters as
  \code{\link[countrycode]{countrycode}} formats.}

  \item{name}{a description of the dataset.}

  \item{url}{a URI locator for the dataset (the website
  address).}

  \item{quiet}{whether to return some details. Defaults to
  \code{FALSE} (verbose).}
}
\value{
  a data frame with the \code{xtdata} attribute.
}
\description{
  Function to specify the panel data properties of a data
  frame. The syntax is provided in the 'Details'. Intended
  for country-year cross-sectional time series (CSTS) data,
  but should fit any panel data format.
}
\details{
  an \code{xtdata} attribute is a list of 5 to 11 panel
  data paramaters stored as character strings in the
  following named vectors: \itemize{ \item \bold{data}, a
  vector of variable names coding for the unique identifier
  and time period, optionally followed by the variable
  names for the short and long observation names, as with
  alphabetical country codes and country names.  \item
  \bold{spec}, a vector of variable formats that match the
  \code{data} variables; this allows to store
  \code{\link[countrycode]{countrycode}} formats in the
  dataset and can theoretically work with any other format,
  like FIPS or households.  \item \bold{type}, a vector
  that defines the type of observations contained in the
  data; \code{"country"} is the only type that currently
  produces any specific behaviour from \code{xtdata}
  through the \code{\link[countrycode]{countrycode}}
  package, but \code{"region"} is on the way for working
  with NUTS codes, and it should be feasable to implement
  FIPS codes.  \item \bold{name}, an optional vector that
  defines the dataset name, which is printed on top of the
  function results.  \item \bold{url}, an optional vector
  that defines the URI locator for the data source
  (typically, the website address where to find codebooks,
  technical documentation and related publications). }
  These characteristics mimic some of the behaviour of the
  \code{xtset} and \code{label data} commands in Stata.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Set xtdata attribute on QOG time series.
QOG = xtset(qog.ts.demo)
# Set xtdata attribute on recent years.
QOG.200x = xtset(subset(qog.ts.demo, year > 1999))
# Manually set xtdata attribute for UDS dataset.
UDS = get_uds()
UDS = xtset(UDS,
            data = c("ccodecow", "year"),
            spec = c("cown", "year"),
            type = "country",
            name = "Unified Democracy Scores"
    )
}
\seealso{
  \code{\link[countrycode]{countrycode}}
}

