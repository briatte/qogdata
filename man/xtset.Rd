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
  \code{spec} parameters as \code{countrycode} formats.}

  \item{name}{a description of the dataset.}

  \item{quiet}{whether to return some details. Defaults to
  \code{FALSE} (verbose).}
}
\description{
  Function to specify the panel data properties of a data
  frame. The syntax is provided in the 'Details'. Intended
  for country-year cross-sectional time series (CSTS) data,
  but should fit any panel data format.
}
\details{
  an \code{xtdata} attribute is a list of 5 to 11 panel
  data paramaters stored in the following named vectors:
  \itemize{ \item \bold{data}, a vector of variable names
  coding for the unique identifier and time period,
  optionally followed by the variable names for the short
  and long observation names, as with alphabetical country
  codes and country names.  \item \bold{spec}, a vector of
  variable formats that match the \code{data} variables;
  this allows to store \code{countrycode} formats in the
  dataset and can theoretically work with any other format,
  like FIPS or households.  \item \bold{type}, a
  single-element vector that defines the type of
  observations contained in the data; \code{"country"} is
  the only type that currently produces any specific
  behaviour from \code{xtdata}.  \item \bold{name}, an
  optional single-element vector that defines the dataset
  name, which is printed on top of the function results.
  \item \bold{url}, an optional URI locator for the data
  source (typically, where to find codebooks and
  publications). } These characteristics mimic the
  behaviour of the \code{xtset} and \code{label data}
  commands in Stata.
}
\examples{
## Identify the QOG Basic time series data (not run).
# QOG = qogdata(file = TRUE, version = "bas", format = "ts")
# QOG = xtset(QOG)
## Identify the subset of most recent years (not run).
# QOG.200x = xtset(subset(QOG, year > 1999))
# Identify the UDS dataset.
UDS = merge_uds()
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

