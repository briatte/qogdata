\name{xtmissing}
\alias{xtmissing}
\title{Plot availablity of panel data}
\usage{
  xtmissing(data = NULL, variable = NULL, id = "ccodealp",
    time = "year")
}
\arguments{
  \item{data}{a TSCS dataset.}

  \item{variable}{the variable to plot.}

  \item{id}{the variable to label the groups. Defaults to
  \code{ccodealp}, the ISO-3C country code provided in QOG
  datasets.}

  \item{time}{the variable that codes for the time period.
  Defaults to \code{year}, the time variable provided in
  QOG datasets.}
}
\description{
  Function to plot data availability in time series
  cross-sectional (TSCS) data. Requires the \code{ggplot2}
  package.
}
\examples{
# Load QOG Basic time series data.
QOG = qogdata(tempfile(fileext = ".dta"), version = "bas", format = "ts",
              variables = c("ccodealp", "year", "wdi_gdpc", "ihme_nm"))
xtmissing(QOG, "wdi_gdpc") +
  ggtitle("Country-year availability of gross domestic product per capita")
xtmissing(QOG, "ihme_nm") +
  ggtitle("Country-year availability of neonatal mortality rate")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}

