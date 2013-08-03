\name{panel}
\alias{panel}
\title{Plot availablity of panel data}
\usage{
  panel(data = NULL, variable = NULL, id = "ccodealp",
    time = "year")
}
\arguments{
  \item{data}{a TSCS dataset.}

  \item{variable}{the variable to plot.}

  \item{id}{the variable to label the groups.}

  \item{time}{the variable that codes for the time period.}
}
\description{
  Function to plot data availability in time series
  cross-sectional (TSCS) data. Requires the \code{ggplot2}
  package.
}
\details{
  The auto-detection of time limits is buggy.
}
\examples{
# Load QOG Standard time series data.
QOG = qogdata(tempfile(fileext = ".dta"), version = "bas", format = "ts",
              variables = c("ccodealp", "year", "wdi_gdpc", "ihme_nm"))
# Availablity of gross domestic product per capita.
panel(QOG, "wdi_gdpc")
# Availablity of neonatal mortality rate.
panel(QOG, "ihme_nm")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}

