\name{xtshift}
\alias{xtlag}
\alias{xtlead}
\alias{xtshift}
\title{Lag or lead a panel data variable}
\usage{
  xtshift(data, variable, k = 1)
}
\arguments{
  \item{data}{a data frame carrying an \code{\link{xtdata}}
  attribute.}

  \item{variable}{the variable to lag/lead.}

  \item{k}{the number of lags/leads.}
}
\description{
  Function to lag/lead a panel variable in a data frame
  with an \code{\link{xtdata}} attribute. Based on the
  \code{shift} function by TszKin Julian Chan.
}
\details{
  You might want to use the \code{xtlag} and \code{xtlead}
  convenience wrappers: \itemize{ \item \code{xtlag} will
  perform a negative shift of \code{k} lags \item
  \code{xtlag} will perform a positive shift of \code{k}
  leads }
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Subset to a short series.
QOG = xtsubset(qog.ts.demo, year \%in\% 2008:2012 & cname == "United States")
# Lag by one time period.
QOG$L1.wdi_gdpc = xtshift(QOG, "wdi_gdpc", -1)
# Lead by two time periods.
QOG$F2.wdi_gdpc = xtshift(QOG, "wdi_gdpc", 2)
# Check results.
QOG[, c("year", "wdi_gdpc", "L1.wdi_gdpc", "F2.wdi_gdpc")]
}
\references{
  Christopher Gandrud, "Slide: one function for lag/lead
  variables in data frames, including time-series
  cross-sectional data",
  \url{http://christophergandrud.blogspot.com/2013/05/slide-one-function-for-laglead.html}
}
\seealso{
  \code{\link{xtdata}}
}
\keyword{ts}
\keyword{xt}

