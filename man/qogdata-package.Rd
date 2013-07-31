\name{qogdata-package}
\alias{qogdata-package}
\docType{package}
\title{
Helper functions for Quality of Government data
}
\description{
Helper functions to download Quality of Government datasets
    and codebooks. Additional methods are provided to merge data from
    other sources. Please refer to \url{http://www.qog.pol.gu.se}
    for details on QOG research projects.
}
\details{
\tabular{ll}{
Package: \tab qogdata\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-07-30\cr
License: \tab GPL-3\cr
URL: \tab \url{https://github.com/briatte/qogdata}\cr
}
}
\author{
Francois Briatte \email{f.briatte@ed.ac.uk}

Maintainer: Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo
  Rothstein. 2012. \emph{The QoG Social Policy Dataset},
  version 4Apr12. 
  University of Gothenburg: The Quality of
  Government Institute.

  Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
  Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson.
  2013. \emph{The Quality of Government Dataset}, version
  15May13. 
  University of Gothenburg: The Quality of
  Government Institute.

  Teorell, Jan. Carl Dahlstrom & Stefan Dahlberg. 2011. 
  \emph{The QoG Expert Survey Dataset}. 
  University of Gothenburg: The Quality of 
  Government Institute.
}
\examples{
# get datasets
qogdata()
qogdata(file = TRUE)
# get codebooks
qogbook()
qogbook(file = TRUE)
# find variables
qogfind("democracy", version = "std")
qogfind("income tax", version = "soc")
# plot variables
QOG = qogdata(tempfile(), warn.missing.labels = FALSE, convert.factors = TRUE,
              version = "bas", variables = c("ccodealp", "undp_hdi", "ihme_nm"))
qogmap(subset(QOG, ccodealp != "RUS"), "ihme_nm", continent = "Asia") +
  ggtitle("Neonatal Mortality Rate per 1,000 births (IHME, 2009)")
qogmap(QOG, "undp_hdi", quantize = 3, continents = c("Africa", "Asia")) +
  scale_fill_brewer("", palette = "RdYlBu", labels = c("Low", "Med", "High")) +
  ggtitle("Human Development Index (UNDP, 2009-2010)")
}
