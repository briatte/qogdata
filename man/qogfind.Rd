\name{qogfind}
\alias{qogfind}
\title{Find Quality of Government variables}
\usage{
  qogfind(..., version = "std", compact = TRUE,
    show = "all")
}
\arguments{
  \item{...}{keywords or \code{regex} phrases passed to
  \code{grepl}.}

  \item{version}{the QOG version to search: either
  \code{std} (the default) or \code{soc}.}

  \item{compact}{whether to limit the labels returned to 32
  characters. Defaults to \code{TRUE} for better console
  output.}

  \item{show}{which variables to show for years of
  measurement: \code{cs} (cross-sectional), \code{ts} (time
  series), or \code{all} (the default).}
}
\value{
  a data frame containg matching variables, described by
  their names, labels and years of measurement in the time
  series (\code{ts}) cross-sectional (\code{cs}) datasets.
  The information should match the ranges indicated in the
  \emph{QOG Standard Codebook} and \emph{QOG Social Policy
  Codebook}.
}
\description{
  Function to perform a \code{regex} search on QOG variable
  names and labels. A few labels are missing for strictly
  cross-sectional variables.
}
\examples{
# QOG Standard search.
qogfind("regime", "institutions")
# QOG Standard search, with regex syntax.
qogfind("public|administration")
# QOG Social Policy search, showing cross-sectional information only.
head(qogfind("^socx", version = "soc", show = "cs", compact = FALSE))
# QOG Standard variables featured only in the cross-sectional version.
qogfind("*")[is.na(qogfind("*")$ts.N), ]
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo
  Rothstein. 2012. \emph{The QoG Social Policy Dataset},
  version 4Apr12. University of Gothenburg: The Quality of
  Government Institute, \url{http://www.qog.pol.gu.se}.

  Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
  Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson.
  2013. \emph{The Quality of Government Dataset}, version
  15May13. University of Gothenburg: The Quality of
  Government Institute, \url{http://www.qog.pol.gu.se}.
}
\seealso{
  \code{\link{grep}}, \code{\link{qogdata}}
}
\keyword{qog}

