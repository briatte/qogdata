\name{qogfind}
\alias{qogfind}
\title{qogfind -- find Quality of Government variables}
\usage{
  qogfind(..., version = "std", shorter.labels = TRUE,
    show = "all")
}
\arguments{
  \item{...}{keywords or \code{regex} phrases passed to
  \code{grepl}.}

  \item{version}{the QOG version to search: either
  \code{std} (the default) or \code{soc}.}

  \item{shorter.labels}{whether to abbreviate the labels to
  32 characters. Defaults to \code{TRUE} for better console
  output.}
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
head(qogfind("^socx", version = "soc", show = "cs", shorter.labels = FALSE))
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
  Government Institute, \link{http://www.qog.pol.gu.se}.
  Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Sören
  Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson.
  2013. \emph{The Quality of Government Dataset}, version
  15May13. University of Gothenburg: The Quality of
  Government Institute, \link{http://www.qog.pol.gu.se}.
}
\seealso{
  \code{\link{grep}}, \code{\link{qogdata}}
}

