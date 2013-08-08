\docType{package}
\name{qogdata-package}
\alias{qogdata-package}
\title{Functions to manipulate Quality of Government data and related material}
\description{
  This package contains a set of functions to download and
  manipulate Quality of Government (QOG) datasets and
  codebooks, including merging and plotting QOG datasets
  with other series of country-year observations.
}
\details{
  Please refer to \url{http://www.qog.pol.gu.se} for
  details on QOG datasets.

  \tabular{ll}{ Package: \tab qogdata\cr Type: \tab
  Package\cr Version: \tab 0.1.1\cr Date: \tab
  2013-08-08\cr License: \tab GPL-3\cr URL: \tab
  \url{https://github.com/briatte/qogdata}\cr }

  \itemize{ \item \code{doc.R}: package and dataset
  documentation \item \code{get.R}: functions to download
  and format additional data \item \code{qog.R}: functions
  to download and manipulate QOG datasets \item
  \code{utils.R}: internal utilities coded mostly by others
  \item \code{xt.R}: functions to manipulate the
  \code{\link{xdata}} attribute \item \code{xtfuncs.R}:
  functions for data carrying an \code{\link{xdata}}
  attribute \item \code{xtplots.R}: plots for data carrying
  an \code{\link{xdata}} attribute }

  This is a demo. There might be sufficient functionality
  by version 1.0 to justify turning the \code{\link{xdata}}
  attribute into a proper S3 or S4 class. For related
  packages, check e.g. the
  \code{\link[DataCombine]{DataCombine}} package for
  working with CSTS data.
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo
  Rothstein. 2012. \emph{The QoG Social Policy Dataset},
  version 4Apr12. University of Gothenburg: The Quality of
  Government Institute.

  Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
  Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson.
  2013. \emph{The Quality of Government Dataset}, version
  15May13. University of Gothenburg: The Quality of
  Government Institute.

  Teorell, Jan. Carl Dahlstrom & Stefan Dahlberg. 2011.
  \emph{The QoG Expert Survey Dataset}. University of
  Gothenburg: The Quality of Government Institute.
}

