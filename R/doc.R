#' Functions to manipulate Quality of Government data and related material
#' 
#' This package contains a set of functions to download and 
#' manipulate Quality of Government (QOG) datasets and codebooks, including 
#' merging and plotting QOG datasets with other series of country-year 
#' observations.
#' 
#' Please refer to \url{http://www.qog.pol.gu.se} for details on QOG datasets.
#'
#'  \tabular{ll}{
#'    Package: \tab qogdata\cr
#'    Type: \tab Package\cr
#'    Version: \tab 0.1.3\cr
#'    Date: \tab 2013-08-09\cr
#'    License: \tab GPL-3\cr
#'    URL: \tab \url{https://github.com/briatte/qogdata}\cr
#'  }
#'  
#'  \itemize{
#'    \item \code{doc.R}: package and dataset documentation
#'    \item \code{get.R}: functions to download and format additional data
#'    \item \code{qog.R}: functions to download and manipulate QOG datasets
#'    \item \code{utils.R}: internal utilities coded mostly by others
#'    \item \code{xt.R}: functions to manipulate the \code{\link{xtdata}} attribute
#'    \item \code{xtfuncs.R}: functions for data carrying an \code{\link{xtdata}} attribute
#'    \item \code{xtplots.R}: plots for data carrying an \code{\link{xtdata}} attribute
#'  }
#'  
#'  This is a demo. There might be sufficient functionality by version 1.0 to 
#'  justify turning the \code{\link{xtdata}} attribute into a proper S3 or S4 
#'  class. For related packages, check e.g. the 
#'  \code{DataCombine} package for working with CSTS data.
#' 
#' @name qogdata-package
#' @docType package
#' @import foreign countrycode ggplot2 mapproj maps
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references
#' Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo
#' Rothstein. 2012. \emph{The QoG Social Policy Dataset},
#' version 4Apr12. 
#' University of Gothenburg: The Quality of Government Institute.
#' 
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
#' Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson. 
#' 2013. \emph{The Quality of Government Dataset}, version 
#' 15May13. 
#' University of Gothenburg: The Quality of Government Institute.
#' 
#' Teorell, Jan. Carl Dahlstrom & Stefan Dahlberg. 2011. 
#' \emph{The QoG Expert Survey Dataset}. 
#' University of Gothenburg: The Quality of Government Institute.
NULL

#' Quality of Government demo data
#' 
#' Selected variables from the Quality of Government Standard dataset:
#' 
#' \itemize{
#'   \item \code{year}: year of measurement (\code{ts} only)
#'   \item \code{ccode}: country code, numeric (ISO-3N)
#'   \item \code{ccodealp}: country code, alphabetical (ISO-3C)
#'   \item \code{cname}: country name
#'   \item \code{wdi_pop}: population (millions)
#'   \item \code{wdi_gdpc}: GDP per capita (contant dollars)
#'   \item \code{wdi_fr}: fertility rate (average births per woman)
#'   \item \code{chga_hinst}: regime type
#'   \item \code{bl_asy25mf}: average schooling years, both sexes aged 25+
#'   \item \code{bl_asy15f}: average schooling years, females aged 15+
#'   \item \code{bl_asy15m}: average schooling years, males aged 15+
#' }
#' 
#' @seealso \code{\link{qogfind}} to search the index of a QOG dataset
#' @docType data
#' @keywords datasets qog
#' @name qog.demo
#' @aliases qog.ts.demo qog.cs.demo
#' @usage data(qog.ts.demo)
#' data(qog.cs.demo)
#' @format two data frames, cross-sectional (\code{cs}) and time series (\code{ts})
#' @references
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
#' Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson. 
#' 2013. \emph{The Quality of Government Dataset}, version 
#' 15May13. 
#' University of Gothenburg: The Quality of Government Institute.
NULL

#' Quality of Government variable indexes
#' 
#' This dataset lists some information on the variables included in the 
#' Quality of Government Basic, Standard and Social Policy datasets:
#' 
#'   \itemize{
#'     \item \code{variable}
#'     \item \code{label}
#'     \item \code{ts.min}: first year of measurement (time series dataset)
#'     \item \code{ts.max}: last year of measurement (time series dataset)
#'     \item \code{ts.N}: number of observations  (time series dataset)
#'     \item \code{ts.T}: year coverage (time series dataset)
#'     \item \code{cs.N}: number of observations (cross-sectional dataset)
#'     \item \code{cs.min}: first year of measurement (cross-sectional dataset)
#'     \item \code{cs.max}: last year of measurement (cross-sectional dataset)
#'   }
#'   
#' @docType data
#' @keywords datasets qog
#' @name qog.index
#' @aliases qog.std.index qog.bas.index qog.soc.index
#' @format a data frame
#' @seealso \code{\link{qogfind}} to search the index of a QOG dataset
#' @references
#' Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo
#' Rothstein. 2012. \emph{The QoG Social Policy Dataset},
#' version 4Apr12. 
#' University of Gothenburg: The Quality of Government Institute.
#' 
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
#' Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson. 
#' 2013. \emph{The Quality of Government Dataset}, version 
#' 15May13. 
#' University of Gothenburg: The Quality of Government Institute.
#' 
#' Teorell, Jan. Carl Dahlstrom & Stefan Dahlberg. 2011. 
#' \emph{The QoG Expert Survey Dataset}. 
#' University of Gothenburg: The Quality of Government Institute.
NULL