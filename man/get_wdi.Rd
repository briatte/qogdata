\name{get_wdi}
\alias{get_wdi}
\title{Get World Development Indicators in \code{xtdata} format}
\usage{
  get_wdi(x, country = "all", start = 1945, end = 2013,
    add = NULL)
}
\arguments{
  \item{x}{the name(s) of the indicator(s) to download from
  the World Development Indicators API. See the
  documentation of the \code{\link[WDI]{WDI}} function.}

  \item{country}{the ISO-2C country codes for which to
  download the indicators. Defaults to \code{"all"}, which
  downloads all available data.}

  \item{start}{first year of data to download. Defaults to
  \code{1945}.}

  \item{end}{last year of data to download. Defaults to
  \code{2012}.}

  \item{add}{a vector of variable names from the WDI query
  that should be returned with the indicators. See the
  documentation of the \code{\link[WDI]{WDI}} function.}
}
\description{
  Function to download World Development Indicators (WDI)
  through the World Bank API, using the
  \code{\link[WDI]{WDI}} package. If the
  \code{\link[countrycode]{countrycode}} package is
  available, it is used to add ISO-3N country codes to the
  data frame. #' The result carries an \code{\link{xtdata}}
  attribute that can be passed to the \code{\link{xtmerge}}
  panel data method.
}
\examples{
# Download WDI series with income classification.
W = get_wdi(x = "SH.XPD.PCAP.PP.KD", add = "income")
# Merge to QOG time series demo data that contains identical indicator.
Q = xtmerge(qog.ts.demo, W)
# Compare measurements: dots are QOG data points, lines are WDI data points.
if(require(ggplot2))
  qplot(data = subset(Q, !is.na(wdi_hec)),
        x = year, y = wdi_hec, color = income, alpha = I(.5)) +
  geom_smooth(aes(y = SH.XPD.PCAP.PP.KD, group = ccode), se = FALSE) +
  scale_colour_brewer("", palette = "RdYlBu") +
  labs(y = NULL, x = NULL) +
  theme_minimal(16)
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Arel-Bundock, Vincent. 2012. \emph{WDI: World Development
  Indicators (World Bank).} R package version 2.2.
  \url{http://CRAN.R-project.org/package=WDI}

  World Bank. 2013. \emph{World Development Indicators}.
  Washington DC: The World Bank Group. Online publication,
  January 24, 2013.
}
\seealso{
  \code{\link[WDI]{WDI}}, \code{\link{xtdata}}
}

