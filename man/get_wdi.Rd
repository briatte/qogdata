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
  \code{"auto"}, which sets the year to the minimum year of
  the original dataset.}

  \item{end}{last year of data to download. Defaults to
  \code{"auto"}, which sets the year to the maximum year of
  the original dataset.}

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
  data frame. The result carries an \code{xdata} attribute
  that can be passed to the \code{\link{xtmerge}} panel
  data method.
}
\examples{
# Download QOG Standard time series data for WDI healthcare expenditure.
QOG = qogdata(tempfile(fileext = ".dta"), format = "ts",
              years = 1995:2010, variables = c("ccodealp", "wdi_hec"))
# Merge QOG data to corresponding WDI series; add income classification.
WDI = get_wdi(x = "SH.XPD.PCAP.PP.KD", add = "income")
# Plot results: dots are QOG measurements, lines are WDI measurements.
if(require(ggplot2))
  qplot(data = subset(QOG, !is.na(wdi_hec)),
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
  \url{http://CRAN.R-project.org/package=WDI} World Bank.
  2013. \emph{World Development Indicators}. Washington DC:
  The World Bank Group. Online publication, January 24,
  2013.
}
\seealso{
  \code{\link[WDI]{WDI}}, \code{\link{xtdata}}
}

