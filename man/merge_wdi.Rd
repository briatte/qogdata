\name{merge_wdi}
\alias{merge_wdi}
\title{Add and merge WDI data to QOG Standard time series data}
\usage{
  merge_wdi(data, id = "ccode", year = "year", x,
    country = "all", start = "auto", end = "auto",
    add = NULL, out = "wdi", ...)
}
\arguments{
  \item{data}{a QOG Standard time series dataset, or any
  data frame with \code{cname} (country) and \code{year}
  information coded as in the QOG Standard time series
  dataset.}

  \item{id}{the country variable to join data over.
  Defaults to \code{ccode}, the ISO3-N country code
  variable most recommended for merging purposes.}

  \item{year}{the year variable to join data over. Defaults
  to \code{"year"}.}

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

  \item{out}{what to return. If set to \code{"data"}, the
  joined QOG/WDI dataset is returned. Defaults to
  \code{"wdi"}, which only returns the result of the WDI
  query.}

  \item{...}{other parameters passed to \code{merge}. Set
  \code{out} to \code{"data"} for the function to return
  the joined QOG/WDI dataset.}
}
\description{
  Function to perform a World Development Indicators (WDI)
  query through the World Bank API, using the
  \code{\link[WDI]{WDI}} and
  \code{\link[countrycode]{countrycode}} packages. The
  result might be simultaneously merged to QOG Standard
  time series data.
}
\examples{
# Download QOG Standard time series data for WDI healthcare expenditure.
QOG = qogdata(tempfile(fileext = ".dta"), format = "ts",
              years = 1995:2010, variables = c("ccodealp", "wdi_hec"))
# Merge QOG data to corresponding WDI series; add income classification.
QOG = merge_wdi(QOG, x = "SH.XPD.PCAP.PP.KD", add = "income", out = "data")
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
  \code{\link[WDI]{WDI}}
}

