\name{merge_wdi}
\alias{merge_wdi}
\title{Join historical and recent states in QOG Standard time series data}
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
  documentation of the \code{\link[WDI]{WDI}} function. #'}

  \item{out}{what to return. If set to \code{"data"}, the
  entire dataset is returned, merged to the WDI data.
  Defaults to \code{"wdi"}, which only returns the result
  of the WDI query.}
}
\description{
  Function to perform a World Development Indicators (WDI)
  query through the World Bank API, using the
  \code{\link[WDI]{WDI}} package. The result might be
  simultaneously merged to QOG Standard time series data.
}
\examples{
# Download QOG Standard time series data for WDI healthcare expenditure.
QOG = qogdata(tempfile(fileext = ".dta"), format = "ts",
              years = 1950:2010, variables = c("ccodealp", "wdi_hec"))
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
\seealso{
  \code{\link[WDI]{WDI}}
}

