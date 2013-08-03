\name{merge_state}
\alias{merge_state}
\title{Add and merge state-level historical events to QOG Standard time series data}
\usage{
  merge_state(data = NULL, id = "ccode", year = "year",
    independence = "gw_indep", coups = "pt_coup",
    out = "state", ...)
}
\arguments{
  \item{data}{a QOG Standard time series dataset, or any
  data frame with \code{cname} (country) and \code{year}
  information coded as in the QOG Standard time series
  dataset.}

  \item{id}{the country variable to join data over.
  Defaults to \code{ccode}, the ISO3-N numeric country code
  used in the Quality of Government dataset. See
  'Details'.}

  \item{year}{the year variable to join data over. Defaults
  to \code{"year"}.}

  \item{independence}{name under which to create the state
  independence variable. Defaults to \code{"gw_indep"}. See
  'Details'.}

  \item{coups}{name under which to create the state coups
  variable. Defaults to \code{"pt_coup"}. See 'Details'.}

  \item{out}{what to return. If set to \code{"data"}, the
  joined QOG/Gleditsch and Ward/Powell and Thyne dataset is
  returned. Defaults to \code{"state"}, which only returns
  the Gleditsch and Ward/Powell and Thyne dataset/}

  \item{...}{other parameters passed to \code{merge}. Set
  \code{out} to \code{"data"} for the function to return
  the joined QOG/Gleditsch and Ward/Powell and Thyne
  dataset.}
}
\description{
  Function to download state-level historical events from
  Gleditsch and Ward (1999, updated 3 May 2013) and Powell
  and Thyne (2011, updated c. 2013). The result might be
  simultaneously merged to QOG Standard time series data.
}
\details{
  If \code{out} is set to anything but \code{"data"}, the
  returned country codes are named \code{ccodegw} and
  respect the original country codes of the Gleditsch and
  Ward dataset, which are also used in the Powell and Thyne
  dataset. The variables produced by this function are
  \bold{gw_indep} (years of independence, coded 0/1), from
  Gleditsch and Ward, and \bold{pt_coup} (attempted and
  successful \emph{coups d'\'{E}tat}), from Powell and
  Thyne.
}
\examples{
# By default, the function downloads the gw_indep and pt_coups variables.
head(GIC <- merge_state())
# Show country-years.
if(require(plyr)) {
  ddply(GIC, .(country.name), summarize, n = length(year), min = min(year), max = max(year))
}
# Plot the full data.
if(require(countrycode) & require(ggplot2)) {
  GIC$iso3c = countrycode(GIC$country.name, "country.name", "iso3c")
  GIC$continent = countrycode(GIC$country.name, "country.name", "continent")
  qplot(data = subset(GIC, !is.na(continent)),
            x = year, y = reorder(iso3c, as.numeric(pt_coup), mean),
            fill = continent, alpha = pt_coup, geom = "tile") +
    scale_fill_brewer("Continent", palette = "Set1") +
    scale_alpha_manual("Event", values = c(0, .5, 1)) +
    scale_x_continuous(breaks = seq(1953, 2013, 10)) +
    labs(y = NULL)
}
if(require(ggplot2)) {
qplot(data = subset(GIC, pt_coup != "No verified coup attempt"),
      x = year, fill = pt_coup, binwidth = 3, alpha = I(2/3),
      position = "stack", stat = "bin", geom = "bar") +
  theme(legend.position = "bottom") +
  scale_fill_brewer("", palette = "Set1") +
  labs(x = NULL)
}
## Left outer merge to QOG (not run).
# QOG <- qogdata(file = TRUE, format = "ts")
# QOG <- merge_state(QOG, all.x = TRUE, out = "data")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Gleditsch, Kristian S. & Michael D. Ward. 1999.
  "Interstate System Membership: A Revised List of the
  Independent States since 1816.". \emph{International
  Interactions} 25:393-413,
  \url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.

  Powell, Jonathan M. & Clayton L. Thyne. 2011. "Global
  Instances of Coups from 1950 to 2010: A New Dataset.".
  \emph{Journal of Peace Research} 48(2): 249-259,
  \url{http://www.uky.edu/~clthyn2/coup_data/home.htm}.
}

