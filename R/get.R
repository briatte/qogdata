
get_qog <- function(...) {
  qogdata(...)
}

#' Get World Development Indicators in \code{xtdata} format
#'
#' Function to download World Development Indicators (WDI) through the 
#' World Bank API, using the \code{\link[WDI]{WDI}} package. The 
#' \code{\link[countrycode]{countrycode}} package is also used to
#' add ISO-3N country codes to the data frame. 
#' The result carries an \code{\link{xtdata}} attribute that can be passed to the 
#' \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @param x the name(s) of the indicator(s) to download from the World 
#' Development Indicators API. 
#' See the documentation of the \code{\link[WDI]{WDI}} function.
#' @param country the ISO-2C country codes for which to download the indicators. 
#' Defaults to \code{"all"}, which downloads all available data.
#' @param start first year of data to download. Defaults to \code{1945}.
#' @param end last year of data to download. Defaults to \code{2012}.
#' @param extra a vector of additional variables from the WDI query that 
#' should be returned with the indicators. 
#' See the documentation of the \code{\link[WDI]{WDI}} function, or set to 
#' \code{TRUE} to save all additional variables.
#' @param aggregates whether to keep World Bank aggregates in the results, which 
#' causes the \code{xtdata} attribute to carry two data types, \code{"country"} 
#' and \code{"aggregate"}. Defaults to \code{FALSE}.
#' @param ccode the variable from which to create ISO-3N country codes. Defaults 
#' to \code{iso3c}, but might be set to any of the country variables returned by 
#' \code{\link[WDI]{WDI}}. Intended for testing purposes.
#' @return a data frame with country-year observations 
#' and an \code{\link{xtdata}} attribute
#' @seealso \code{\link[WDI]{WDI}}, \code{\link{xtdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Arel-Bundock, Vincent. 2012. \emph{WDI: World Development 
#' Indicators (World Bank).} R package version 2.2. 
#' \url{http://CRAN.R-project.org/package=WDI}
#' 
#' World Bank. 2013. \emph{World Development Indicators}. 
#' Washington DC: The World Bank Group. Online publication, January 24, 2013.
#' @examples
#' if(require(WDI) & require(ggplot2)) {
#'   # Download WDI series with income classification.
#'   WDI = get_wdi(x = "SH.XPD.PCAP.PP.KD", extra = "income")
#'   # Merge to QOG time series demo data that contains identical indicator.
#'   QOG = xtmerge(qog.ts.demo, WDI)
#'   # Compare measurements: dots are QOG data points, lines are WDI data points.
#'   qplot(data = subset(QOG, !is.na(wdi_hec)), 
#'         x = year, y = wdi_hec, color = income, alpha = I(.5)) + 
#'   geom_smooth(aes(y = SH.XPD.PCAP.PP.KD, group = ccode), se = FALSE) +
#'   scale_colour_brewer("", palette = "RdYlBu") +
#'   labs(y = NULL, x = NULL) +
#'   theme_minimal(16)
#' }

get_wdi <- function(x, country = "all", start = 1945, end = 2013, extra = NULL,
                    aggregates = FALSE, ccode = "iso3c") {
  try_require(c("countrycode", "WDI"))
  W <- WDI(country = country, indicator = x, start = start, end = end, extra = TRUE, cache = NULL)
  W$income = factor(W$income, levels = c("High income: OECD", "High income: nonOECD", "Upper middle income", "Lower middle income", "Low income"), ordered = TRUE)
  if(isTRUE(extra)) {
    extra = names(W)[!grepl(paste0("iso3|year|", x), names(W))]
  }
  f = ifelse(grepl("iso", ccode), ccode, "country.name")
  W[, "iso3n"] = countrycode(W[, ccode], f, "iso3n")
  W = W[, c("iso3n", "year", x, extra)]
  type = "country"
  if(!aggregates)
    W = W[!is.na(W[, "iso3n"]), ]
  else
    type = c("country", "aggregate")
  #
  # xtset
  #
  data = c("iso3n", "year")
  if("country" %in% names(W)) data = c(data, "country")
  spec = c("iso3n", "year")
  if("country" %in% names(W)) spec = c(data, "country.name")
  W = xtset(W,
            data = data,
            spec = spec,
            type = type,
            name = paste("World Bank Indicator(s)", x),
            url = "http://data.worldbank.org/data-catalog/world-development-indicators")
  return(W)
}  

#' Get UDS data in \code{xtdata} format
#'
#' Function to download the Unified Democracy Scores (UDS) by Pemstein, 
#' Meserve and Melton (2010). 
#' The result carries an \code{\link{xtdata}} attribute that can be passed to the 
#' \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @return a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Pemstein, Daniel, Stephen A. Meserve & James Melton. 2010. 
#' "Democratic Compromise: A Latent Variable Analysis of Ten Measures of 
#' Regime Type." \emph{Political Analysis} 18(4): 426-449.
#' \url{http://www.unified-democracy-scores.org/}
#' @examples
#' # By default, the function downloads the UDS dataset.
#' head(UDS <- get_uds())
#' # Basic visualization of average scores in the 2000s.
#' if(require(countrycode) & require(ggplot2)) {
#'   UDS$ccode = countrycode(UDS$ccodecow, "cown", "iso3n")
#'   xtmap(aggregate(uds_mean ~ ccode, mean, data = subset(UDS, year > 2000)), 
#'         "uds_mean", quantize = 5, continent = c("Africa", "Asia"), 
#'         iso3n = "ccode") + 
#'     scale_fill_brewer("Mean UDS since 2000", palette = "RdYlGn")
#' }

get_uds <- function() {
  url = "http://www.unified-democracy-scores.org/files/uds_summary.csv.gz"
  y = tempfile(fileext = ".csv.gz")
  download.file(url, y, quiet = TRUE)
  # exclude country names
  y = read.csv(gzfile(y), sep = ",")[, -1]
  # rename variables
  names(y) = c("year", "ccodecow", paste0("uds_", names(y)[-2:-1]))
  # xtdata specs  
  y = xtset(y, 
            data = c("ccodecow", "year"), 
            spec = c("cown", "year"), 
            type = "country", 
            name = "Unified Democracy Scores",
            url = "http://www.unified-democracy-scores.org/"
  )
  return(y)
}

#' Get state-level historical events from Gledistch & Ward and Powell & Thyne
#'
#' Function to download state-level historical events from Gleditsch and Ward (1999, updated 3 May 2013) and Powell and Thyne (2011, updated c. 2013). 
#' The result carries an \code{\link{xtdata}} attribute that can be passed to the 
#' \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @param start the first year of measurement to include. Defaults to \code{1945}.
#' @param end the last year of measurement to include. Defaults to \code{2013}.
#' @param independence name under which to create the state independence variable.
#' Defaults to \code{"gw_indep"}. See 'Details'.
#' @param coups name under which to create the state coups variable. 
#' Defaults to \code{"pt_coup"}. See 'Details'.
#' @details The variables produced by this function are \bold{gw_indep} (years of independence, coded 0/1), from Gleditsch and Ward, and \bold{pt_coup} (attempted and successful \emph{coups d'\'{E}tat}), from Powell and Thyne. The revised gross domestic product and population estimates from Gleditsch (2002) are based on older and shorter versions of the Penn World Table than the QOG datasets, and are therefore not included.
#' @return a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Gleditsch, Kristian S. & Michael D. Ward. 1999. "Interstate 
#' System Membership: A Revised List of the Independent States since 1816.". 
#' \emph{International Interactions} 25: 393-413, 
#' \url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.
#' 
#' Gleditsch, Kristian S. 2002. "Expanded Trade and GDP Data," 
#' \emph{Journal of Conflict Resolution} 46: 712-24.
#' 
#' Powell, Jonathan M. & Clayton L. Thyne. 2011.
#' "Global Instances of Coups from 1950 to 2010: A New Dataset.". 
#' \emph{Journal of Peace Research} 48(2): 249-259, 
#' \url{http://www.uky.edu/~clthyn2/coup_data/home.htm}.
#' @examples
#' # Download data up to 2012.
#' head(G <- get_gwpt(end = 2012))
#' if(require(countrycode) & require(ggplot2)) {
#'   # Get geographic markers.
#'   G$iso3c = countrycode(G$ccode, "iso3n", "iso3c")
#'   G$continent = countrycode(G$ccode, "iso3n", "continent")
#'   # Plot the full data.
#'   qplot(data = subset(G, !is.na(continent)),
#'             x = year, y = reorder(iso3c, as.numeric(pt_coup), mean),
#'             fill = continent, alpha = pt_coup, geom = "tile") + 
#'     scale_fill_brewer("Continent", palette = "Set1") +
#'     scale_alpha_manual("Event", values = c(0, .5, 1)) +
#'     scale_x_continuous(breaks = seq(1953, 2013, 10)) +
#'     labs(y = NULL)
#' }
#' if(require(ggplot2)) {
#'   # Time distribution.
#'   qplot(data = subset(G, pt_coup != "No verified coup attempt"), 
#'         x = year, fill = pt_coup, binwidth = 3, alpha = I(2/3),
#'         position = "stack", stat = "bin", geom = "bar") +
#'     theme(legend.position = "bottom") +
#'     scale_fill_brewer("", palette = "Set1") +
#'     labs(x = NULL)
#' }

get_gwpt <- function(start = 1945, end = 2013, 
                       independence = "gw_indep", coups = "pt_coup") {
  try_require("countrycode")
  message("Downloading data for years ", start, "-", end, "...")
  
  # Gleditsch and Ward independence data
  url = "http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat"
  x = read.table(url, quote = "", sep = "\t")[, -2:-3]
  names(x) = c("ccode", "start", "end")
  
  x$start = as.numeric(substring(x$start, 7))
  x$end = as.numeric(substring(x$end, 7))
  # drop left of timeline
  x = x[x$end > start, ]
  # right censor
  x[x$end >= end, ] = end
  
  # Powell and Thyne coup attempts data
  url = "http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt"
  y = read.csv(url, sep = "\t")[, -4:-5]
  # rename variables
  names(y) = c("country.name", "ccode", "year", coups)
  
  y$country.name[y$country.name == "Guinea Bissau"] = "Guinea-Bissau"
  
  d = expand.grid(unique(y[, "ccode"]), seq(start, end))
  names(d) = c("ccode", "year")
  d = d[order(d[, "ccode"]), ]
  
  for(i in 1:nrow(d)) {
    m = which(x[, "ccode"] == d[i, "ccode"])
    if(length(m))
      d[i, independence] = d[i, "year"] %in% seq(x$start[m], x$end[m])
  }
  d[, independence] = as.numeric(d[, independence])
  
  # add coups d'Etat
  d[, coups] = 0
  for(i in 1:nrow(y)) {
    m = which(d[, "ccode"] == y[i, "ccode"] & d[, "year"] == y[i, "year"])
    if(length(m))
      d[m, coups] = y[i, coups]
    else
      message("Excluding ", y[i, "country.name"], " ", y[i, "year"], " (out of time period)")
  }
  d[, coups] = factor(d[, coups], labels = c(
    "No verified coup attempt", 
    "Unsuccessful coup attempt", 
    "Successful coup attempt"))
  
  d = merge(unique(y[, 1:2]), d, by = "ccode", all.y = TRUE)
  # G+W country codes
  d[, "ccodegw"] = d[, "ccode"]
  d[, "ccode"] = countrycode(d[, "country.name"], "country.name", "iso3n")
  # historical states
  m = which(d[, "country.name"] == "Yemen Arab Republic; N. Yemen")
  d[m, "ccode"] = 886
  m = which(d[, "country.name"] == "Yemen People's Republic; S. Yemen")
  d[m, "ccode"] = 720
  d[, "country.name"] = NULL
  d = xtset(d,
            data = c("ccode", "year", "ccodegw"),
            spec = c("iso3n", "year", "ccodegw"),
            type = "country",
            name = "Gleditsch and Ward, Powell and Thyne state-level data")
  return(d)
}
