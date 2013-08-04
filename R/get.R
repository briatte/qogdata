
#' Get World Development Indicators in \code{xtdata} format
#'
#' Function to download World Development Indicators (WDI) through the 
#' World Bank API, using the \code{\link[WDI]{WDI}} package. If the 
#' \code{\link[countrycode]{countrycode}} package is available, it is used to
#' add ISO-3N country codes to the data frame. The result carries an \code{xdata}
#' attribute that can be passed to the \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @param x the name(s) of the indicator(s) to download from the World Development Indicators API. See the documentation of the \code{\link[WDI]{WDI}} function.
#' @param country the ISO-2C country codes for which to download the indicators. Defaults to \code{"all"}, which downloads all available data.
#' @param start first year of data to download. Defaults to \code{"auto"}, which sets the year to the minimum year of the original dataset.
#' @param end last year of data to download. Defaults to \code{"auto"}, which sets the year to the maximum year of the original dataset.
#' @param add a vector of variable names from the WDI query that should be returned with the indicators. See the documentation of the \code{\link[WDI]{WDI}} function.
#' @value a data frame with country-year observations 
#' and an \code{\link{xtdata}} attribute
#' @seealso \code{\link[WDI]{WDI}}, \code{\link{xtdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Arel-Bundock, Vincent. 2012. \emph{WDI: World Development Indicators 
#' (World Bank).} R package version 2.2. \url{http://CRAN.R-project.org/package=WDI}
#' World Bank. 2013. \emph{World Development Indicators}. 
#' Washington DC: The World Bank Group. Online publication, January 24, 2013.
#' @examples
#' # Download QOG Standard time series data for WDI healthcare expenditure.
#' QOG = qogdata(tempfile(fileext = ".dta"), format = "ts", 
#'               years = 1995:2010, variables = c("ccodealp", "wdi_hec"))
#' # Merge QOG data to corresponding WDI series; add income classification.
#' WDI = get_wdi(x = "SH.XPD.PCAP.PP.KD", add = "income")
#' # Plot results: dots are QOG measurements, lines are WDI measurements.
#' if(require(ggplot2))
#'   qplot(data = subset(QOG, !is.na(wdi_hec)), 
#'         x = year, y = wdi_hec, color = income, alpha = I(.5)) + 
#'   geom_smooth(aes(y = SH.XPD.PCAP.PP.KD, group = ccode), se = FALSE) +
#'   scale_colour_brewer("", palette = "RdYlBu") +
#'   labs(y = NULL, x = NULL) +
#'   theme_minimal(16)

get_wdi <- function(x, country = "all", start = 1945, end = 2013, add = NULL) {
  if(require(WDI)) {
    WDI <- WDI(country = country, indicator = x, start = start, end = end, extra = TRUE, cache = NULL)
    WDI$income = factor(WDI$income, levels = c("High income: OECD", "High income: nonOECD", "Upper middle income", "Lower middle income", "Low income"), ordered = TRUE)
    if(require(countrycode)) {
      WDI[, "iso3n"] = countrycode(WDI$iso3c, "iso3c", "iso3n")
      WDI = WDI[, c("iso3n", "year", x, add)]
      WDI = xtset(WDI,
                  data = c("iso3n", "year"),
                  spec = c("iso3n", "year"),
                  type = "country",
                  name = paste("World Bank Indicator(s)", x),
                  url = "http://data.worldbank.org/data-catalog/world-development-indicators")
    }
    else {
      warning("Install the countrycode package to get the data with iso3n codes.") 
      WDI = WDI[, c("iso3c", "year", x, add)]
      WDI = xtset(WDI,
                  data = c("iso3c", "year"),
                  spec = c("iso3c", "year"),
                  type = "country",
                  name = paste("World Bank Indicator(s)", x),
                  url = "http://data.worldbank.org/data-catalog/world-development-indicators")
    }    
    return(WDI)
  }  
}

#' Get UDS data in \code{xtdata} format
#'
#' Function to download the Unified Democracy Scores (UDS) by Pemstein, 
#' Meserve and Melton (2010). The result carries an \code{xdata} attribute 
#' that can be passed to the \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @param data a QOG Standard time series dataset, or any data frame with \code{cname} (country) and \code{year} information coded as in the QOG Standard time series dataset.
#' @param id the country variable to join data over. Defaults to \code{ccodecow}, the Correlate of War numeric country code used in the Unified Democracy Scores dataset.
#' @param year the year variable to join data over. Defaults to \code{"year"}.
#' @param out what to return. If set to \code{"data"}, the joined QOG/UDS dataset is returned. Defaults to \code{"uds"}, which only returns the UDS dataset.
#' @param ... other parameters passed to \code{merge}. Set \code{out} to \code{"data"} for the function to return the joined QOG/UDS dataset.
#' @value a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Pemstein, Daniel, Stephen A. Meserve & James Melton. 2010. 
#' "Democratic Compromise: A Latent Variable Analysis of Ten Measures of 
#' Regime Type." \emph{Political Analysis} 18(4): 426-449.
#' \url{http://www.unified-democracy-scores.org/}
#' @examples
#' # By default, the function downloads the UDS dataset.
#' head(UDS <- merge_uds())
#' # Basic visualization of average scores in the 2000s.
#' if(require(countrycode)) {
#'   UDS$ccode = countrycode(UDS$ccodecow, "cown", "iso3n")
#'   qogmap(aggregate(uds_mean ~ ccode, mean, data = subset(UDS, year > 2000)), 
#'          "uds_mean", quantize = 5, continent = c("Africa", "Asia")) + 
#'     scale_fill_brewer("Mean UDS since 2000", palette = "RdYlGn")
#' }
#' ## Inner merge to QOG (not run).
#' # QOG <- qogdata(file = TRUE, format = "ts")
#' # UDS <- get_uds()
#' # data <- xtmerge(QOG, UDS)

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

#' Add and merge state-level historical events to QOG Standard time series data
#'
#' Function to download state-level historical events from Gleditsch and Ward (1999, updated 3 May 2013) and Powell and Thyne (2011, updated c. 2013). The result might be simultaneously merged to QOG Standard time series data.
#'
#' @export
#' @param data a QOG Standard time series dataset, or any data frame with \code{cname} (country) and \code{year} information coded as in the QOG Standard time series dataset.
#' @param id the country variable to join data over. Defaults to \code{ccode}, the ISO3-N numeric country code used in the Quality of Government dataset. See 'Details'.
#' @param year the year variable to join data over. Defaults to \code{"year"}.
#' @param independence name under which to create the state independence variable.
#' Defaults to \code{"gw_indep"}. See 'Details'.
#' @param coups name under which to create the state coups variable. 
#' Defaults to \code{"pt_coup"}. See 'Details'.
#' @param out what to return. If set to \code{"data"}, the joined 
#' QOG/Gleditsch and Ward/Powell and Thyne dataset is returned. 
#' Defaults to \code{"state"}, which only returns the 
#' Gleditsch and Ward/Powell and Thyne dataset/
#' @param ... other parameters passed to \code{merge}. Set \code{out} to \code{"data"} for the function to return the joined QOG/Gleditsch and Ward/Powell and Thyne dataset.
#' @details If \code{out} is set to anything but \code{"data"}, the returned country codes are named \code{ccodegw} and respect the original country codes of the Gleditsch and Ward dataset, which are also used in the Powell and Thyne dataset.
#' The variables produced by this function are \bold{gw_indep} (years of independence, coded 0/1), from Gleditsch and Ward, and \bold{pt_coup} (attempted and successful \emph{coups d'\'{E}tat}), from Powell and Thyne. The revised gross domestic product and population estimates from Gleditsch (2002) are based on older and shorter versions of the Penn World Table than the QOG datasets, and are therefore not included.
#' @value a data frame with country-year observations
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
#' # By default, the function downloads the gw_indep and pt_coups variables.
#' head(GIC <- merge_state())
#' # Show country-years.
#' if(require(plyr)) {
#'   ddply(GIC, .(country.name), summarize, n = length(year), min = min(year), max = max(year))
#' }
#' # Plot the full data.
#' if(require(countrycode) & require(ggplot2)) {
#'   GIC$iso3c = countrycode(GIC$country.name, "country.name", "iso3c")
#'   GIC$continent = countrycode(GIC$country.name, "country.name", "continent")
#'   qplot(data = subset(GIC, !is.na(continent)),
#'             x = year, y = reorder(iso3c, as.numeric(pt_coup), mean),
#'             fill = continent, alpha = pt_coup, geom = "tile") + 
#'     scale_fill_brewer("Continent", palette = "Set1") +
#'     scale_alpha_manual("Event", values = c(0, .5, 1)) +
#'     scale_x_continuous(breaks = seq(1953, 2013, 10)) +
#'     labs(y = NULL)
#' }
#' if(require(ggplot2)) {
#' qplot(data = subset(GIC, pt_coup != "No verified coup attempt"), 
#'       x = year, fill = pt_coup, binwidth = 3, alpha = I(2/3),
#'       position = "stack", stat = "bin", geom = "bar") +
#'   theme(legend.position = "bottom") +
#'   scale_fill_brewer("", palette = "Set1") +
#'   labs(x = NULL)  
#' }
#' ## Left outer merge to QOG (not run).
#' # QOG <- qogdata(file = TRUE, format = "ts")
#' # QOG <- get_state(QOG)

get_state <- function(start = 1945, end = NULL, 
                      independence = "gw_indep", coups = "pt_coup") {
  
  message("Downloading data for years ", start, "-", end, "...")
  
  # Gleditsch and Ward independence data
  url = "http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat"
  x = read.table(url, quote = "", sep = "\t")[, -2:-3]
  names(x) = c(id, "start", "end")
  
  x$start = as.numeric(substring(x$start, 7))
  x$end = as.numeric(substring(x$end, 7))
  x = subset(x, end >= start)
# shoudl that work?  
#   if(!is.null(end)) x[x$end >= end, ]) = end
  
  # Powell and Thyne coup attempts data
  url = "http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt"
  y = read.csv(url, sep = "\t")[, -4:-5]
  # rename variables
  names(y) = c("country.name", id, year, coups)
  
  y$country.name[y$country.name == "Guinea Bissau"] = "Guinea-Bissau"
  
  d = expand.grid(unique(y[, id]), seq(tmin, tmax))
  names(d) = c(id, year)
  d = d[order(d[, id]), ]
  
  for(i in 1:nrow(d)) {
    m = which(x[, id] == d[i, id])
    if(length(m))
      d[i, independence] = d[i, year] %in% seq(x$start[m], x$end[m])
  }
  d[, independence] = as.numeric(d[, independence])
  
  # add coups d'Etat
  d[, coups] = 0
  for(i in 1:nrow(y)) {
    m = which(d[, id] == y[i, id] & d[, year] == y[i, year])
    if(length(m))
      d[m, coups] = y[i, coups]
    else
      message("Excluding ", y[i, "country.name"], " ", y[i, year], " (out of time period)")
  }
  d[, coups] = factor(d[, coups], labels = c(
    "No verified coup attempt", 
    "Unsuccessful coup attempt", 
    "Successful coup attempt"))
  
  if(require(countrycode)) {
    d = merge(unique(y[, 1:2]), d, by = id, all.y = TRUE)
    d[, "ccodegw"] = d[, id]
    d[, id] = countrycode(d[, "country.name"], "country.name", "iso3n")
    # historical states
    m = which(d[, "country.name"] == "Yemen Arab Republic; N. Yemen")
    d[m, id] = 886
    m = which(d[, "country.name"] == "Yemen People's Republic; S. Yemen")
    d[m, id] = 720
    d[, "country.name"] = NULL
    d = xtset(d,
              data = c("ccode", "year", "ccodegw"),
              spec = c("iso3n", "year", "ccodegw"),
              type = "country",
              name = "Gleditsch and Ward, Powell and Thyne state-level data")
  }
  else {
    d = merge(unique(y[, 1:2]), d, by = id, all.y = TRUE)
    warning("Caution: Gleditsch and Ward country codes look like ISO-3 but aren't.",
            "Install the countrycode package to get the data with iso3n codes.")
    names(d)[names(d) == id] = "ccodegw"
    d = xtset(d,
              data = c("ccodegw", "year", "country.name"),
              spec = c("ccodegw", "year", "country.name"),
              type = "country",
              name = "Gleditsch and Ward, Powell and Thyne state-level data")
  }
  return(d)
}
