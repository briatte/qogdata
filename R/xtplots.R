#' Plot availability of panel variables
#'
#' Function to plot data availability in \code{xtdata} datasets. 
#' Requires the \code{ggplot2} package.
#'
#' @export
#' @param data a dataset with the \code{xtdata} attribute.
#' @param variable the variable to plot.
#' @return a \code{ggplot2} object
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' if(require(ggplot2)) {
#'   # Load QOG demo datasets.
#'   data(qog.demo)
#'   xtmissing(qog.ts.demo, "chga_hinst") +
#'     ggtitle("Country-year availability of regime type")
#'   xtmissing(qog.ts.demo, "bl_asy15f") +
#'     ggtitle("Country-year availability of female education")
#' }
#' @keywords xt graphics

xtmissing <- function(data = NULL, variable) {
  #
  # checks
  #
  stopifnot(xtdata(data))
  stopifnot(variable %in% names(data))

  #
  # xt variables
  #
  id = ifelse(!is.null(xt(data)$data[3]), xt(data)$data[3], xt(data)$data[1])
  time = xt(data)$data[2]

  #
  # time range
  #
  x = !is.na(data[, variable])
  y = reorder(data[, id], as.numeric(x), mean)

  t = data[, time]
  r = aggregate(as.numeric(x) ~ t, mean, data = NULL)
  r = range(r[r[, 2] > 0, 1], na.rm = TRUE)
  message("Plotting years ", paste0(r, collapse = "-"),
          " (T = ", diff(r) + 1, ")")
  
  #
  # plot
  #
  g = qplot(x = t, y = y, fill = x, geom = "tile", size = I(6)) + 
    scale_fill_discrete("", labels = c("missing", "nonmissing")) + 
    labs(y = id, x = paste0("T = ", diff(r) + 1, " (", 
                            paste0(r, collapse = "-"), 
                            ")")
         ) + 
    theme(legend.position = "bottom") + 
    xlim(r)    
  return(g)
}


#' Plot country-level maps
#'
#' Function to plot country-level maps. 
#' Requires the \code{countrycode}, \code{ggplot2}, \code{mapproj} and 
#' \code{maps} packages.
#'
#' @export
#' @param data a data frame.
#' @param variable a variable name.
#' @param t the time period to plot from, as specified in the 
#' \code{\link{xtdata}} attribute of the data. If the data carries an 
#' \code{\link{xtdata}} attribute but \code{t} is left unspecified, the 
#' maximal value of the time period is used. See 'Details'.
#' @param ncol if the plot is faceted by time periods, the number of facet columns
#' @param nrow if the plot is faceted by time periods, the number of facet rows
#' @param continents a vector of continent names to subset the map to.
#' @param regions a vector of region names to subset the map to.
#' @param name a name to give to the color scale
#' @param title a title to give to the map
#' @param quantize how many quantiles to cut the variable into. 
#' Defaults to \code{FALSE}, which leaves \code{variable} unaffected.
#' @param quantize.t how many time periods to cut the time variable into.
#' Defaults to \code{FALSE}, which produces one map per time period. 
#' See 'Details'.
#' @param text.size the size for text elements.
#' @param iso3n the ISO-3N variable name, if you are using the function on 
#' cross-sectional data (which will return a warning), or if you are overriding 
#' the \code{\link{xtdata}} attribute of the data frame (which also returns a warning).
#' @param simplify the threshold of points under which to remove a geographic 
#' subregion. Set to something like \code{30} to remove islands and overseas 
#' areas. More of a bug than a feature. Defaults to \code{NULL}, which leaves 
#' the map intact.
#' @param ... other arguments passed to \code{\link[ggplot2]{map_data}} or 
#' \code{\link[maps]{map}}.
#' @details The function is intended as a helper to map country-year data. It
#' falls back to mapping the data as a cross-section if the data carries no
#' \code{\link{xtdata}} attribute or if \code{t} is left unspecified, in which 
#' case it will map the values of \code{variable} for the most recent time 
#' period (usually years).
#' 
#' When the data carries an \code{\link{xtdata}} attribute and \code{t} is 
#' specified, the function returns facet maps of the data, by time period. Use 
#' \code{ncol} and \code{nrow} to arrange the disposition of the maps. If 
#' \code{quantize.t} is specified, the time period is cut to quantiles and the  
#' mean value of \code{variable} are plotted, in order to plot things like 
#' average values of a variable over several decades.
#' @return a \code{ggplot2} object
#' @seealso \code{\link[ggplot2]{map_data}}, \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Fertility rates in Africa, most recent year.
#' xtmap(qog.ts.demo, "wdi_fr", continent = "Africa")
#' # Fertility rates in Africa, 1995.
#' xtmap(qog.ts.demo, "wdi_fr", 1995, continent = "Africa")
#' # Political regimes in Asia, excluding Russia, using cross-sectional data.
#' xtmap(subset(qog.cs.demo, ccode != 643), "chga_hinst", continent = "Asia", 
#'       iso3n = "ccode") + 
#'   geom_polygon(color = "grey25") +
#'   scale_fill_brewer("", palette = "Accent")
#' # Grayscale version.
#' xtmap(subset(qog.cs.demo, ccode != 643), "chga_hinst", continent = "Asia", 
#'       iso3n = "ccode") + 
#'   geom_polygon(color = "white") +
#'   scale_fill_grey("")
#' # Gross domestic product per capita in Asia, 1980-2009.
#' xtmap(qog.ts.demo, "wdi_gdpc", t = 1980:2009, continent = "Asia",
#'       quantize = 10, quantize.t = 4, text.size = 16)
#' @keywords xt graphics

xtmap <- function(data, variable, t = NULL,
                  continents = NULL, regions = NULL, name = "",
                  title = NULL, quantize = FALSE, quantize.t = FALSE, 
                  iso3n = NULL, simplify = NULL,
                  text.size = 12, ncol = NULL, nrow = NULL,
                  ...) {
  #
  # checks
  #
  try_require(c("countrycode", "ggplot2", "mapproj", "maps"))
  stopifnot(variable %in% names(data))

  ccode = xt(data)$data[1]
  cspec = xt(data)$spec[1]
  time  = xt(data)$data[2]
  if(is.null(ccode)) {
    # 
    # cross-section checks
    #
    if(is.null(iso3n))
      stop("no iso3n variable name")
    if(!iso3n %in% names(data))
      stop(iso3n, " does not exist in the data")
    ccode = data[, iso3n]
    t = NULL
    warning("cross-sectional map (unknown time period)")
  }
  else {
    #
    # xtdata checks
    #
    if(!is.null(iso3n)) {
      warning("overriding xtdata identifier with: ", iso3n)
      ccode = iso3n
      cspec = "iso3n"
    }
    if(!ccode %in% names(data))
      stop(ccode, " does not exist in the data")
    if(cspec != "iso3n") {
      data[, "iso3n"] = countrycode(data[, ccode], cspec, "iso3n")
      ccode = "iso3n"
      warning(ccode, " converted to iso3n to match map data")
    }
    data$ccode <- data[, ccode]
    #
    # time period
    #
    if(is.null(t)) {
      t = max(data[, time], na.rm = TRUE)
      warning("cross-sectional map (latest time period)")
    }
    data = data[data[, time] %in% t, ]
    if(length(t) > 1 & is.numeric(quantize.t)) {
      data[, time] = quantize(data[, time], quantize.t, TRUE)
      message("Subsetting to time periods: ", 
              paste0(levels(data[, time]), collapse = ", "))
      data = aggregate(data[, variable], 
                       by = list(data[, time], data[, "ccode"]), 
                       mean, na.rm = TRUE)
      names(data) = c(time, "ccode", variable)
    }
    else {
      message("Subsetting to time: ",
              ifelse(length(t) == 1, t, paste0(range(t), collapse = "-")))      
    }
  }
  #
  # map data
  #
  world = map_data("world", ...)
  world$ccode  = countrycode(world$region, "country.name", "iso3n")
  #
  # geo data
  #
  if(!is.null(continents)) {
    message("Subsetting to continent(s): ", 
            paste0(continents, collapse = ", "))
    world$continent = countrycode(world$ccode, "iso3n", "continent")
    world = world[world$continent %in% continents, ]
  }
  if(!is.null(regions)) {
    message("Subsetting to region(s): ", 
            paste0(regions, collapse = ", "))
    world$region = countrycode(world$ccode, "iso3n", "region")
    world = world[world$region %in% regions, ]
  }
  if(!is.null(simplify)) {
    islands = names(table(map$subregion))[table(map$subregion) < simplify]
    world = world[!world$subregion %in% islands, ]
  }
  #
  # merge data
  #
  choro = merge(data, world, by = "ccode", sort = FALSE)
  choro = choro[order(choro$order), ]
  #
  # quantize
  #
  if(!is.null(quantize)) {
    if(quantize > 1)
      choro[, variable] = quantize(choro[, variable], quantize)
  }
  #
  # plot
  #
  map = ggplot(data = choro) +
    aes(long, lat, group = group) +
    geom_polygon(aes_string(fill = variable)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_classic(text.size) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_map() # should fix this by passing orientation() parameters
  #
  # fill scale
  #
  if(is.numeric(choro[, variable])) {
    map = map + 
      scale_fill_gradient2(name, 
                           low = "steelblue", 
                           mid = "yellow",
                           high = "orangered", 
                           midpoint = median(choro[, variable], na.rm = TRUE))      
  }
  else if(is.factor(choro[, variable]) & is.ordered(choro[, variable])) {
    map = map + 
      scale_fill_brewer(name, palette = "RdYlBu")
  }
  else if(is.factor(choro[, variable])) {
    map = map + 
      scale_fill_discrete(name)
  }
  #
  # t-faceting
  #
  if(!is.null(t)) {
    if(length(t) > 1) {
      map = map + 
        facet_wrap(formula(paste("~", time)),
                   ncol = ncol, 
                   nrow = nrow) +
        theme(strip.background = element_rect(fill = NA, colour = NA))
    }
    else {
      map = map + 
        ggtitle(as.character(t))
    }
  }
  return(map)
}

#' xtplot
#'
#' Plot function for data frames with an \code{\link{xtdata}} attribute.
#' 
#' @name xtplot
#' @aliases xtline xtpoint xtstep xtarea
#' @export
#' @param data a data frame with an \code{\link{xtdata}} attribute.
#' @param variable the variable to plot.
#' @param name the name to give the color scale.
#' @examples
#' # Population in countries with a population over 100,000 people.
#' xtplot(xtsubset(qog.ts.demo, unna_pop > 10^8), "unna_pop") + geom_area()
#' # Health care expenditure per capita in a random country sample.
#' xtplot(xtsample(qog.ts.demo, 12), "wdi_hec", "line") + 
#'   geom_smooth() + geom_point() + 
#'   xlim(1995, 2010)
xtplot <- function(data, variable, name = "") {
  try_require("ggplot2")
  stopifnot(xtdata(data))
  stopifnot(variable %in% names(data))
  ggplot(data = data, 
         aes_string(fill = xt(data)$data[4], 
                    colour = xt(data)$data[4], 
                    x = xt(data)$data[2], 
                    y = variable)) +
    geom_blank()
}
