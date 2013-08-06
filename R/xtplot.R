#' Plot availablity of panel data
#'
#' Function to plot data availability in \code{xtdata} datasets. 
#' Requires the \code{ggplot2} package.
#'
#' @export
#' @param data a dataset with the \code{xtdata} attribute.
#' @param variable the variable to plot.
#' @value a \code{ggplot2} object
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' xtmissing(qog.ts.demo, "chga_hinst") +
#'   ggtitle("Country-year availability of regime type")
#' xtmissing(qog.ts.demo, "bl_asy15f") +
#'   ggtitle("Country-year availability of female education")

xtmissing <- function(data = NULL, variable) {
  stopifnot(xtdata(data))
  stopifnot(variable %in% names(data))
  # try short names
  id = xt(data)$data[3]
  # if null, revert
  if(is.null(id))
    id = xt(data)$data[1]
  stopifnot(id %in% names(data))
  time = xt(data)$data[2]
  stopifnot(time %in% names(data))

  if(require(ggplot2)) {
    x = !is.na(data[, variable])
    t = data[, time]
    r = aggregate(as.numeric(x) ~ t, mean, data = NULL)
    r = r[r[, 2] > 0, 1]
    r = range(r)
    message("Plotting years ", paste0(r, collapse = "-"),
            " (T = ", r[2] - r[1] + 1, ")")
    y = reorder(data[, id], as.numeric(x), mean)
    g = qplot(x = t, y = y, fill = x, geom = "tile", size = I(6)) + 
      scale_fill_discrete("", labels = c("missing", "nonmissing")) + 
      labs(y = "Country", x = NULL) + 
      theme(legend.position = "bottom") + 
      xlim(r)    
    return(g)
  }
}


#' Map \code{xtdata} variables
#'
#' Function to plot maps of Quality of Government (QOG) data. Requires the \code{ggplot2} and \code{maps} packages.
#'
#' @export
#' @param data the QOG data frame. The function requires the \code{ccode} variable from the QOG \code{std} and \code{soc} datasets, as well as the \code{countrycode}, \code{ggplot2} and \code{maps} packages.
#' @param variable the QOG variable name to colour the map with, in quotes.
#' @param continents a vector of continent names to subset the map to.
#' @param regions a vector of region names to subset the map to.
#' @param name the legend name
#' @param title the map title
#' @param quantize whether to cut the variable into quantiles.
#' @param text.size the size for text elements.
#' @param t the time period to plot from. The maximal value is used by default,
#' as with 'most recent year' in country-year data.
#' @param iso3n the ISO-3N variable name, if you are using the function on cross-sectional data (which will return a warning).
#' @param ... other arguments passed to \code{map_data}.
#' @value a \code{ggplot2} object
#' @seealso \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
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
#'       iso3n = "ccode")
#' # Education levels in Central America, using cross-sectional data.
#' xtmap(qog.cs.demo, "bl_asy25mf", quantize = 3, 
#'       region = c("Central America", "South America"), 
#'       iso3n = "ccode") +
#'       scale_fill_brewer("", palette = "Blues")

xtmap <- function(data, variable, t = NULL,
                  continents = NULL, regions = NULL, name = "",
                  title = NULL, quantize = FALSE, text.size = 12, 
                  iso3n = NULL,
                  ...) {
  stopifnot(variable %in% names(data))
  ccode = xt(data)$data[1]
  if(is.null(ccode)) {
    warning("mapping as a cross-section from iso3n parameter")
    stopifnot(!is.null(iso3n))
    stopifnot(iso3n %in% names(data))
    ccode = data[, iso3n]
    t = NULL
  }
  else {
    stopifnot(ccode %in% names(data))
    stopifnot(xt(data)$spec[1] == "iso3n")
    data$ccode <- data[, ccode]
    # default subset is to max year
    if(is.null(t))
      t = max(data[, xt(data)$data[2]], na.rm = TRUE)
    message("Subsetting to time: ", t)
    data = data[data[, xt(data)$data[2]] %in% t, ]
  }
  if (require(maps) & require(ggplot2) & require(countrycode)) {
    #
    # map data
    #
    world <- map_data("world", ...)
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
    #
    # merge data
    #
    choro = merge(data, world, by = "ccode", sort = FALSE)
    choro = choro[order(choro$order), ]
    #
    # quantize
    #
    if(as.numeric(quantize) > 1) {
      quantile(choro[, variable], na.rm = TRUE)
      choro[, variable] = cut(choro[, variable],
                              quantile(choro[, variable], 
                                       probs = seq(0, 1, by = 1/quantize), 
                                       na.rm = TRUE),
                              include.lowest = TRUE)
    }
    #
    # plot
    #
    map = qplot(long, lat, data = choro, group = group, fill = choro[, variable],
                geom = "polygon") +
      labs(title = title, x = NULL, y = NULL) +
      theme_classic(text.size) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      coord_map() # should fix this by passing orientation() parameters
    #
    # fill scale
    #
    if(class(choro[, variable]) == "numeric") {
      map = map + 
        scale_fill_gradient2(name, 
                             low = "steelblue", 
                             mid = "yellow",
                             high = "orangered", 
                             midpoint = median(choro[, variable], na.rm = TRUE))      
    }
    else if(class(choro[, variable]) == "factor") {
      map = map + 
        scale_fill_discrete(name)
    }
    if(!is.null(t))
      map = map + ggtitle(as.character(t))
    return(map)
  }
}
