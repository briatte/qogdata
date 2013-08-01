
#' Map Quality of Government data
#'
#' Function to plot maps of Quality of Government (QOG) data. Requires the \code{ggplot2} and \code{maps} packages.
#'
#' @export
#' @param data the QOG data frame. The function requires the \code{ccodealp} variable from the QOG \code{std} and \code{soc} datasets, as well as the \code{countrycode}, \code{ggplot2} and \code{maps} packages.
#' @param variable the QOG variable name to colour the map with, in quotes.
#' @param continents a vector of continent names to subset the map to.
#' @param regions a vector of region names to subset the map to.
#' @param name the legend name
#' @param title the map title
#' @param quantize whether to cut the variable into quantiles.
#' @param text.size the size for text elements.
#' @param ... other arguments passed to \code{map_data}.
#' @value a \code{ggplot2} object
#' @seealso \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Fertility rates in Africa.
#' # Temporary dataset.
#' QOG = qogdata(tempfile(fileext = ".dta"), warn.missing.labels = FALSE,
#'               variables = c("wdi_fr", "chga_hinst", "bl_asy25mf"))
#' # Fertility rates in Africa.
#' qogmap(QOG, "wdi_fr", continent = "Africa")
#' # Political regimes in Asia.
#' qogmap(subset(QOG, ccodealp != "RUS"), "chga_hinst", continent = "Asia")
#' # Education levels in Central America.
#' qogmap(QOG, "bl_asy25mf", quantize = 3, 
#'        region = c("Central America", "South America")) +
#'        scale_fill_brewer("", palette = "Blues")

qogmap <- function(data, variable, continents = NULL, regions = NULL, name = "",
                   title = NULL, quantize = FALSE, text.size = 12, ...) {
  stopifnot("ccodealp" %in% names(data))
  stopifnot(variable %in% names(data))
  if (require(maps) & require(ggplot2) & require(countrycode)) {
    #
    # map data
    #
    world <- map_data("world", ...)
    world$ccodealp  = countrycode(world$region, "country.name", "iso3c")
    #
    # geo data
    #
    if(!is.null(continents)) {
      message("Subsetting to continents: ", paste0(continents, collapse = ", "))
      world$continent = countrycode(world$ccodealp, "iso3c", "continent")      
      world = world[world$continent %in% continents, ]
    }
    if(!is.null(regions)) {
      message("Subsetting to regions: ", paste0(regions, collapse = ", "))
      world$region = countrycode(world$ccodealp, "iso3c", "region")
      world = world[world$region %in% regions, ]
    }
    #
    # merge data
    #
    choro = merge(data, world, by = "ccodealp", sort = FALSE)
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
    return(map)
  }
}
