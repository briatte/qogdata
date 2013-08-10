#' Panel data properties
#'
#' Function to check for panel data properties in a data frame. The syntax is
#' provided in the 'Details' of \code{\link{xtset}}. Intended for programming
#' purposes.
#'
#' @export
#' @param dataset the dataset in which to check for \code{xtdata} attributes.
#' @return an error if the data frame has no \code{xtdata} attributes, or 
#' \code{TRUE} if the data frame has an \code{xtdata} attribute that conforms 
#' to the syntax of \code{\link{xtset}}.
#' @seealso \code{\link{xtset}}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Check xtdata attribute of QOG time series demo dataset.
#' xtdata(qog.ts.demo)
#' ## Unsuccessful checks send back an error (not run).
#' # xtdata(qog.cs.demo)
#' @keywords xt

xtdata <- function(dataset) {
  if(is.null(xt(dataset)))
    stop("data frame has no xtdata attribute")
    
  if(!all(sapply(xt(dataset), is.character))) {
    stop("invalid xtdata specification (all parameters must be character strings)")
  }

  xtcheck <- function(dataset, x, y = 0) {
    a = xt(dataset)[[x]]
    if(y > 0) a = a[y]
    if(is.null(a) | is.na(a) | !nchar(a))
      stop("invalid xtdata specification (", 
           ifelse(x != "type", 
                  paste(ifelse(y == 1, "unique identifier", "time period"),
                        ifelse(x == "data", "variable", "format")),
                  "type"),
           " is missing, null or null-length)")
    # find variables
    if(x == "data" & !a %in% names(dataset))
      stop("invalid xtdata specification (variable ", a, " does not exist in the data)")
  }
  xtcheck(dataset, "type", 1)
  xtcheck(dataset, "data", 1)
  xtcheck(dataset, "data", 2)
  xtcheck(dataset, "spec", 1)
  xtcheck(dataset, "spec", 2)
  
  # should now only return TRUE
  all(sapply(xt(dataset), is.character)) &
    all(sapply(xt(dataset)$data[1:2], nchar) > 0) &
    all(sapply(xt(dataset)$spec[1:2], nchar) > 0)
}

#' Get panel data properties
#'
#' Function to extract the panel data properties of a data frame. The syntax is
#' provided in the 'Details' of \code{\link{xtset}}. Intended for programming
#' purposes.
#'
#' @export
#' @param dataset the dataset from which to return the \code{xtdata} attributes.
#' @return a list of \code{xtdata} attributes, if it exists.
#' @seealso \code{\link{xtset}}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Identify the QOG Basic time series data properties.
#' xt(qog.ts.demo)
#' @keywords xt

xt <- function(dataset) {
  x = attr(dataset, "xtdata")
  return(x)
}

#' Set panel data properties
#'
#' Function to specify the panel data properties of a data frame. The syntax is
#' provided in the 'Details'. Intended for country-year cross-sectional 
#' time series (CSTS) data, but should fit any panel data format.
#'
#' @export
#' @param dataset the panel data frame to set the attributes to.
#' @param data the \code{data} parameters to pass to the data:
#' unique identifier and time period, optionally followed by short and long 
#' observation names. 
#' Defaults to QOG country-year data settings. See 'Details'.
#' @param spec the \code{spec} parameters to pass to the data:
#' formats of unique identifier and time period, optionally followed by formats
#' of short and long observation names. 
#' Defaults to QOG country-year data settings. See 'Details'.
#' @param type the type of observations in the panel data. 
#' Defaults to \code{"country"}, which will read the \code{spec} parameters as
#' \code{\link[countrycode]{countrycode}} formats.
#' @param name a description of the dataset.
#' @param url a URI locator for the dataset (the website address).
#' @param quiet whether to return some details. Defaults to \code{FALSE} (verbose).
#' @details an \code{xtdata} attribute is a list of 5 to 11 panel data 
#' paramaters stored as character strings in the following named vectors: 
#' \itemize{
#'   \item \bold{data}, a vector of variable names coding for the unique 
#'   identifier and time period, optionally followed by the variable names for
#'   the short and long observation names, as with alphabetical country codes
#'   and country names.
#'   \item \bold{spec}, a vector of variable formats that match the \code{data} 
#'   variables; this allows to store \code{\link[countrycode]{countrycode}} 
#'   formats in the dataset and can theoretically work with any other format, 
#'   like FIPS or households.
#'   \item \bold{type}, a vector that defines the type of 
#'   observations contained in the data; \code{"country"} is the only type that
#'   currently produces any specific behaviour from \code{xtdata} through the 
#'   \code{\link[countrycode]{countrycode}} package, but
#'   \code{"region"} is on the way for working with NUTS codes, and it should 
#'   be feasable to implement FIPS codes.
#'   \item \bold{name}, an optional vector that defines the 
#'   dataset name, which is printed on top of the function results.
#'   \item \bold{url}, an optional vector that defines the 
#'   URI locator for the data source (typically, the website address where to 
#'   find codebooks, technical documentation and related publications).
#' }
#' These characteristics mimic some of the behaviour of the \code{xtset} and 
#' \code{label data} commands in Stata.
#' @return a data frame with the \code{xtdata} attribute.
#' @seealso \code{\link[countrycode]{countrycode}}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Set xtdata attribute on QOG time series.
#' QOG = xtset(qog.ts.demo)
#' # Set xtdata attribute on recent years.
#' QOG.200x = xtset(subset(qog.ts.demo, year > 1999))
#' # Manually set xtdata attribute for UDS dataset.
#' UDS = get_uds()
#' UDS = xtset(UDS, 
#'             data = c("ccodecow", "year"), 
#'             spec = c("cown", "year"), 
#'             type = "country", 
#'             name = "Unified Democracy Scores"
#'     )
#' @keywords xt

xtset <- function(dataset = NULL, 
                  data = c("ccode", "year", "ccodealp", "cname"), 
                  spec = c("iso3n", "year", "iso3c", "country.name"),
                  type = "country",
                  name = "Quality of Government, time series data",
                  url = "http://www.qog.pol.gu.se/",
                  quiet = FALSE) {
  if(!"data.frame" %in% class(dataset))
    warning("Untested with objects of class other than data.frame.")
  
  # set attributes
  attr(dataset, "xtdata") = list(
    data = data,
    spec = spec,
    type = type,
    name = name,
    url = url)
  
  # set class
  if(!"xtdata" %in% class(dataset))
    class(dataset) = c(class(dataset), "xtdata")
  
  # check syntax
  if(!xtdata(dataset))
    stop("invalid xtdata specification (please report this bug)")
  
  # print name
  if(!is.null(xt(dataset)$name) & !quiet)
    message(xt(dataset)$name)
  
  # print panel variable
  id = xt(dataset)$data[1]
  
  # enforce iso3n recommendation
  if("country" %in% xt(dataset)$type & xt(dataset)$spec[1] == "iso3n")
    msg = "ISO-3166-1 numeric standard"
  else if ("country" %in% xt(dataset)$type)
    msg = "possibly non-unique"
  else
    msg = "undefined coding scheme"
  if(!quiet)
    message("Panel variable: ", id, " (N = ", 
            length(unique(dataset[, id])), ", ",
            msg, ")")
  
  # print time variable
  time = xt(dataset)$data[2]
  r = range(dataset[, time], na.rm = TRUE)
  if(!quiet)
    message("Time variable: ", 
            time, 
            " (", paste0(r, collapse = "-"), 
            ", T = ", diff(r) + 1, ")" )
  
  # sort_df
  dataset = sort_df(dataset, c(id, time))
  return(dataset)
}

#' Describe panel data
#' 
#' Function to describe the unique identifier, time period and distribution of 
#' $T$ for a data frame carrying an \code{xtdata} attribute. The function is 
#' similar to the \code{xtdes} command in Stata.
#' 
#' @export
#' @param data a data frame carrying an \code{xtdata} attribute
#' @return a vector
#' @seealso \code{\link{xtset}}
#' @examples
#' q = xtdes(qog.ts.demo)
#' summary(q)
#' @keywords xt

xtdes <- function(data) {
  stopifnot(xtdata(data))
  
  ccode = xt(data)$data[1]
  x = unique(data[, ccode])
  message(ccode, ": ", paste0(x[1:2], collapse = ", "), "..., ", rev(x)[1],
          " (N = ", length(x), ")")
  
  time = xt(data)$data[2]
  y = unique(data[, time])
  message(time, ": ", paste0(y[1:2], collapse = ", "), "..., ", rev(y)[1],
          " (T = ", length(y), ")")
  
  z = tapply(data[, time], data[, ccode], length)
  return(z)
}

#' Try ISO-3N country code conversion on \code{xtdata} data frames
#' 
#' This function tests conversions to ISO3-N country codes on the country 
#' codes, acronyms and names identified in a data frame that carries the
#' \code{\link{xtdata}} attribute with the \code{country} type. Used by
#' \code{\link{xtmerge}}.
#' 
#' @export
#' @param dataset a data frame with the \code{\link{xtdata}} attribute. The 
#' \code{type} parameter must be set to \code{"country"}.
#' @return a vector of how many observations were successfully matched on their
#' country code, short country name and long country name, based on the 
#' variable names specified as \code{\link{xtdata}} properties.
#' @seealso \code{\link{xtdata}}, \code{\link{xtmerge}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' if(require(countrycode)) {
#'   # Test the country identifiers in the QOG dataset.
#'   data(qog.demo)
#'   xtcountry(qog.ts.demo) 
#' }
#' @keywords xt country

xtcountry <- function(dataset) {
  stopifnot(xtdata(dataset))
  stopifnot("country" %in% xt(dataset)$type)
  data = xt(dataset)$data
  spec = xt(dataset)$spec
  countrytest <- function(x, y = NULL, data = NULL, spec = NULL) {
    if(is.na(data[x]) | is.na(spec[x]) | !data[x] %in% names(y)) {
      y = 0
    }
    else {
      y = sum(as.numeric(!is.na(countrycode(y[, data[x]], spec[x], "iso3n"))))
    }
    y
  }
  data = sapply(seq_along(data)[-2], countrytest, y = dataset, data = data, spec = spec)
  names(data) = xt(dataset)$data[-2]
  return(data)
}

#' Merge \code{xtdata} data frames
#'
#' This function merges panel data based on their \code{"xtdata"} attributes.
#' 
#' @export
#' @param x a data frame with the \code{\link{xtdata}} attribute. See 'Details'.
#' @param y a data frame with the \code{\link{xtdata}} attribute. See 'Details'.
#' @param t the name of the time period variable in the data frames, which 
#' propagates to \code{t.x} and \code{t.y}. Defaults to \code{"year"}.
#' @param t.x the name of the time period variable in the first dataset.
#' @param t.y the name of the time period variable in the second dataset.
#' @param ... other methods passed to \code{\link{merge}}, typically 
#' instructions on whether to perform an inner or outer merge; \code{xtmerge} 
#' defaults, like \code{merge}, to an inner merge.
#' @details The function is intended to work as \code{merge} with a safety 
#' check: it will refuse to merge data that do not carry identical formats 
#' for their unique identifiers and time periods, as it will refuse to merge 
#' data of different primary \code{type}.
#' 
#' If the \code{type} parameter is set to \code{"country"}, the function will 
#' also try to resolve data frames with different country code formats 
#' by matching them to \code{iso3n} codes with \code{\link{xtcountry}}.
#' @return a data frame
#' @seealso \code{\link{xtcountry}}, \code{\link{xtdata}}, \code{\link{merge}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' if(require(countrycode)) {
#'   # Load QOG demo datasets.
#'   data(qog.demo)
#'   # Load UDS democracy scores.
#'   UDS = get_uds()
#'   # Merge QOG and UDS time series.
#'   xt(xtmerge(qog.ts.demo, UDS))
#'   names(xtmerge(qog.ts.demo, UDS))
#' }
#' @keywords xt

xtmerge <- function(x, y, t = "year", t.x = NULL, t.y = NULL, ...) {
  try_require("countrycode")
  stopifnot(xtdata(x))
  stopifnot(xtdata(y))
  if(is.null(t.x)) t.x = t
  if(is.null(t.y)) t.y = t
  if(xt(x)$data[2] != t.x)
    stop("t.x different from xtdata time period of x: ", xt(x)$data[2])
  if(xt(y)$data[2] != t.y)
    stop("t.y different from xtdata time period of y: ", xt(y)$data[2])
  if(xt(x)$type[1] != xt(y)$type[1])
    stop("different xtdata primary types: ", xt(x)$type[1], xt(y)$type[1])
  if(xt(x)$spec[2] != xt(y)$spec[2])
    stop("different xtdata time period formats: ", xt(x)$spec[2], xt(y)$spec[2])  
  if(xt(x)$spec[1] != xt(y)$spec[1] & 
       "country" %in% xt(x)$type) {
    warning("merged different country code formats on iso3n best matches.")
    mx = xtcountry(x)
    my = xtcountry(y)
    p = rbind(round(100 * mx / nrow(x)),
              round(100 * my / nrow(y)))
    rownames(p) = c("x", "y")
    # compare
    ox = order(mx, decreasing = TRUE)[1]
    oy = order(my, decreasing = TRUE)[1]
    x[, "iso3n"] <- countrycode(x[, xt(x)$data[ox]], xt(x)$spec[ox], "iso3n")
    y[, "iso3n"] <- countrycode(y[, xt(y)$data[oy]], xt(y)$spec[oy], "iso3n")
    x = xtset(x, 
              data = c("iso3n", xt(x)$data[-1]), 
              spec = c("iso3n", xt(x)$spec[-1]), 
              type = xt(x)$type, 
              name = xt(x)$name,
              url = xt(x)$url,
              quiet = TRUE)
    y = xtset(y, 
              data = c("iso3n", xt(y)$data[-1]), 
              spec = c("iso3n", xt(y)$spec[-1]), 
              type = xt(y)$type, 
              name = xt(y)$name,
              url = xt(y)$url,
              quiet = TRUE)
  }
  stopifnot(xt(x)$spec[1] == xt(y)$spec[1])
  d = merge(x, y, 
            by.x = c(xt(x)$data[1], xt(x)$data[2]),
            by.y = c(xt(y)$data[1], xt(y)$data[2]), ...)
  name = "unknown"
  if(length(c(xt(x)$name, xt(y)$name)) > 0)
    name = paste(xt(x)$name, xt(y)$name, sep = " / ")
  url = "unknown"
  if(length(c(xt(x)$url, xt(y)$url)) > 0)
    url = c(xt(x)$url, xt(y)$url)
  d = xtset(d, 
            data = xt(x)$data, 
            spec = xt(x)$spec,
            type = xt(x)$type,
            name = name,
            url = url)
  return(d)
}

#' Subset a data frame while preserving its \code{\link{xtdata}} attribute
#'
#' A wrapper of the \code{\link{subset}} function that preserves 
#' the \code{\link{xtdata}} attribute.
#' 
#' The method is explained at \url{https://github.com/hadley/devtools/wiki/Computing-on-the-language#non-standard-evaluation-in-subset}.
#' 
#' @export
#' @param data a data frame with the \code{\link{xtdata}} attribute
#' @param formula a logical formula to subset to.
#' @param select the names of the variables to keep.
#' @param drop passed on to \code{[} indexing operator.
#' @param ... other methods passed to \code{\link{subset}}
#' @return a data frame
#' @seealso \code{\link{xtdata}}, \code{\link{subset}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Subset to two countries.
#' QOG = xtsubset(qog.ts.demo, cname %in% c("China", "India"))
#' if(require(ggplot2)) {
#'   # Plot log-population curves.
#'   qplot(data = QOG, y = unna_pop, x = year, colour = cname, geom = "step") + 
#'     scale_colour_brewer(palette = "Set1") +
#'     scale_y_log10()
#' }
#' @keywords xt

xtsubset <- function(data, formula, select = names(data), drop = FALSE) {
  stopifnot(xtdata(data))
  xtdata = xt(data)
  keep = eval(substitute(formula), data)
  data = data[keep, select, drop = drop]
  data = xtset(data, 
            data = xtdata$data, 
            spec = xtdata$spec,
            type = xtdata$type,
            name = xtdata$name,
            url = xtdata$url)
  return(data)
}

#' Sample out of an \code{\link{xtdata}} data frame
#' 
#' Function to extract a sample of observations out of a panel dataset, 
#' preserving all time measurements for each sampled observation.
#' 
#' @export
#' @param data data frame carrying an \code{\link{xtdata}} data frame
#' @param n how many observations to sample
#' @seealso \code{\link{sample}}
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Random sample of ten QOG countries.
#' unique(xtsample(qog.ts.demo, 10)$cname)
#' # Random cross-section of year 2000.
#' xtsubset(xtsample(qog.ts.demo, 10), year == 2000)[, 1:5]
#' @keywords xt

xtsample <- function(data, n = 20) {
  stopifnot(xtdata(data))
  stopifnot(is.numeric(n))
  
  uid = xt(data)$data[1]
  sample = sample(data[, uid], n, replace = F)
  sample = data[, uid] %in% sample
  data = do.call("xtsubset", args = list(data = data, formula = sample))
  return(data)
}
