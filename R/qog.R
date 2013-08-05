
#' Import Quality of Government data into R
#'
#' Function to download Quality of Government (QOG) data and load it as a data 
#' frame in R. The result carries an \code{\link{xtdata}} attribute that can be 
#' passed to the \code{\link{xtmerge}} panel data method.
#'
#' @export
#' @aliases get_qog
#' @param file a filename to save the dataset at. If set to \code{TRUE}, the name of the CSV dataset on the QOG server will be used. If set to \code{FALSE} (the default), the function only returns the link to the dataset. QOG dataset versions other than \code{std} require that \code{file} ends in \code{.dta}.
#' @param replace whether to download the dataset even if a file already exists at the download location. Defaults to \code{FALSE}.
#' @param path a folder path to prepend to the filename and to the codebook if relevant.
#' @param version the QOG version: \code{std} (Standard), \code{soc} (Social Policy), \code{bas} (Basic) or \code{exp} (Expert). Defaults to \code{std}.
#' @param format the QOG format, usually \code{cs} for cross-sectional data or \code{ts} for time series in the \code{std} and \code{bas} versions. See 'Details' for the full list of specifications. Defaults to \code{cs}.
#' @param codebook whether to download the codebook. Calls \code{qogbook} by passing the \code{codebook}, \code{version} and \code{path} arguments to it, where \code{codebook} is treated as the filename for the codebook. Defaults to \code{FALSE}.
#' @param variables a selection of variables to import. \code{ccode} ISO-3N country codes and \code{year} identifiers will be forced into the output if relevant.
#' @param years a selection of years to import. Effective only with the \code{ts}, \code{tsl} or \code{ind} formats.
#' @param ... other arguments supplied to the import method, which is `read.csv` by default, or \code{foreign::read.dta} if \code{file} is a Stata \code{dta} dataset, or \code{Hmisc::spss.get} if \code{file} is a SPSS \code{sav} dataset.
#' @details This version of the package handles all four QOG datasets:
#' \tabular{lcl}{
#'  QOG Standard \tab \code{std} \tab 15 May 2013\cr
#'  QOG Social Policy \tab \code{soc} \tab 4 April 2012\cr
#'  QOG Basic \tab \code{bas}): \tab 28 March 2011\cr
#'  QOG Expert Survey \tab \code{exp} \tab 3-6 September 2012\cr
#'  URL: \tab \tab \url{http://www.qog.pol.gu.se}\cr
#' }
#' 
#' \itemize{
#'   \item QOG datasets \code{std} and \code{bas} 
#'   require format \code{cs} (cross-section) 
#'   or \code{ts} (time series).
#'   \item QOG dataset \code{soc} 
#'   requires format \code{cs}, \code{tsl} (time series, long) 
#'   or \code{tsw} (time series, wide)
#'   \item QOG dataset \code{exp} 
#'   requires format \code{cntry} (country-level) 
#'   or \code{ind} (individual survey)
#' }
#' 
#' If format is \code{csyom}, \code{version} is automatically set to \code{std}, 
#' and \code{file} must be a CSV file.
#' 
#' The function mimics Christoph Thewes' \code{qoguse} Stata command.
#' @seealso \code{\link{qogbook}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Show URL to QOG Standard time series.
#' qogdata()
#' # Show URL to QOG Social Policy time series, long format.
#' QOG = qogdata(file = "qog-soc.dta", version = "soc", format = "tsl")
#' ## Download codebook and recent years from QOG Basic cross-section (not run).
#' # QOG = qogdata(file = "qog.cs.txt", version = "bas", format = "cs", 
#' #         years = 2002:2012, codebook = TRUE)
#' ## Download QOG Standard cross-section years of measurement (not run).
#' # QOG = qogdata(tempfile(fileext = ".csv"), format = "csyom")
#' ## Show QOG years of measurement for Gini coefficient (not run).
#' # table(QOG$wdi_gini)

qogdata <- function(file = FALSE, replace = FALSE, codebook = FALSE, path = "",
                version = "std", format = "cs", 
                variables = NULL, years = NULL, ...) {
  #
  # currently available
  #
  versions = list(
    std = c("ts", "cs", "csyom"),
    bas = c("ts", "cs"),
    exp = c("ctry", "ind"),
    soc = c("cs", "tsl", "tsw"))
  #
  # correct version
  #
  if(!version %in% names(versions)) {
    stop("Invalid version: use one of ", 
         paste0(names(versions), collapse = ", "))
  }
  #
  # correct format
  #
  if(format == "csyom") {
    version = "std"
    if(!grepl(".csv$|.txt$", file))
      file = gsub("(\\.|\\w){4}$", ".csv", file)
  }
  if(!format %in% unlist(versions[version])) {
    stop("Invalid format: use one of ", 
         paste0(unlist(versions[version]), collapse = ", "))
  }
  #
  # automatic filename
  #
  if(isTRUE(file)) {
    file = paste0("qog_", 
                  version, 
                  "_", 
                  format, 
                  ifelse(version == "std", 
                         paste0("_", "15May13.csv"), 
                         ".dta")
                  )
  }
  else {
    if(is.character(file) & version != "std" & !grepl(".dta$", file)) {
      file = gsub("(\\.|\\w){4}$", ".dta", file)
      warning("QOG datasets other than std are available only as Stata files.\n",
           "  The filename that you specified was modified to ", file)      
    }
  }
  if(is.character(path))
    if(nchar(path) > 0) file = paste(path, file, sep = "/")
  #
  # online source
  #
  link = paste0("http://www.qogdata.pol.gu.se/data/",
                ifelse(version == "std", "QoG", "qog"),
                "_", version, "_", format, 
                ifelse(version == "std", paste0("_", "15May13"), ""),
                ifelse(version == "std" & grepl("csv|dta|sav", file), 
                       substring(file, nchar(file) - 3),
                       ".dta")
  )
  if(is.logical(file)) {
    return(link)
  }
  else {
    if(replace || !file.exists(file)) {
      message("Downloading ", link, "...")
      download.file(link, file, mode = "wb", quiet = TRUE)
    }
    else {
      message("Loading from disk...")
    }
  }
  #
  # reader call
  #
  read = "read.csv"
  args = list(file = file, ...)
  if(!grepl(".dta$|.sav$", file)) args["sep"] = ";"
  if(grepl(".dta$", file)) {
    library(foreign)
    read = "read.dta"
    if(is.null(unlist(args["warn.missing.labels"])))
      args["warn.missing.labels"] = FALSE
  }
  if(grepl(".sav$", file)) {
    library(Hmisc)
    read = "Hmisc::spss.get"
  }
  data = do.call(read, args)
  #
  # selected variables
  #
  if(!is.null(variables)) {
    if(grepl("ts|tsl", format) & !"year" %in% variables) {
      warning("Forcing year identifier into the dataset.")
      variables = c("year", variables)
    }
    if(grepl("std|bas|soc", version) & !"ccode" %in% variables) {
      warning("Forcing country code identifier into the dataset.")
      variables = c("ccode", variables)
    }
    data = data[, names(data) %in% variables]
  }
  #
  # selected years
  #
  if(!is.null(years) && format %in% c("ts", "tsl", "ind"))
    data = data[data$year %in% years, ]
  #
  # message
  #
  message("Loaded ", file, " (N = ", nrow(data),
          ifelse(format %in% c("ts", "tsl", "ind"),
                 paste0(", ", min(data$year), 
                        "-", max(data$year), 
                        ", T = ", length(unique(data$year))),
                 ""),
          ").")
  #
  # grab codebook
  #
  if(isTRUE(codebook) || grepl(".pdf", codebook))
    qogbook(codebook, version, path, replace)
  #
  # xtdata spec
  #
  if(format == "ts" | format == "tsl") {
    data = xtset(data, 
                 data = c("ccode", "year", "ccodealp", "cname"), 
                 spec = c("iso3n", "year"), 
                 type = "country", 
                 name = "Quality of Government, time series data"
    )
  }
  #
  # finish line
  #
  return(data)
}

#' Download Quality of Government codebooks
#'
#' Function to download Quality of Government (QOG) codebooks. Please visit the QOG Institute website at \url{http://www.qog.pol.gu.se/} for a presentation of QOG research.
#'
#' @export
#' @param file a filename to save the codebook at. 
#' If set to \code{TRUE}, the name of the codebook on the QOG server will be used. 
#' If set to \code{FALSE} (the default), the function only returns the link to 
#' the dataset. The filename must end in \code{.pdf}.
#' @param replace whether to download the dataset even if a file already exists 
#' at the download location. Defaults to \code{FALSE}.
#' @param path a folder path to append to the filename.
#' @param version the QOG version: \code{std} (Standard), 
#' \code{soc} (Social Policy), \code{bas} (Basic) or \code{exp} (Expert). 
#' Defaults to \code{std}.
#' @details The function mimics Richard Svensson's \code{qogbook} Stata command.
#' @seealso \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Show the URL to the QOG Standard dataset codebook.
#' qogbook()
#' ## Download QOG Standard codebook with default filename (not run).
#' # qogbook(file = TRUE)
#' ## Download QOG Basic dataset codebook to specific filename (not run).
#' # qogbook(file = "qog.basic.codebook.pdf", version = "bas")

qogbook <- function(file = FALSE, version = "std", path = "", replace = FALSE) {
  if(!version %in% c("std", "bas"))
    stop("Codebook available only for versions bas, std")
  
  link = "http://www.qogdata.pol.gu.se/data/Codebook_QoG_Std15May13.pdf"
  if(version == "bas") {
    link = "http://www.qogdata.pol.gu.se/codebook/codebook_basic_20120608.pdf"
  }
  if(isTRUE(file) & version == "bas") {
    file = "codebook_basic_20120608.pdf"
  }
  else if(isTRUE(file)) {
    file = "Codebook_QoG_Std15May13.pdf"
  }
  #
  # path
  #
  if(is.character(path))
    if(nchar(path) > 0) file = paste(path, file, sep = "/")
  #
  # download
  #
  if(is.logical(file)) {
    return(link)
  }
  else if(!grepl(".pdf$", file)) {
    stop("Please specify a .pdf codebook filename or set file = TRUE.")
  }
  else {  
    if(replace || !file.exists(file)) {
      message("Downloading codebook to ", file, "...")
      download.file(link, file, mode = "wb", quiet = TRUE)
    }
    message("Codebook: ", file)
  }
}

#' Find Quality of Government variables
#'
#' Function to perform a \code{regex} search on QOG variable names and labels. 
#' A few labels are missing for strictly cross-sectional variables.
#'
#' @export
#' @param ... keywords or \code{regex} phrases passed to \code{grepl}.
#' @param version the QOG version to search: either \code{std} (the default) or \code{soc}.
#' @param compact whether to limit the labels returned to 32 characters. Defaults to \code{TRUE} for better console output.
#' @param show which variables to show for years of measurement: \code{cs} (cross-sectional), \code{ts} (time series), or \code{all} (the default).
#' @value a data frame containg matching variables, described by their names, labels and years of measurement in the time series (\code{ts}) cross-sectional (\code{cs}) datasets. The information should match the ranges indicated in the \emph{QOG Standard Codebook} and \emph{QOG Social Policy Codebook}.
#' @references
#' Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo Rothstein. 
#' 2012. \emph{The QoG Social Policy Dataset}, version 4Apr12. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \url{http://www.qog.pol.gu.se}.
#' 
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren Holmberg, 
#' Bo Rothstein, Petrus Sundin & Richard Svensson. 2013. 
#' \emph{The Quality of Government Dataset}, version 15May13. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \url{http://www.qog.pol.gu.se}.
#' @seealso \code{\link{grep}}, \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # QOG Standard search.
#' qogfind("regime", "institutions")
#' # QOG Standard search, with regex syntax.
#' qogfind("public|administration")
#' # QOG Social Policy search, showing cross-sectional information only.
#' head(qogfind("^socx", version = "soc", show = "cs", compact = FALSE))
#' # QOG Standard variables featured only in the cross-sectional version.
#' qogfind("*")[is.na(qogfind("*")$ts.N), ]

qogfind <- function(..., version = "std", compact = TRUE, show = "all") {
  x = paste0(c(...), collapse = "|")
  data(qog.index)
  if (version == "std") {
    message("QOG Standard results")
    r = qog.std.index[grepl(x, qog.std.index$variable, ignore.case = TRUE) | 
                        grepl(x, qog.std.index$label, ignore.case = TRUE), ]
  }
  if (version == "soc") {
    message("QOG Social Policy results")
    r = qog.soc.index[grepl(x, qog.soc.index$variable, ignore.case = TRUE) | 
                        grepl(x, qog.soc.index$label, ignore.case = TRUE), ]
  }
  if (version == "bas") {
    message("QOG Basic results")
    r = qog.bas.index[grepl(x, qog.bas.index$variable, ignore.case = TRUE) | 
                        grepl(x, qog.bas.index$label, ignore.case = TRUE), ]
  }
  r$variable = as.character(r$variable)
  if(compact) r$label = substr(r$label, 1, 32)
  if(show == "cs") r = r[, !grepl("ts.", names(r))]
  if(show == "ts") r = r[, !grepl("cs.", names(r))]
  return(r)
}

#' Join historical and recent states in QOG Standard time series data
#'
#' Function to plot maps of Quality of Government (QOG) data. Requires the \code{ggplot2} and \code{maps} packages.
#'
#' @export
#' @param data a QOG Standard time series dataset, or any data frame with \code{cname} (country) and \code{year} information coded as in the QOG Standard time series dataset.
#' @param country the country name to join data over. Requires the \code{cname} variable.
#' @details The function will try to find two series of country-year observations that both match the \code{country} argument. Within the QOG Standard time series dataset, this will match historical states like "France (-1962)" to modern states like "France (1963-)". The function will then create a new variable out of both series, joined at their separation years, and set its country code attributes to the most recent ones. See Appendix A of the \emph{QOG Standard Codebook} for details on historical states in the QOG Standard time series dataset.
#' @value a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren Holmberg, 
#' Bo Rothstein, Petrus Sundin & Richard Svensson. 2013. 
#' \emph{The Quality of Government Dataset}, version 15May13. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \url{http://www.qog.pol.gu.se}.
#' @examples
#' # Load QOG demo datasets.
#' data(qog.demo)
#' QOG = qog.ts.demo
#' QOG = qogjoin(QOG, "Ethiopia")
#' QOG = qogjoin(QOG, "France")
#' QOG = qogjoin(QOG, "Malaysia")
#' QOG = qogjoin(QOG, "Pakistan")

qogjoin <- function(data, country = NULL) {
  stopifnot("cname" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(is.character(country))
  x = data$cname[grepl(country, data$cname)]
  x = unique(x)
  if(length(x) == 0)
    stop("No country match for ", country)
  else if(length(x) == 1)
    stop("Single country match for ", country)
  else if(length(x) > 2)
    stop("More than two country matches: ", paste0(x, collapse = ", "))
  
  t = xt(data)
  
  y = as.numeric(gsub("\\D", "", x))
  min = x[order(y) == 1]
  max = x[order(y) == 2]
  message("Joining ", min, " to ", max)
  
  one = data[data$cname == min & data$year <= y[order(y) == 1], ]
  two = data[data$cname == max & data$year >= y[order(y) == 2], ]
  new = rbind(one, two)
  
  new$cname = country
  
  # country codes
  if("ccode" %in% names(data))
    new$ccode = unique(data$ccode[data$cname == max])
  if("ccodealp" %in% names(data))
    new$ccodealp = unique(data$ccodealp[data$cname == max])
  if("cname_year" %in% names(data))
    new$cname_year = paste(new$cname, new$year)
  if("ccodealp_year" %in% names(data))
    new$ccodealp_year = paste(new$ccodealp, new$year)
  if("ccodewb" %in% names(data))
    new$ccodewb = unique(data$ccodewb[data$cname == max])
  
  message(country, " now runs over ", nrow(new), " country-year observations, ",
          paste0(range(new$year), collapse = "-"))

  data[, "cname"] = as.character(data[, "cname"])
  data[data$cname == max, ] = new
  data = subset(data, cname != min)
  data[, "cname"] = factor(data[, "cname"])
  
  data = xtset(data,
               data = t$data,
               spec = t$spec,
               name = t$name,
               url = t$url,
               quiet = FALSE)
  return(data)
}
