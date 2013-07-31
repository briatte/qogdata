#' qogdata - Import Quality of Government data into R
#'
#' Function to download Quality of Government (QOG) data and load it as a data frame in R. Please visit the QOG Institute website at \link{http://www.qog.pol.gu.se/} for a presentation of QOG research.
#'
#' @export
#' @param file a filename to save the dataset at. If set to \code{TRUE}, the name of the CSV dataset on the QOG server will be used. If set to \code{FALSE} (the default), the function only returns the link to the dataset. QOG datasets other than \code{std} require that \code{file} ends in \code{.dta}. QOG dataset \code{csyom} requires that \code{file} ends in \code{.csv}.
#' @param replace whether to download the dataset even if a file already exists at the download location. Defaults to \code{FALSE}.
#' @param path a folder path to append to the filename.
#' @param version the QOG version: \code{std} (Standard), \code{soc} (Social Policy), \code{bas} (Basic) or \code{exp} (Expert). Defaults to \code{std}.
#' @param format the QOG format, usually \code{cs} for cross-sectional data or \code{ts} for time series in the \code{std} and \code{bas} versions. QOG dataset \code{soc} requires \code{cs}, \code{tsl} (time series, long) or \code{tsw} (time series, wide). QOG dataset \code{exp} requires \code{cntry} (country-level) or \code{ind} (individual survey). If format is \code{csyom}, \code{version} must be \code{std} and \code{file} must be a CSV file. Defaults to \code{cs}.
#' @param codebook whether to download the codebook. Calls \code{qogbook} by passing the \code{codebook}, \code{version} and \code{path} arguments to it, where \code{codebook} is treated as the filename for the codebook. Defaults to \code{FALSE}.
#' @param variables a selection of variables to import. \code{ccodealp} ISO-3C country codes and \code{year} identifiers will be forced into the output if relevant.
#' @param ... other arguments supplied to the import method, which is `read.csv` by default, or \code{foreign::read.dta} if \code{file} is a Stata \code{dta} dataset, or \code{Hmisc::spss.get} if \code{file} is a SPSS \code{sav} dataset.
#' @seealso \code{\link{qogbook}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Show the URL to the QOG Standard dataset.
#' qogdata()
#' # Download QOG Standard time series dataset and codebook.
#' qogdata(format = "ts", file = "qog.ts.csv", codebook = TRUE)
#' # Download selected years from the QOG Social Policy dataset.
#' QOG = qogdata(file = "qog-soc.dta", version = "soc", format = "tsl", years = 2002:2012)
#' # Download QOG Standard cross-section years of measurement.
#' QOG = qogdata(tempfile(fileext = ".csv"), format = "csyom")
#' # Display the years of measurements for the WDI Gini coefficient.
#' barplot(table(QOG$wdi_gini))

qogdata <- function(file = FALSE, replace = FALSE, path = "", version = "std", format = "cs", codebook = FALSE, variables = NULL, years = NULL, ...) {
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
  if(!format %in% unlist(versions[version])) {
    stop("Invalid format: use one of ", 
         paste0(unlist(versions[version]), collapse = ", "))
  }
  #
  # automatic filename
  #
  if(isTRUE(file)) {
    file = paste0("qog_", version, "_", format, ifelse(version == "std", paste0("_", "15May13.csv"), ".dta"))
  }
  else {
    if(version != "std" & !grepl(".dta$", file))
      stop("QOG datasets other than std are available only as Stata files.\n",
           "  Please specify a .dta filename or set file = TRUE.")
    if(format == "csyom" & version != "std")
      stop("The csyom dataset is available only for the std version.\n",
           "  Please use version = 'std' and a CSV filename.")
    if(format == "csyom" & !grepl(".csv$|.txt$", file))
      stop("The csyom dataset is available only as a CSV file\n",
           "  Please specify a CSV filename or set file = TRUE.")
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
                ifelse(grepl("csv|dta|sav", file), 
                       substring(file, nchar(file) - 3),
                       ".csv")
  )
  if(is.logical(file)) {
    return(link)
  }
  else {
    if(replace || !file.exists(file)) {
      message("Downloading ", link, "...")
      download.file(link, file, mode = "wb", quiet = TRUE)
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
    if(grepl("std|bas|soc", version) & !"ccodealp" %in% variables) {
      warning("Forcing country code identifier into the dataset.")
      variables = c("ccodealp", variables)
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
  # finish line
  #
  return(data)
}

#' qogdata - Import Quality of Government data into R
#'
#' Alias for the \code{qogdata} function, for maximum resemblance with Christoph Thewes' \code{QOG} package for Stata.
#'
#' @export
#' @param ... arguments supplied to the \code{qogdata} function.
#' @seealso \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}

qoguse <- function( ...) {
  qogdata(...)
}

#' qogbook - Download Quality of Government codebooks
#'
#' Function to download Quality of Government (QOG) codebooks. Please visit the QOG Institute website at \link{http://www.qog.pol.gu.se/} for a presentation of QOG research.
#'
#' @export
#' @param file a filename to save the codebook at. If set to \code{TRUE}, the name of the codebook on the QOG server will be used. If set to \code{FALSE} (the default), the function only returns the link to the dataset. The filename must end in \code{.pdf}.
#' @param replace whether to download the dataset even if a file already exists at the download location. Defaults to \code{FALSE}.
#' @param path a folder path to append to the filename.
#' @param version the QOG version: \code{std} (Standard), \code{soc} (Social Policy), \code{bas} (Basic) or \code{exp} (Expert). Defaults to \code{std}.
#' @seealso \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Show the URL to the QOG Standard dataset codebook.
#' qogbook()
#' # Download with default filename.
#' qogbook(file = TRUE)
#' # Download the QOG Basic dataset codebook to a specific filename.
#' qogbook(file = "qog.basic.codebook.pdf", version = "bas")

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
  if(save.labels) {
    
    message("Variable labels: ", file)
  }
}

#' qogfind - find Quality of Government variables
#'
#' Function to perform a \code{regex} search on QOG variable names and labels. 
#' A few labels are missing for strictly cross-sectional variables.
#'
#' @export
#' @param ... keywords or \code{regex} phrases passed to \code{grepl}.
#' @param version the QOG version to search: either \code{std} (the default) or \code{soc}.
#' @param shorter.labels whether to abbreviate the labels to 32 characters. Defaults to \code{TRUE} for better console output.
#' @value a data frame containg matching variables, described by their names, labels and years of measurement in the time series (\code{ts}) cross-sectional (\code{cs}) datasets. The information should match the ranges indicated in the \emph{QOG Standard Codebook} and \emph{QOG Social Policy Codebook}.
#' @references
#' Svensson, Richard, Stefan Dahlberg, Staffan Kumlin & Bo Rothstein. 
#' 2012. \emph{The QoG Social Policy Dataset}, version 4Apr12. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \link{http://www.qog.pol.gu.se}.
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Sören Holmberg, 
#' Bo Rothstein, Petrus Sundin & Richard Svensson. 2013. 
#' \emph{The Quality of Government Dataset}, version 15May13. 
#' University of Gothenburg: The Quality of Government Institute, 
#' \link{http://www.qog.pol.gu.se}.
#' @seealso \code{\link{grep}}, \code{\link{qogdata}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # QOG Standard search.
#' qogfind("regime", "institutions")
#' # QOG Standard search, with regex syntax.
#' qogfind("public|administration")
#' # QOG Social Policy search, showing cross-sectional information only.
#' head(qogfind("^socx", version = "soc", show = "cs", shorter.labels = FALSE))
#' # QOG Standard variables featured only in the cross-sectional version.
#' qogfind("*")[is.na(qogfind("*")$ts.N), ]

qogfind <- function(..., version = "std", shorter.labels = TRUE, show = "all") {
  data = paste("data/qog", version, "index.Rda", sep = ".")
  load(data)
  x = paste0(c(...), collapse = "|")
  if (version == "std") {
    message("QOG Standard results:")
    r = qog.std.index[grepl(x, qog.std.index$variable, ignore.case = TRUE) | 
                        grepl(x, qog.std.index$label, ignore.case = TRUE), ]
  }
  if (version == "soc") {
    message("QOG Social Policy results:")
    r = qog.soc.index[grepl(x, qog.soc.index$variable, ignore.case = TRUE) | 
                        grepl(x, qog.soc.index$label, ignore.case = TRUE), ]
  }
  r$variable = as.character(r$variable)
  if(shorter.labels) r$label = substr(r$label, 1, 32)
  if(show == "cs") r = r[, !grepl("ts.", names(r))]
  if(show == "ts") r = r[, !grepl("cs.", names(r))]
  return(r)
}
