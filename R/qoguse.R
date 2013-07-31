qoguse <- function(file = FALSE, replace = FALSE, path = "", version = "std", format = "cs", codebook = FALSE, variables = NULL, years = NULL, ...) {
  #
  # currently available
  #
  versions = list(
    std = c("ts", "cs", "csyom"),
    bas = c("ts", "cs"),
    exp = c("ctry", "ind"),
    soc = c("tsl", "tsw"))
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
    if(!file.exists(file) | replace) {
      message("Downloading ", link, "...")
      download.file(link, file, mode = "wb", quiet = TRUE)
    }
  }
  #
  # reader call
  #
  read = "read.csv"
  args = list(file = file, ...)
  if(grepl(".csv$", file)) args["sep"] = ";"
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
  if(!is.null(variables))
    data = data[, names(data) %in% c("cname", variables)]
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
  if(isTRUE(codebook) || grepl(".pdf", codebook)) {
    link = "http://www.qogdata.pol.gu.se/data/Codebook_QoG_Std15May13.pdf"
    if(version == "bas") {
      link = "http://www.qogdata.pol.gu.se/codebook/codebook_basic_20120608.pdf"
    }
    if(isTRUE(codebook) & version == "bas") {
      codebook = "codebook_basic_20120608.pdf"
    }
    else if(isTRUE(codebook)) {
      codebook = "Codebook_QoG_Std15May13.pdf"
    }
    if(!file.exists(codebook)) {
      message("Downloading codebook to ", codebook, "...")
      download.file(link, codebook, mode = "wb", quiet = TRUE)
    }
    message("Codebook: ", codebook)
  }
  return(data)
}

# QOG <- qoguse(file = "data/qog-soc.dta", version = "soc", format = "tsl", years = 1982:1984)
# qoguse()
# dim(qoguse(file = TRUE, format = "ts"))
# QOG = qoguse("data/qog.std.csv", format = "cs", replace = TRUE)
# QOG = qoguse("data/qog.std.csv", codebook = "data/qog.std.codebook.pdf")