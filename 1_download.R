
source("0_init.R")

## index is what will be downloaded and assembled

ext = ".tsv.gz"

for(x in index$code) {
  file = paste0("data/", x, ext)
  if(!file.exists(file)) {
    message("downloading: ", file)
    download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownlo adListing?sort=1&file=data%2F", x, ext), file, mode = "wb", quiet = TRUE)
  }
  gz = gzfile(file)
  data = read.table(gz, sep = "\t", header = TRUE, 
                    na.strings = ": ", stringsAsFactors = FALSE)

  message("[", which(index$code == x), "] reading: ", file, " (", nrow(data), " rows, ", ncol(data), " columns)")

  # header
  head = ldply(str_split(data[, 1], ","))
  names(head) = unlist(strsplit(names(data)[1], "\\."))[1:ncol(head)]
  data = cbind(head, data[, -1])

  # subset
  data = subset(data, icd10 %in% c("A-R_V-Y", "A_B", "C", "F", "I", "J", "V01-Y89"))
  data = subset(data, !age %in% c("Y1-4", "Y10-14", "Y15-19", "Y_LT1"))

  # years
  year = grepl("^X", names(data))
  names(data)[year] = gsub("X", "", names(data)[year])
  names(data)[year] = gsub("_", "-", names(data)[year])
  data = melt(data, id.vars = names(data)[!year], variable = "year")

  # flags
  data$flag = str_extract(data$value, " (.*)")
  data$flag[data$flag == " "] = NA
  data$value = gsub(" (.*)", "", data$value)
  data$value[data$value == ":"] = NA
#   data$sex = NULL
#   data$unit = NULL

  # save
  file = gsub(ext, ".txt", file)
  write.table(data, file, row.names = FALSE, na = "", quote = FALSE, sep = "\t")
  message(file, " is ready (", file.info(file)$size, " bytes).")
}

#files
