
source("0_init.R")

## index is what will be downloaded and assembled

ext = ".tsv.gz"

files = sapply(index$code, function(x) {
  file = paste0("data/", x, ext)
  if(!file.exists(file)) {
    message("downloading: ", x)
    download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownlo adListing?sort=1&file=data%2F", x, ext), file, mode = "wb", quiet = TRUE)
  }
  message("[", which(index$code == x), "] reading: ", file)
  z = gzfile(file)
  dat = read.table(z, sep = "\t", header = TRUE)
  message(nrow(dat), " rows, ", ncol(dat), " columns")
  head = ldply(str_split(dat[, 1], ","))
  names(head) = unlist(strsplit(names(dat)[1], "\\."))[1:ncol(head)]
  cbind(head, dat[, -1])
})

if(!file.exists("data/raw.Rda"))
  save.image("data/raw.Rda")

#files