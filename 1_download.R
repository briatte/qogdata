
## scan index

library(downloader)
library(plyr)
library(stringr)

dir.create("docs")

code = "hlth_cd"
file = "docs/table_of_contents_en.txt"

## download

if(!file.exists(file))
  download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=", file), file)

## parse

index = readLines(file)
index = unlist(str_extract_all(index, paste0("(.*)", code, "(.*)")))
index = str_split(index, "\t")

## dataset

index = subset(ldply(index), V3 == "dataset")[, 1:7]
names(index) = c("title", "code",  "type", "last_update", "last_change", "start", "end")
index$title = str_trim(index$title)

write.table(index, "docs/datasets.txt", row.names = FALSE)

## index is what will be downloaded and assembled

ext = ".tsv.gz"

files = sapply(index$code, function(x) {
  file = paste0("data/", x, ext)
  if(!file.exists(file)) {
    message("downloading: ", x)
    download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F", x, ext), file, mode = "wb", quiet = TRUE)
  }
  message("reading: ", file)
  z = gzfile(file)
  dat = read.table(z, sep = "\t", header = TRUE)
  print(nrow(dat))
  dat
})

if(!file.exists("data/files.Rda"))
  save.image("data/files.Rda")

#files