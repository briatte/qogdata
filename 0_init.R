
#
# keyword
#

code = "hlth_cd"

## packages

library(downloader)
library(plyr)
library(stringr)

## create folders

if(!file.exists("docs")) dir.create("docs")
if(!file.exists("data")) dir.create("data")

## create index

file = "docs/table_of_contents_en.txt"

## download

if(!file.exists(file))
  download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=", file), file)

## parse

data = "docs/datasets.txt"

if(!file.exists(data)) {
  index = readLines(file)
  index = unlist(str_extract_all(index, paste0("(.*)", code, "(.*)")))
  index = str_split(index, "\t")
  index = subset(ldply(index), V3 == "dataset")[, 1:7]
  
  names(index) = c("title", "code",  "type", "updated", "changed", "start", "end")
  index$title = str_trim(index$title)

  write.table(unique(index), data, row.names = FALSE, sep = "\t")
}

index = read.table(data, sep = "\t", header = TRUE)
message(nrow(index), " Eurostat datasets in index.")

# done
