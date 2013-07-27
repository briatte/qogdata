
#
# keyword
#

code = paste0("hlth_cd_y")
code = paste0(code, collapse = "|")
code = paste0("(.*)(", code, ")(.*)")

## packages

library(downloader)
library(ggplot2)
library(plyr)
library(reshape2)
library(stringr)

## create folders

if(!file.exists("docs")) dir.create("docs")
if(!file.exists("data")) dir.create("data")

## save NUTS-2 codes

# nuts = url("http://ec.europa.eu/eurostat/ramon/documents/nuts/NUTS_2010.zip")
# xlsx::read.xlsx(unz(nuts, "NUTS_2010.xls"), 1)
# close(nuts)

## create index

file = "docs/table_of_contents_en.txt"

## download

if(!file.exists(file))
  download(paste0("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=", file), file)

## parse

data = "docs/datasets.txt"

index = readLines(file)
index = unlist(str_extract_all(index, code))
index = str_split(index, "\t")
index = subset(ldply(index), V3 == "dataset")[, 1:7]
index = unique(index)

names(index) = c("title", "code",  "type", "updated", "changed", "start", "end")
index$title = str_trim(index$title)

if(!file.exists(data))
  write.table(index, data, row.names = FALSE, quote = FALSE, sep = "\t")

message(nrow(index), " Eurostat datasets in index.")
print(index[, -1])

# done
