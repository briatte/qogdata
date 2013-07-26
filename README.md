Inspired by @ajdamico's [usgsd](https://github.com/ajdamico/usgsd/), here's a collection of scripts to cram large quantities of data from Eurostat into a nice R data frame.

Currently:

* grabs the index of all datasets with a given prefix; see the [table of contents](http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=table_of_contents_en.pdf) for the codes
* downloads all files cited in the index, and extracts the data from the gzipped files (slow)

Next move: find a way to safely collate the columns (which are _not_ the same in each segment of the data).

Residual testing for now.
