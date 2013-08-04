The __qogdata__ package is a collection of functions to manipulate [Quality of Government](http://www.qog.pol.gu.se/) datasets and codebooks in R. It provides a few additional services to map QOG variables and to merge QOG datasets with other country-level data sources.

Version 0.1 of the `qogdata` package is installable with the `devtools` package:

    library(devtools)
    devtools::install_github("qogdata", "briatte", dependencies = TRUE)

Please post comments, issues and suggestions in the [Issues](https://github.com/briatte/qogdata/issues) section. The package might be submitted to CRAN if I manage to add as many functionalities as I would like to.

The rest of this document describes currently available functions.

## `qogdata`

`qogdata` simply points to a [QOG server](http://www.qogdata.pol.gu.se/) to download available versions of the QOG datasets. By default, it simply returns the path to the QOG Standard cross-section dataset:

    > qogdata()
    [1] "http://www.qogdata.pol.gu.se/data/QoG_std_cs_15May13.csv"

Set `file` to `TRUE` or to a specific filename to download the dataset:

    > QOG = qogdata(file = TRUE, format = "ts", codebook = TRUE)
    Downloading http://www.qogdata.pol.gu.se/data/QoG_std_ts_15May13.csv...
    Loaded qog_std_ts_15May13.csv (N = 14137, 1946-2012, T = 67).

Set `codebook` to `TRUE` or to a specific filename to also download the codebook with the `qogbook` function:

    > QOG = qogdata(file = TRUE, format = "ts", codebook = TRUE)
    Downloading http://www.qogdata.pol.gu.se/data/QoG_std_ts_15May13.csv...
    Loaded qog_std_ts_15May13.csv (N = 14137, 1946-2012, T = 67).
    Downloading codebook to Codebook_QoG_Std15May13.pdf...
    Codebook: Codebook_QoG_Std15May13.pdf

The examples use `tempfile()` to save temporary copies of a QOG dataset before plotting:

    qplot(data = qogdata(tempfile(), version = "bas"), 
          x = bl_asyt25, y = wdi_fr, size = mad_pop / 10^3) + 
      scale_size_area("Population (mill.)", max_size = 12) +
      labs(y = "Fertility rate", x = "Average years of schooling, pop. aged 25+") +
      theme_grey(16)

![](https://github.com/briatte/qogdata/raw/master/example1.png)

The QOG Standard dataset is [currently](http://www.qogdata.pol.gu.se/data/) available in CSV, SPSS and Stata formats, and other versions of the dataset are available only in Stata format. `qogdata` will call `foreign` to import the Stata format and `Hmisc` to import the SPSS format. The [codebooks](http://www.qogdata.pol.gu.se/codebook/) are in PDF format and are downloaded by the `qogbook` function.

## `qogfind`

`qogfind` uses the two indexes bundled with the package to make finding variables a bit quicker for the end-user:

    > qogfind("public|administration")
    QOG Standard results:
         variable                            label ts.min ts.max ts.N ts.T cs.N cs.min cs.max
    217   gir_acs Administration and Civil Service   2004   2011  251    8   91   2006   2011
    291 irai_epru    Equity of Public Resource Use   2005   2011  532    7   80   2006   2011
    301  irai_qpa Quality of Public Administration   2005   2011  532    7   80   2006   2011
    304  irai_tac Transparency, Accountability and   2005   2011  532    7   80   2006   2011
    460  wdi_puhe Public Health Expenditure (% of    1995   2010 2960   16  187   2009   2009
    710  wvs_f115 Justifiable: avoiding a fare on    1981   2008  157   28   NA     NA     NA
    716   wvs_pet           Public self-expression   1981   2008  161   28   NA     NA     NA

The function searches through variable names and labels, as the `lookfor` command would in Stata. The `ts` columns provides years of measurement for the time-series dataset, the `cs` columns for the cross-sectional dataset. The information matches the figures reported in the _QOG Standard Codebook_ and _QOG Social Policy Codebook_. It can be easily plotted:

    qplot(data = qogfind("ihme_|undp_"), 
          y = label, yend = label, x = ts.min, xend = ts.max, 
          geom = "segment", size = I(6), alpha = ts.T) +
        scale_alpha("Year range") +
        theme_minimal(16) +
        labs(y = NULL, x = NULL, title = "Data availability") + 
        theme(legend.position = "bottom")

![](https://github.com/briatte/qogdata/raw/master/example2.png)

## `qogmap`

`qogmap` calls the `countrycode`, `maps` and `ggplot2` packages to draw choropleth maps of QOG cross-sectional data:

    QOG = qogdata(tempfile(), warn.missing.labels = FALSE, convert.factors = TRUE,
                  version = "bas", variables = c("ccodealp", "undp_hdi", "ihme_nm"))
    qogmap(subset(QOG, ccodealp != "RUS"), "ihme_nm", continent = "Asia") +
      ggtitle("Neonatal Mortality Rate per 1,000 births (IHME, 2009))")

![](https://github.com/briatte/qogdata/raw/master/example3.png)

The function works with `ggplot2` to detect the scale of the map (continuous or discrete). The `quantize` option can also create quantiles of a variable on the fly:

    qogmap(QOG, "undp_hdi", quantize = 3, continents = c("Africa", "Asia")) +
      scale_fill_brewer("", palette = "RdYlBu", labels = c("Low", "Med", "High")) +
      ggtitle("Human Development Index (UNDP, 2009-2010)")

![](https://github.com/briatte/qogdata/raw/master/example4.png)

The function matches QOG countries to geographic information from the `world` map provided in the `maps` package. It also adds continents and regions with the `countrycode` package to allow plots of specific areas. The map projection currently suffers from a little bug as soon as you include Russia.

## `qogjoin`

`qogjoin` joins historical state information to recent state information in the QOG Standard time series dataset. This will make the dataset backward compatible with older versions of the data where this separation did not exist. Complex cases like Sudan or Yemen are left intact.

## `merge_wdi`

`merge_wdi` calls the [WDI](http://cran.r-project.org/web/packages/WDI/) package to merge QOG Standard time series data with the [World Development Indicators](http://data.worldbank.org/data-catalog/world-development-indicators) provided through the World Bank API. This function makes it easy to update a WDI variable to the latest measurements, and to compare measurement differences between the QOG and WDI series:

![](https://github.com/briatte/qogdata/raw/master/example5.png)

The additional information in this plot is obtained in a single call to `merge_wdi`:

    QOG = merge_wdi(QOG, x = "SH.XPD.PCAP.PP.KD", add = "income", out = "data")

## `merge_uds`

`merge_uds` merges QOG Standard time series data with the [Unified Democracy Scores](http://www.unified-democracy-scores.org/) (UDS) from Pemstein _et al._.

## `merge_state`

`merge_state` merges QOG Standard time series data with the [state independence][statei] data from Gleditsch and Ward and [state coups][statec] data from Powell and Thyne.

[statei]: http://privatewww.essex.ac.uk/~ksg/statelist.html
[statec]: http://www.uky.edu/~clthyn2/coup_data/home.htm

## `xtset`

`xtset` is a simple way to specify panel data properties into a data frame attribute.

## `xtmissing`

`xtmissing` visualizes nonmissing country-year observations as a ggplot2 time graph.

# TODO

* `xtset` data frame attribute to work with any panel dataset
  * `xtdes`, `xtsum`, `xttab`, `xtline` Stata equivalents
  * `xtsubset` to subset panel data while preserving full time series.
* `merge_eurostat` to merge QOG Standard time series data with Eurostat data.
* `merge_parlgov`

# CREDITS

`qogdata` takes inspiration from two [QOG packages for Stata users](http://www.qog.pol.gu.se/data/dataextras/forstatausers/), [`qog`](http://ideas.repec.org/c/boc/bocode/s457283.html) by Christoph Thewes and [`qogbook`](http://ideas.repec.org/c/boc/bocode/s457599.html) by Richard Svensson. Further credits due to the authors of the QOG datasets (see the package documentation for references).
