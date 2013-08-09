The __qogdata__ package is a collection of functions to manipulate [Quality of Government][qog] (QOG) data and related material. It lets the user access QOG datasets and codebooks and contains helpers to search and prepare QOG variables. The package also contains methods to merge and plot time series data, and provides a few examples of how these methods can be deployed to import World Bank and country-level indicators into QOG datasets.

[qog]: http://www.qog.pol.gu.se/

# INSTALL

Version 0.1 of the `qogdata` package is installable with the `devtools` package:

    library(devtools)
    install_github("qogdata", "briatte", dependencies = TRUE)
    require(qogdata)

Installing dependencies is particularly recommended to work with country-level data. If loading fails after running `devtools`, install dependencies manually:

    deps = c("countrycode", "foreign", "ggplot2", "maps", "WDI")
	lapply(deps, install.packages)

# HOWTO

This document describes all currently available functions, which are also internally documented. Additional notes are provided in the [repository wiki][wiki]. Please post comments, issues and suggestions in the [Issues](https://github.com/briatte/qogdata/issues) section. Feel free to [email me](mailto:f.briatte@ed.ac.uk) if you have any additional queries or ideas on how the `qogdata` package could be developed.

[wiki]: https://github.com/briatte/qogdata/wiki

## `qogdata`

1. Install and load the `xtdata` package.
2. Type `qogdata(TRUE)` to download the QOG Standard dataset.
3. Type `qogbook(TRUE)` to download the QOG Standard codebook.

__Details:__

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

__Notes:__

* The QOG Standard dataset is [currently](http://www.qogdata.pol.gu.se/data/) available in CSV, SPSS and Stata formats, and other QOG datasets are available only in Stata format. `qogdata` will call `foreign` to import SPSS and Stata files, which contain variable and value labels.
* The `qogdata` package does not currently deal with the [QOG EU Regions datasets][qog-reg], which comes in Excel format and has not been updated throughout the years. However, the `xtdata` specification (explained below) is flexible enough to accommodate for regional data, so that [might happen][wiki] in the future.
* The [codebooks](http://www.qogdata.pol.gu.se/codebook/) are in PDF format. It should be easy to build an additional `qoglabel` function to call value labels extracted from Stata and stored as index files, as shown for variable labels and years of measurement in `qogfind` (below).

[qog-reg]: http://www.qog.pol.gu.se/data/datadownloads/qogeuregionaldata/

## `qogfind`

`qogfind` uses two indexes bundled with the package to help the user find variables in QOG datasets:

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

    qplot(data = na.omit(qogfind("ims_", version = "soc")), 
          y = label, yend = label, x = as.numeric(ts.min), xend = as.numeric(ts.max), 
          geom = "segment", size = I(6), alpha = ts.T) +
      scale_alpha("Year range") +
      theme_minimal(16) +
      labs(y = NULL, x = NULL, title = "Data availability") + 
      theme(legend.position = "bottom")

![](https://github.com/briatte/qogdata/raw/master/example1.png)

## `qogjoin`

`qogjoin` is a highly idiosyncratic method to join historical to recent states in the QOG Standard time series dataset. Only states that went through a single territorial departure (e.g. France/Algeria or Pakistan/Bangladesh) are joined: more complex state partition cases like North/South Sudan and North/South Yemen are left aside.

In the current Standard version of the dataset (15 May 2013), this will cause a merge between the split versions of Ethiopia, France, Malaysia and Pakistan. This will make the dataset more backward compatible with older versions of the data where this separation did not exist.

## `xtdata`, `xtset`, `xt`

`xtdata` and its related functions are a way to specify panel data properties into the `xtdata` attribute of a data frame attribute, which makes it possible to:

* safely merge several panel datasets with `xtmerge`, a merge function that first checks whether the unit type and time period formats match, and looks for matches if they do not.
* pass some default information to plot methods for panel data, which is minimally illustrated in `xtmap` and forthcoming as a larger series of `xtplot` methods for `ggplot2`.

Please consult the [repository wiki][wiki] for details on `xt` methods in the `qogdata` package, and how it might turn into a proper class by version 1.0. The method is intended for use with panel data: users who want to work with full-fledged time series should use the `zoo` or `xts` packages.

## `xtshift`, `xtlag`, `xtlead`, `xtdecay`, `xttse`

`xtshift`, `xtlag` and `xtlead` are [functions to shift (lag or lead) a panel variable](http://christophergandrud.blogspot.fr/2013/05/slide-one-function-for-laglead.html). `xtdecay` and `xttse` (time since event) are additional [time series functions for panel data](http://www.zmjones.com/panel-ts.html). These functions, as well as several other little utilities used in the `qogdata` package, are based on code found online (see the package documentation for the sources).

## `xtmerge`, `xtsubset`

`xtmerge` performs a merge of two panel datasets based on their `xtdata` attributes, checking for identically formatted data identifiers and time periods before performing the merge, which otherwise works like [`merge`][merge] in base R.

[merge]: http://www.rdocumentation.org/packages/base/functions/merge

When `xtmerge` is provided datasets of type `country` with different country code formats, it runs the `xtcountry` helper function to determine the best ISO-3N conversion match and performs the merge on the new `iso3n` variable.

`xtsubset` is a wrapper for `subset` that preserves the `xtdata` attribute of a data frame.

## `xtmap`, `xtplot`

`xtmap` calls the `countrycode`, `maps` and `ggplot2` packages to draw choropleth maps, with helper transformations for time series. The examples provided below use QOG cross-sectional data:

    QOG = qogdata(tempfile(), version = "bas", variables = c("ccodealp", "undp_hdi", "ihme_nm"))

The result is a `ggplot2` object with the [map and data plotted together][map_data]:

[map_data]: http://www.rdocumentation.org/packages/ggplot2/functions/map_data

    xtmap(subset(QOG, ccodealp != "RUS"), "ihme_nm", continent = "Asia", iso3n = "ccode") +
      ggtitle("Neonatal Mortality Rate per 1,000 births (IHME, 2009))")

![](https://github.com/briatte/qogdata/raw/master/example2.png)

Use the `quantize` option to map quantiles of a continuous variable on the fly:

    xtmap(QOG, "undp_hdi", quantize = 3, continents = c("Africa", "Asia"), iso3n = "ccode") +
      scale_fill_brewer("", palette = "RdYlBu", labels = c("Low", "Med", "High")) +
      ggtitle("Human Development Index (UNDP, 2009-2010)")

![](https://github.com/briatte/qogdata/raw/master/example3.png)

The function matches countries to geographic information from the `world` map provided in the `maps` package, and currently suffers from a little [map projection bug][bug] as soon as you include Russia. It also adds continents and regions with the `countrycode` package to allow plots of specific geographic areas.

[bug]: https://github.com/briatte/qogdata/issues/1

When provided with a data frame carrying the `xtdata` attribute, the function currently uses the maximum time period as `t`, as with 'most recent year' with `country`-level data. See the [wiki][wiki] for planned improvements.

`xtplot` is a stub for a similar function that will plot time series out of data frames carrying an `xtdata` attribute.

## `xtmissing`

`xtmissing` plots a basic tile plot of missing and nonmissing values in a data frame with an `xtdata` attribute, with observations in rows and ordered time periods in columns.

## `get_eurostat`

`get_eurostat` is a soon-to-be-added downloader for Eurostat data. See the [repository wiki][wiki] for development notes, or see the [`eurostat_r`][eurostat_r] script to bulk import Eurostat data in R.

[eurostat_r]: https://github.com/toprach/eurostat_r

## `get_wdi`

`get_wdi` calls the [WDI](http://cran.r-project.org/web/packages/WDI/) package to download one or more [World Development Indicators](http://data.worldbank.org/data-catalog/world-development-indicators) provided through the World Bank API. The result is a data frame with an `xdtata` attribute ready for merging onto ISO-3N country codes, converted from the ISO-3C country codes returned by `WDI`.

This function makes it easy to update a WDI within a QOG dataset to the latest measurements, and to compare measurement differences between the latest QOG and WDI series:

![](https://github.com/briatte/qogdata/raw/master/example4.png)

The WDI data for this plot was retrieved with `get_wdi` as follows:

    WDI = get_wdi(x = "SH.XPD.PCAP.PP.KD", add = "income")

The code for the comparative measurements plot is in the documentation of the `get_wdi` function.

## `get_uds`

`get_uds` downloads the [Unified Democracy Scores](http://www.unified-democracy-scores.org/) (UDS) from Pemstein _et al._. The result is a data frame with an `xdtata` attribute ready for merging onto Correlates of War (COW) numeric country codes.

## `get_gwpt`

`get_gwpt` downloads [state independence][statei] data from Gleditsch & Ward and  [state coups][statec] data from Powell & Thyne. The result is a data frame with an `xdtata` attribute that is idiosyncratic (Gleditsch and Ward country codes), but might still be merged with `xtcountry` and `xtmerge` (see above).

[statei]: http://privatewww.essex.ac.uk/~ksg/statelist.html
[statec]: http://www.uky.edu/~clthyn2/coup_data/home.htm

# CREDITS

`qogdata` takes inspiration from two [QOG packages for Stata users](http://www.qog.pol.gu.se/data/dataextras/forstatausers/), [`qog`](http://ideas.repec.org/c/boc/bocode/s457283.html) by Christoph Thewes and [`qogbook`](http://ideas.repec.org/c/boc/bocode/s457599.html) by Richard Svensson. Further credits are due to the [authors of the QOG datasets][qog-team], and a list of related R packages appears on the [wiki][wiki].

[qog-team]: http://www.qog.pol.gu.se/aboutus/Organization/
