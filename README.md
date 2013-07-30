Inspired by @ajdamico's [usgsd](https://github.com/ajdamico/usgsd/), here's a collection of scripts to manipulate [Quality of Government](http://www.qog.pol.gu.se/) data. __Just started working on it__. I hope to turn it into a [helper package](http://www.qog.pol.gu.se/data/dataextras/forstatausers/) for QOG users.

# OUTLINE

* `qoguse`
* `qogmerge`
* `qogmap`
* `read.eurostat` and `merge.eurostat`: add Eurostat variables based on the codes shown in their [table of contents](http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=table_of_contents_en.pdf)

Other Eurostat scripts:

* [datamarket/rdatamarket](https://github.com/DataMarket/rdatamarket) (R, many more sources)
* [toprach/eurostat_r](https://github.com/toprach/eurostat_r/blob/master/eurostat_r.r) (R, data table function)
* [gka/eurostat](https://github.com/gka/eurostat) (Python, bulk download scraper)

# EXAMPLES

I'm working on the Eurostat merge function. Missing values are properly encoded as `NA`. Some values have [flags](http://epp.eurostat.ec.europa.eu/NavTree_prod/htdocs/explanation/explanation_en_auth.html) of the following form:

    b = break in time series 
    c = confidential 
    d = definition differs, see metadata 
    e = estimated 
    f = forecast 
    i = see metadata (phased out) 
    n = not significant 
    p = provisional 
    r = revised (phased out) 
    s = Eurostat estimate (phased out) 
    u = low reliability 
    z = not applicable 

Here's what the index looks like for causes of death (the first column are titles):

    > index[, -1]
               code    type    updated    changed     start       end
    1  hlth_cd_ynrt dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    2  hlth_cd_ynrm dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    3  hlth_cd_ynrf dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    4 hlth_cd_ycdrt dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    5 hlth_cd_ycdrm dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    6 hlth_cd_ycdrf dataset 07.06.2013 26.06.2013 1994_1996 2008_2010
    7 hlth_cd_ysdr1 dataset 07.06.2013 26.06.2013 1994_1996 2008_2010

The data are coded by [NUTS-2](http://epp.eurostat.ec.europa.eu/portal/page/portal/nuts_nomenclature/introduction) regions, [age groups](http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2Fen%2Fage.dic) and [ICD-10 codes](http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2Fen%2Ficd10.dic), like these:

    A-R_V-Y   All causes of death (A00-Y89) excluding S00-T98
    A-T_Z     All causes of diseases (A00-Z99) excluding V00-Y98
    A-T_Z_XNB All causes of diseases (A00-Z99) excluding V00-Y98 and Z38
    A_B       Certain infectious and parasitic diseases (A00-B99)
    C         Malignant neoplasms (C00-C97)
    D00-D48   Non-malignant neoplasms (benign and uncertain)
    F         Mental and behavioural disorders (F00-F99)
    I         Diseases of the circulatory system (I00-I99)
    J         Diseases of the respiratory system (J00-J99)
    V01-Y89   External causes of morbidity and mortality (V01-Y89)
