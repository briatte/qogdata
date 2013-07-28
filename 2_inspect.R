
require(ggplot2)

sdr = read.table("data/hlth_cd_ysdr1.txt", sep = "\t", na = "", header = TRUE)
head(sdr)

# ddply(sdr, .(icd10, year), summarise, 
#       count = length(value),
#       missing = length(value[is.na(value)]),
#       percent = round(100 * length(value[is.na(value)]) / length(value)))

names(sdr)
sdr$country = nchar(as.character(sdr$geo)) < 3

ksdr = subset(sdr, country & icd10 == "C" & age == "TOTAL" & sex != "T")

qplot(data = ksdr,
      group = geo, colour = geo, x = year, y = value, label = geo,
      geom = c("text", "line")) + facet_wrap(~ sex)

g = qplot(data = ksdr,
      group = geo, x = year, y = value, se = FALSE, colour = I("grey80"),
      geom = c("point", "smooth")) + facet_wrap(~ sex)
g + geom_smooth(data = subset(ksdr, geo %in% c("FR", "UK")), se = FALSE,
                aes(colour = geo))

qplot(data = subset(sdr, country & icd10 == "C" & age == "TOTAL" & sex != "T"),
      fill = geo, x = year, y = value,
      stat = "identity", geom = "bar") + facet_wrap(~ sex)


