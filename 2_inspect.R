
sdr = read.table("data/hlth_cd_ysdr1.txt", sep = "\t", na = "", header = TRUE)
head(sdr)

# ddply(sdr, .(icd10, year), summarise, 
#       count = length(value),
#       missing = length(value[is.na(value)]),
#       percent = round(100 * length(value[is.na(value)]) / length(value)))

names(sdr)
sdr$country = nchar(as.character(sdr$geo)) < 3

qplot(data = subset(sdr, country & icd10 == "C" & age == "TOTAL" & sex != "T"),
      fill = geo, x = year, y = value,
      stat = "identity", geom = "bar") + facet_wrap(~ sex)


