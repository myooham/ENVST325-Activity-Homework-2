# install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)


streamH = read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo = read.csv("/cloud/project/activtiy02/site_info.csv")

streamH$dateF = ymd_hm(streamH$datetime)
streamH$year = year(streamH$dateF)


peaceH = streamH %>% 
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date", ylab=" Stage Height (ft)")


floods = full_join(streamH, siteInfo, by="siteID")
View(floods)

height.ave = floods %>%
  group_by(names) %>%
  summarise(mean.height = mean(gheight.ft))

floods$doy = yday(floods$dateF)

height.day = floods %>%
  group_by(names,doy) %>%
  summarise(mean.height = mean(gheight.ft))

max.cat = floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n())

flood_cat = floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF)

