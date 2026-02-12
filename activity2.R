# install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)


streamH = read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo = read.csv("/cloud/project/activtiy02/site_info.csv")

streamH$dateF = ymd_hm(streamH$datetime)
streamH$year = year(streamH$dateF)


peaceH = streamH %>% 
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, pch=1, xlab="Date", ylab=" Stage Height (ft)", main = "Peace River")


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
  summarise(min_date = min(dateF))

# Homework
#Question 1. 
plot(peaceH$dateF, peaceH$gheight.ft, pch=1, xlab="Date", ylab=" Stage Height (ft)", main = "Peace River")
fisheatingH = streamH %>% 
  filter(siteID == 2256500)
plot(fisheatingH$dateF, fisheatingH$gheight.ft, pch=1, xlab="Date", ylab=" Stage Height (ft)", main = "Fisheating Creek")
withlacoocheeH = streamH %>% 
  filter(siteID == 2312000)
plot(withlacoocheeH$dateF, withlacoocheeH$gheight.ft, pch=1, xlab="Date", ylab=" Stage Height (ft)", main = "Withlacoochee River")
santafeH = streamH %>% 
  filter(siteID == 2322500)
plot(santafeH$dateF, santafeH$gheight.ft, pch=1, xlab="Date", ylab=" Stage Height (ft)", main = "Santa Fe River")


#Quetion 2. 
flood_action = floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
flood_moderate = floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
flood_major = floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
#Question 3.
floods$flood_ex = floods$gheight.ft - floods$major.ft 
highest_flood = floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(floodabove =max(flood_ex))
  

#Question 4.
? select()
# used select
floods %>%
  select(names:major.ft)
? ifelse()
#used ifelse to
sum(ifelse(floods$gheight.ft > floods$major.ft, 1, 0))

? hist()
hist(floods$gheight, xlab="Height of Floods (ft)", main = "Histogram of height of floods")
