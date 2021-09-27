require(neonUtilities) # make sure you have all "require" entered
require(ggplot2)
require(plotly)
require(scales)
require(gridExtra)
require(ggthemes)
require(lubridate)
require(dplyr)
require(tidyquant)

### Dont use these codes
Pri.cip <- loadByProduct(dpID="DP1.00006.001",           
                         site=c("JORN"),
                         startdate="2014-09", enddate="2021-08", 
                         check.size=F)
SECPRE30min <- Pri.cip$SECPRE_30min


#save Pricip data to computer setwd and write.table will need to be tailored to specific working directory on PC. 
setwd("C:/Users/cater/OneDrive - University of Texas at El Paso/NEON_JORN.DP1_Project/Data")
write.table(SECPRE30min, "NEON_JORN_Pricipdata_022017_082021.csv",
            sep=",", dec=".", row.names=FALSE)

### Code starts here if code needs to be changed manipulate top codes.
SECPRE30min <- read.csv("NEON_JORN_Pricipdata_022017_082021.csv", header= TRUE)

Phenodays <- read.csv("NEON.D14.JORN.DP1.00033_GR_1000_1dayz.csv", header= TRUE)

#calculate total percip by year & day to plot precip daily 
total.prec2 <- SECPRE30min %>%
  group_by(year, month, julian) %>%
  dplyr::summarize(sum_prec = sum(secPrecipBulk, na.rm = TRUE), startDateTime = first(startDateTime))



ggplot(data=SECPRE30min,
       aes(startDateTime, secPrecipBulk))+
  geom_line(na.rm=TRUE) +
  facet_grid(.~year(startDateTime), scales = "free_x")  +
  xlab("Date (Daily Values)") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk Secondary Precipitation - NEON Jornada LTER\nSeptember 2014 - December 2020") +
  theme(text = element_text(size=6))

### Precipitation and GCC

Limbo <- total.prec2%>%
  ggplot(data=.,
         aes(startDateTime, sum_prec)) + 
  geom_line(na.rm=TRUE) +
  coord_x_datetime(xlim = c("2017-02-24 00:00:00", "2021-08-31 23:30:00")) +
  xlab("Date (Daily Values)") + ylab("Precipitation (millimeters)") +
  ggtitle("Total Daily Bulk Secondary Precipitation - NEON Jornada LTER\nFeburary 24th, 2017 - August 31 2021") +
  theme(text = element_text(size=6))
Limbo  

Phenodays$date <- as.Date(parse_date_time(Phenodays$date, c('mdy')))
Pheno <- ggplot(data=Phenodays,
         aes(date, midday_gcc)) + 
  geom_line(na.rm=TRUE) +
  coord_x_date(xlim = c("2017-02-24", "2021-08-31")) +
  xlab("Date (Daily Values)") + ylab("GCC") +
  ggtitle("Daily Midday GCC - NEON Jornada LTER\nFeburary 24th, 2017 - August 31 2021") +
  theme(text = element_text(size=6))
Pheno


plot_grid(Limbo, Pheno, nrow =  2)


###Full 30 min Mean Pricip graphs
precPlot_30minYY <- ggplot(data=SECPRE30min,
                           aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm = TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 min Arithmetic mean of volumetric soil water content - NEON Jornada LTER\nSeptember 2014 - December 2020")
precPlot_30minYY 
# Full individual 30 mean year graphs from 2014 - 2021 
precPlotz_30minzoom2014 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\nSeptember 11 ,2014 to the end 2014") +
  coord_x_datetime(xlim = c("2014-09-11 00:00:00", "2014-12-31 23:30:00"))
precPlotz_30minzoom2014 # 2014
precPlotz_30minzoom2015 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\n2015") +
  coord_x_datetime(xlim = c("2015-01-01 00:00:00", "2015-12-31 23:30:00"))
precPlotz_30minzoom2015 # these next graphs are zooming on each year I tried making these graphs first thought subsetting and using scale_x_datetime but the didnt work so I zoomed in instead.
precPlotz_30minzoom2016 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\n2016") +
  coord_x_datetime(xlim = c("2016-01-01 00:00:00", "2016-12-31 23:30:00"))
precPlotz_30minzoom2016 # 2016
precPlotz_30minzoom2017 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\n2017") +
  coord_x_datetime(xlim = c("2017-01-01 00:00:00", "2017-12-31 23:30:00"))
precPlotz_30minzoom2017 # 2017
precPlotz_30minzoom2018 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\n2018") +
  coord_x_datetime(xlim = c("2018-01-01 00:00:00", "2018-12-31 23:30:00"))
precPlotz_30minzoom2018 # 2018
precPlotz_30minzoom2019 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\n2019") +
  coord_x_datetime(xlim = c("2019-01-01 00:00:00", "2019-12-31 23:30:00"))
precPlotz_30minzoom2019 # 2019
precPlotz_30minzoom2020 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\nBeginning of 2020 to December 31 2020") +
  coord_x_datetime(xlim = c("2020-01-01 00:00:00", "2020-12-31 23:30:00"))
precPlotz_30minzoom2020 #2020
precPlotz_30minzoom2021 <- ggplot(data=SECPRE30min,
                                  aes(startDateTime, secPrecipBulk)) + 
  geom_bar(stat="identity", na.rm=TRUE, colour="dark blue") + 
  xlab("Date") + ylab("Precipitation (millimeters)") +
  ggtitle("30 Min Bulk secondary precipitation - NEON Jornada LTER\nBeginning of 2020 to December 31 2020") +
  coord_x_datetime(xlim = c("2021-01-01 00:00:00", "2021-08-31 23:30:00"))
precPlotz_30minzoom2021 #2021