require(neonUtilities)# make sure you have all "require" entered
require(ggplot2)
require(plotly)
require(scales)
require(gridExtra)
require(ggthemes)
require(lubridate)
require(dplyr)
require(tidyquant)
#Merging sensors with 30min Data was too much so data was reduced by one month
#SwcwsZZ <- loadByProduct(dpID="DP1.00094.001", site="JORN",
#                         startdate="2017-07", enddate="2021-08",
#                         package="basic", avg=30, check.size=T)
y

SwcwsZZ <- loadByProduct(dpID="DP1.00094.001", site="JORN",
                         startdate="2017-08", enddate="2021-08",
                         package="basic", avg=30, check.size=T)
y

SwcwsZZ # Soil water and Soil Ion Content from 2017 - 2020

Swcws30min <- SwcwsZZ$SWS_30_minute

# extract sensor position
sens.pos <- SwcwsZZ$sensor_positions_00094
# create HOR.VER as a common variable with the sensor positions
Swcws30min <- Swcws30min %>%
  mutate(HOR.VER = paste(horizontalPosition,verticalPosition,sep="."))
# merge the data with sensor positions
Swcws30min <- merge(Swcws30min, sens.pos, by="HOR.VER", all.x=TRUE)


setwd("C:/Users/cater/OneDrive - University of Texas at El Paso/NEON_JORN.DP1_Project/Data")

write.table(Swcws30min, "NEON_JORN_SoilMoisturedata_082017_082021.csv",
            sep=",", dec=".", row.names=FALSE)
Swcws30min <- read.csv("NEON_JORN_SoilMoisturedata_082017_082021.csv", header= TRUE)
####Importing Corrected Depths sent Ed Ayers from NEON
depthcorrect <- read.csv("SWC_depths_JORN.csv", header= TRUE)
#### Merge swswcs data with corrected data 
depthcorrect <- depthcorrect %>% 
  select(horizontalPosition, verticalPosition, sensorDepths)
Swcws30min <- merge(Swcws30min, depthcorrect, by=c("horizontalPosition", "verticalPosition"))
Swcws30min <- Swcws30min %>% mutate(sensorDepths = round(sensorDepths, digits=2))



Swcws30min$startDateTime <- ymd_hms(Swcws30min$startDateTime)
Swcws30min$endDateTime <- ymd_hms(Swcws30min$endDateTime)
Swcws30min$julian <- yday(Swcws30min$startDateTime)
Swcws30min$year <- year(Swcws30min$startDateTime)
Swcws30min$month <-month(Swcws30min$startDateTime)



ggplot(data=Swcws30min, # Soil water content 2017- 2020
       aes(startDateTime, VSWCMean, colour= factor(zOffset))) +
  geom_line(na.rm=TRUE) +
  xlab("Date") + ylab("cubicCentimetersPerCubicCentimeter") +
  ggtitle("30 min Arithmetic mean of volumetric soil water content - NEON Jornada LTER\nJuly 2017 - December 2020")+
  facet_grid(horizontalPosition~year, scales = "free_x") +
  theme(text = element_text(size=7))
