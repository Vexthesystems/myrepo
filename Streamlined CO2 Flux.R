#Updated 
zipsByProduct(dpID="DP4.00200.001", package="basic", 
              site=c("JORN"), 
              startdate="2017-02", enddate="2021-07",
              savepath="C:/Users/cater/OneDrive - University of Texas at El Paso/NEON_JORN.DP1_Project/Data", 
              check.size=F)
flux <- stackEddy(filepath="C:/Users/cater/OneDrive - University of Texas at El Paso/NEON_JORN.DP1_Project/Data/filesToStack00200",
                  level="dp04")

term <- unlist(strsplit(names(flux$JORN), split=".", fixed=T))
flux$objDesc[which(flux$objDesc$Object %in% term),]
flux$variables
flux.data <- flux$JORN


#save flux.data to computer setwd and write.table will need to be tailored to specific working directory on PC. 
setwd("C:/Users/cater/OneDrive - University of Texas at El Paso/NEON_JORN.DP1_Project/Data")
write.table(flux.data, "UNEON_JORN_ECFluxdata_032017_072021.csv",
            sep=",", dec=".", row.names=FALSE)
flux.data <- read.csv("UNEON_JORN_ECFluxdata_032017_072021.csv", header= TRUE)
timeB <- as.POSIXct(flux$JORN$timeBgn,
                    format="%Y-%m-%dT%H:%M:%S", 
                    tz="GMT")
flux.data <- cbind(timeB, flux.data)
plot(flux$JORN$data.fluxCo2.nsae.flux~timeB, 
     pch=".", ylim=c(-20, 20),
     xlab="Date", ylab="CO2 flux",
     xaxt="n")
plot(flux.data$data.fluxCo2.nsae.flux~flux.data$timeB,
     pch=".", ylim=c(-20, 20),
     xlab="Date", ylab="CO2 flux",
     xaxt="n",
     axis.POSIXct(1, x=timeB, format="%Y-%m-%d"))
#use qfqm column to remove qfqm=1
#Visualize flux data with qfqm flag
ggplot(flux.data, aes(timeB, data.fluxCo2.nsae.flux, colour=factor(qfqm.fluxCo2.nsae.qfFinl)))+
  geom_point()


###The issue where we needed to filter everything before everything before 10-29-2019 has been resolved according to NEONs issue so this 
###code is likely not needed but keeping it for records just in case 
flux.data.qf <- flux.data%>%
  filter(qfqm.fluxCo2.nsae.qfFinl==0&
           timeB<as.POSIXct("2019-10-29", tz="GMT"))
ggplot(flux.data.qf, aes(timeB, data.fluxCo2.nsae.flux, colour=factor(qfqm.fluxCo2.nsae.qfFinl)))+
  geom_point() +
  facet_grid(.~year(timeB),scales = "free_x")
ggplot(flux.data, aes(timeB, data.fluxCo2.nsae.flux, colour=factor(qfqm.fluxCo2.nsae.qfFinl==0)))+
  geom_line()+
  facet_grid(.~year(timeB), scales = "free_x") +
  xlab("Date") + ylab("umolCo2 m-2 s-1") +
  ggtitle("CO2 Flux - NEON Jornada LTER")
facetfilteredCO21 <- ggplot(flux.data.qf, aes(yday(timeB), data.fluxCo2.nsae.flux))+
  geom_point(size = 0.005)+
  facet_grid(.~year(timeB)) +
  scale_x_continuous(breaks=c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"),
                     expand=c(0,0))+
  labs(y=expression("Half-hourly NEE (μmol C" *O[2]*" "*m^-2* "se" *c^-1*")"),
       x = "Month")+
  ggtitle("30 Min CO2 Flux - NEON Jornada LTER") +
  theme(text = element_text(size=6))
facetfilteredCO21 ### End of 10-29-21 filtered Data


#Streamlined CO2 Flux Data Filtered and Unfiltered 
CO2fluxUnfiliterted <- flux.data%>%
  #filter(qfqm.fluxCo2.nsae.qfFinl==0)%>%
  ggplot(data=.,
         aes(timeB, data.fluxCo2.nsae.flux))+
  geom_line() +
  xlab("Date") + ylab("umol Co2 m-2 s-1") +
  ylim(c(-20, 20))+
  facet_grid(scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size=6))
CO2fluxUnfiliterted


CO2fluxfiliterted <- flux.data%>%
  filter(qfqm.fluxCo2.nsae.qfFinl==0)%>%
  ggplot(data=.,
         aes(timeB, data.fluxCo2.nsae.flux))+
  geom_line() +
  xlab("Date") + ylab("umol Co2 m-2 s-1") +
  ylim(c(-20, 20))+
  facet_grid(scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size=6))
CO2fluxfiliterted
