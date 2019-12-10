#Data Setup for WA_PM_Heat Episodes
#Elena Austin and Eddie Kasner
#December 6th, 2019
#

######################################
#Plot Styles

#### DEOHS Theme ####
# Source: https://github.com/koundy/ggplot_theme_Publication/blob/master/R/ggplot_theme_Publication.R

library(pacman)
p_load(extrafont)
p_load(grid)
p_load(ggthemes)
p_load(scales)
p_load(data.table)
p_load(ggplot2)
p_load(plyr)
p_load(readr)
p_load(data.table)
p_load(lubridate)
p_load(ggplot2)  
p_load(scales)   
p_load(gridExtra) 
p_load(grid) 
p_load(dplyr)
p_load(xts)
p_load(sqldf)
p_load(rgeos)
p_load(geosphere)
p_load(tidyr)
p_load(tidyqwi)
p_load(httr)
p_load(sp)

#run on first instance
#font_import()
#loadfonts(device = "win")

#on windows 
#Download Encode Sans Family from here: https://www.washington.edu/brand/graphic-elements/font-download/
#Install by dragging unzipped font files into the control panel font library (windows +R > %windir%\fonts)
#8 inches wide, 5.25 inches high for each figure
theme_DEOHS <- function(base_size=14, base_family="Encode Sans Normal") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(t=0, unit = "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

##########FUNCTIONS ######################
# Write and use GM, GSD, AM, ASD functions

## No error handling

gm<-function(x){
  rslt<-exp(mean(log(x))) 
  return(rslt)
}

gsd<-function(x){
  rslt<-exp(sqrt(var(log(x))))
  return(rslt)
}

## Error handling

gm_eh<-function(x){
  if(sum(is.na(x))>0)
  {return(NA)}
  if(sum(!is.finite(log(x)))==0)
  {rslt<-exp(mean(log(x)))} # problem if x=0 -> log
  else{rslt<-Inf}
  return(rslt)
}

gsd_eh<-function(x){
  if(sum(is.na(x))>0)
  {return(NA)}
  if(sum(!is.finite(log(x)))==0)
  {rslt<-exp(sqrt(var(log(x))))} # problem if x=0 -> log
  else{rslt<-Inf}
  return(rslt)
}

n.am.asd.gm.gsd<-function(dta){
  x<-subset(dta, !is.na(dta))
  return(c( N=length(x),AM=mean(x),ASD=sd(x),GM=gm_eh(x),GSD=gsd_eh(x), Min=min(x), Max=max(x) ))
}

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}


##############
#Data Setup##########################
#
##query qwi employment data#######################
# apikey = as.character(fread("API key/QWI key", header = F))
# qwi = get_qwi(years = as.character(2010:2019), 
#               quarters = c(1, 2, 3, 4), 
#               industry_level = "4", 
#               states = "53",
#               variables = c("sEmp", "Emp"),
#               endpoint = "rh", 
#               geography = "county", 
#               seasonadj = "U", 
#               apikey = apikey,
#               processing = "sequential")
#saveRDS(qwi, "Data/qwi_2010_2019_RH.RDS")
#
# qwi = readRDS("Data/qwi_2010_2019_RH.RDS")
# qwi_ag = setDT(qwi)
# 
# qwi_ag = qwi_ag[, NAICS2 := substr(qwi$industry,1,2) ]
# qwi_ag  = qwi_ag[NAICS2 %in% "11"]
# qwi_ag = add_qwi_labels(qwi_ag)
# 
# save(qwi_ag, file = "Data/qwi_ag_2010_2019_RH.RData")
#
#load("Data/qwi_ag_2010_2019_RH.RData")
# 
# qwi_ag= setDT(qwi_ag)
# 
# qwi_ag_all = qwi_ag[ , lapply(.SD, FUN = function(x)
#   sum(as.numeric(as.character(x)), na.rm = T)), by = c("county", 
#                                                        "year",
#                                                        "year_time",
#                                                        "race",
#                                                        "ethnicity",
#                                                        "ownercode",
#                                                        "seasonadj",
#                                                        "quarter", 
#                                                        "NAICS2")]
# qwi_ag_all$industry = NULL
# 
# qwi_ag = qwi_ag[ , lapply(.SD, FUN = function(x)
#   sum(as.numeric(as.character(x)), na.rm = T)), by = c("county", 
#                                                        "year",
#                                                        "year_time",
#                                                        "industry",
#                                                        "race",
#                                                        "ethnicity",
#                                                        "ownercode",
#                                                        "seasonadj",
#                                                        "quarter", 
#                                                        "NAICS2")]
#save(qwi_ag, file = "Data/qwi_2010_2019_ag.RDS")
#save(qwi_ag_all, file = "Data/qwi_2010_2019_ag_2Digit.RDS")
#
load("Data/qwi_2010_2019_ag_2Digit.RDS")
load("Data/qwi_2010_2019_ag.RDS")

###################
###################
#Ag Weather Net Data #############################
#awn <- list.files("Data/awn", pattern=".csv", full.names = TRUE) %>%  lapply(read_csv) %>% bind_rows 
# 
# awn.temp.rh.na  <- subset(awn, select = c(TSTAMP_PST, UNIT_ID, STATION_NAME, LATITUDE, LONGITUDE, AIR_TEMP_F, `RELATIVE_HUMIDITY_%`))
# awn.temp.rh     <- subset(awn.temp.rh.na, AIR_TEMP_F < 500 & `RELATIVE_HUMIDITY_%` < 500)
# 
# awn.1h.temp     <- aggregate(awn.temp.rh["AIR_TEMP_F"], list(awn.temp.rh$UNIT_ID, awn.temp.rh$LATITUDE, awn.temp.rh$LONGITUDE, hour=cut(as.POSIXct(awn.temp.rh$TSTAMP_PST)-1, "hour")), mean)
# awn.1h.rh       <- aggregate(awn.temp.rh["RELATIVE_HUMIDITY_%"], list(awn.temp.rh$UNIT_ID, hour=cut(as.POSIXct(awn.temp.rh$TSTAMP_PST)-1, "hour")), mean)
# 
# awn.1h          <- merge(awn.1h.temp, awn.1h.rh, by = c("Group.1","hour"))
# awn.1h$datetime_PST <- ymd_hms(awn.1h$hour, tz="America/Los_Angeles")
# awn.1h$year     <- year(awn.1h$datetime_PST)
# 
# awn.1h.summary  <- ddply(awn.1h, .(Group.1,year), summarize,  TEMP_F=mean(AIR_TEMP_F), RH=mean(`RELATIVE_HUMIDITY_%`), N=length(year))
# 
# setDT(awn.1h)
# 
# awn.1h[, hi := heat_index(AIR_TEMP_F, `RELATIVE_HUMIDITY_%`)]
# 
# awn.1h <- awn.1h %>% rename(Site.Num.awn=Group.1, Latitude.awn=Group.2, Longitude.awn=Group.3)
# 
# rm(awn, awn.1h.rh, awn.1h.temp, awn.temp.rh, awn.temp.rh.na)
# 
# setDF(awn.1h); unique(awn.1h[c("Latitude.awn", "Longitude.awn")])
# 
# save(awn.1h, file =         "Data/awn.1h.2010.2018.RData")
# save(awn.1h.summary, file = "Data/awn.1h.summary.2010.2018.RData")

###Download EPA FRM/FEM###############################
#https://aqs.epa.gov/aqsweb/airdata/download_files.html

# epa.2010 <- read.csv("Data/epa/hourly_88101_2010.csv"); epa.2010.wa <- subset(epa.2010, State.Code=='53')
# epa.2011 <- read.csv("Data/epa/hourly_88101_2011.csv"); epa.2011.wa <- subset(epa.2011, State.Code=='53')
# epa.2012 <- read.csv("Data/epa/hourly_88101_2012.csv"); epa.2012.wa <- subset(epa.2012, State.Code=='53')
# epa.2013 <- read.csv("Data/epa/hourly_88101_2013.csv"); epa.2013.wa <- subset(epa.2013, State.Code=='53')
# epa.2014 <- read.csv("Data/epa/hourly_88101_2014.csv"); epa.2014.wa <- subset(epa.2014, State.Code=='53')
# epa.2015 <- read.csv("Data/epa/hourly_88101_2015.csv"); epa.2015.wa <- subset(epa.2015, State.Code=='53')
# epa.2016 <- read.csv("Data/epa/hourly_88101_2016.csv"); epa.2016.wa <- subset(epa.2016, State.Code=='53')
# epa.2017 <- read.csv("Data/epa/hourly_88101_2017.csv"); epa.2017.wa <- subset(epa.2017, State.Code=='53')
# epa.2018 <- read.csv("Data/epa/hourly_88101_2018.csv"); epa.2018.wa <- subset(epa.2018, State.Code=='53')
# 
# epa <- bind_rows(epa.2010.wa, epa.2011.wa, epa.2012.wa, epa.2013.wa, epa.2014.wa, epa.2015.wa, epa.2016.wa, epa.2017.wa, epa.2018.wa)
# 
# epa$datetime_UTC = ymd_hm(paste(epa$Date.GMT, epa$Time.GMT))
# epa$datetime_PST = as_datetime(epa$datetime, tz = "America/Los_Angeles")
# 
# epa$year     <- year(epa$datetime_PST)
# 
# epa.1h <- subset(epa, select = c(State.Code, County.Code, County.Name, Site.Num, Latitude, Longitude, Datum, Parameter.Name, datetime_PST, year, Date.Local, Time.Local, Date.GMT, Time.GMT, Sample.Measurement, Units.of.Measure, State.Name, County.Name))
# 
# table(epa.1h$County.Name,epa.1h$Site.Num)
# 
# epa.1h <- epa.1h %>% unite("Site", County.Name:Site.Num, sep = "-", remove = FALSE)
# 
# epa.1h.summary  <- ddply(epa.1h, .(Site,year), summarize,  PM=mean(Sample.Measurement), N=length(year))
# 
# setDT(epa.1h)
# 
# rm(epa, epa.2010, epa.2011, epa.2012, epa.2013, epa.2014, epa.2015, epa.2016, epa.2017, epa.2018)
# rm(epa.2010.wa, epa.2011.wa, epa.2012.wa, epa.2013.wa, epa.2014.wa, epa.2015.wa, epa.2016.wa, epa.2017.wa, epa.2018.wa)
# #rm(epa.nearestAWN.merge, locationsAWN, locationsPM, nearestAWN, set1sp, set2sp)
# 
# setDF(epa.1h); unique(epa.1h[c("Latitude", "Longitude")]); unique(epa.1h$Site)
# 
# save(epa.1h, file =         "Data/epa.1h.2010.2018.RData")
# save(epa.1h.summary, file = "Data/epa.1h.summary.2010.2018.RData")
# 
#########Download EPA non FEM/FRM##########################
# epa.2010.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2010.csv"); epa.2010.non.wa <- subset(epa.2010.non, State.Code=='53')
# epa.2011.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2011.csv"); epa.2011.non.wa <- subset(epa.2011.non, State.Code=='53')
# epa.2012.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2012.csv"); epa.2012.non.wa <- subset(epa.2012.non, State.Code=='53')
# epa.2013.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2013.csv"); epa.2013.non.wa <- subset(epa.2013.non, State.Code=='53')
# epa.2014.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2014.csv"); epa.2014.non.wa <- subset(epa.2014.non, State.Code=='53')
# epa.2015.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2015.csv"); epa.2015.non.wa <- subset(epa.2015.non, State.Code=='53')
# epa.2016.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2016.csv"); epa.2016.non.wa <- subset(epa.2016.non, State.Code=='53')
# epa.2017.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2017.csv"); epa.2017.non.wa <- subset(epa.2017.non, State.Code=='53')
# epa.2018.non <- read.csv("../Data/USEPA hourly PM 88502 data/hourly_88502_2018.csv"); epa.2018.non.wa <- subset(epa.2018.non, State.Code=='53')
# 
# epa.non <- rbindlist(list(epa.2010.non.wa, epa.2011.non.wa, epa.2012.non.wa, epa.2013.non.wa, epa.2014.non.wa, epa.2015.non.wa, epa.2016.non.wa, epa.2017.non.wa, epa.2018.non.wa))
# 
# epa.non$datetime_UTC = ymd_hm(paste(epa.non$Date.GMT, epa.non$Time.GMT))
# epa.non$datetime_local = as_datetime(epa.non$datetime, tz = "America/Los_Angeles")
# 
# epa.non$year     <- year(epa.non$datetime_local)
# 
# epa.non.1h <- subset(epa.non, select = c(State.Code, County.Code, County.Name, Site.Num, Latitude, Longitude, Datum, Parameter.Name, datetime_local, year, Date.Local, Time.Local, Date.GMT, Time.GMT, Sample.Measurement, Units.of.Measure, State.Name, County.Name))
# 
# table(epa.non.1h$County.Name,epa.non.1h$Site.Num)
# 
# #epa.non.1h <- epa.non.1h %>% unite("Site", County.Name:Site.Num, sep = "-", remove = FALSE)
# 
# epa.non.1h[, Site := paste0(County.Name,"-", Site.Num)]
# 
# epa.non.1h.summary  <- ddply(epa.non.1h, .(Site,year), 
#                              summarize,  PM=mean(Sample.Measurement), N=length(year))
# 
# setDT(epa.non.1h)
# 
# rm(epa, epa.2010, epa.2011, epa.2012, epa.2013, epa.2014, epa.2015, epa.2016, epa.2017, epa.2018)
# rm(epa.2010.wa, epa.2011.wa, epa.2012.wa, epa.2013.wa, epa.2014.wa, epa.2015.wa, epa.2016.wa, epa.2017.wa, epa.2018.wa)
# #rm(epa.nearestAWN.merge, locationsAWN, locationsPM, nearestAWN, set1sp, set2sp)
# 
# setDT(epa.1h); unique(epa.1h[c("Latitude", "Longitude")]); unique(epa.1h$Site)
# 
# save(epa.non.1h, file =         "Data/epa.non.1h.2010.2018.RData")
# save(epa.non.1h.summary, file = "Data/epa.non.1h.summary.2010.2018.RData")

###Merge all EPA data ##############################
# load("Data/epa.non.1h.2010.2018.RData")
# load("Data/epa.non.1h.summary.2010.2018.RData")
# load("Data/epa.1h.2010.2018.RData")
# load("Data/epa.1h.summary.2010.2018.RData")
#
#sites by county
#
# epa.1h[, length(unique(Site)), by = County.Name]
# setnames(epa.1h, "datetime_PST", "datetime_local")
# epa.1h$County.Name.1 = NULL
# 
# epa.non.1h[, length(unique(Site)), by = County.Name]
# 
# epa.all = rbindlist(list(epa.1h, epa.non.1h), fill = T)
#save(epa.all, file =         "Data/epa.all.1h.2010.2018.RData")
#

#
#
# epa.all.1h.summary  <- setDT(ddply(epa.all, .(Site, year),
#                              summarize,  
#                              PM= mean(Sample.Measurement), 
#                              N=length(year), 
#                              County = County.Name[1]))
#                              
#save(epa.all.1h.summary, file = "Data/epa.all.1h.summary.2010.2018.RData")

#Merge AWN and EPA data###################
 
# load("Data/epa.all.1h.summary.2010.2018.RData")
# load("Data/awn.1h.summary.2010.2018.RData")
# load("Data/epa.all.1h.2010.2018.RData")
# load("Data/awn.1h.2010.2018.RData")
# 
# # #find nearest weather station
# locationsPM  = unique(epa.all[, c("Longitude","Latitude")])
# locationsAWN = unique(awn.1h[, c("Longitude.awn","Latitude.awn")])
#  
# set1sp <- SpatialPoints(locationsPM)
# set2sp <- SpatialPoints(locationsAWN)
#  
# locationsPM$nearest_in_set2 <- apply(gDistance(set2sp, set1sp, byid=T), 1, which.min)
# 
# nearestAWN = locationsAWN[locationsPM$nearest_in_set2,]
# nearestAWN$Longitude.pm = locationsPM$Longitude
# nearestAWN$Latitude.pm = locationsPM$Latitude
# 
# #calculate haversine distance. Returns meters. Assumes radius of the earth is 6378137 m 
# nearestAWN$dist_m = distHaversine((nearestAWN[,c("Longitude.awn", "Latitude.awn")]), (nearestAWN[,c("Longitude.pm", "Latitude.pm")]))
# nearestAWN$dist_mi = nearestAWN$dist_m/1609.34
# 
# #Merge AWN and EPA data
# 
# setDT(nearestAWN)
# 
# epa.nearestAWN.merge = merge(epa.all, nearestAWN, by.x=c("Longitude","Latitude"), by.y=c("Longitude.pm","Latitude.pm"))
# 
# setnames(awn.1h, "datetime_PST","datetime_local")
# 
# setDT(epa.nearestAWN.merge); setkey (epa.nearestAWN.merge, Longitude, Latitude, datetime_local)
# setDT(awn.1h); setkey (awn.1h[!is.na(Longitude.awn)], Longitude.awn, Latitude.awn, datetime_local)
# 
# epa.awn.final.wide = merge(epa.nearestAWN.merge, awn.1h, by.x= c("Longitude.awn", "Latitude.awn", "datetime_local"), by.y = c("Longitude.awn", "Latitude.awn", "datetime_local"))
# 
# epa.awn.final.long <- melt(data = epa.awn.final.wide, id.vars = c("Latitude","Longitude","datetime_local","Site"), measure.vars = c("Sample.Measurement", "hi"))
# 
# setDF(epa.awn.final.long); z<- unique(epa.awn.final.long[c("Latitude", "Longitude")])
# setDT(epa.awn.final.long)
# save(epa.awn.final.long , file  = "Data/epa.awn.final.long.2010.2018.RData")
# save(epa.awn.final.wide , file  = "Data/epa.awn.final.wide.2010.2018.RData")
# 
load("Data/epa.all.1h.summary.2010.2018.RData")
load("Data/epa.awn.final.long.2010.2018.RData")
load("Data/epa.awn.final.wide.2010.2018.RData")
