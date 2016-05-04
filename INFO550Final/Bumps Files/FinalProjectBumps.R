setwd("C:/Users/sgille3/Desktop/INFO550Final/Bumps Files")

library(DBI)
library(RSQLite)
library(plyr)
library(dplyr)
library(ggmap)
library(qdapRegex)
library(leaflet)
library(data.table)
library(lubridate)
library(openintro)
library(googleVis)


###############################################
#Bump 1 - Geocoding every nursing home facility
###############################################

projectDB      <- src_sqlite(path="FinalProjectDB.db")

####Southeast####
providerInfoSE <- as.data.frame(tbl(projectDB, "providersQuery") %>%
                                  filter(STATE %IN% c('AL', 'FL', 'GA', 'MS', 'NC', 'SC', 'TN')) %>%
                                  select(provnum, STATE, ZIP, BEDCERT, CERTIFICATION, 
                                         PARTICIPATION_DATE, overall_rating,
                                         TOTHRD, exp_total, adj_total, WEIGHTED_ALL_CYCLES_SCORE, 
                                         incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT))
#latLongSE<-geocode(providerInfoSE$ZIP, output = "latlona")
#latLongSE$ZIP <- unlist(rm_zip(latLongSE$address,extract=TRUE))
#write.csv(latLongSE,'latLongSE.csv')
latLongSE     <- read.csv("latLongSE.csv", header = TRUE)
latLongSE$ZIP <- as.character(latLongSE$ZIP)


####Midwest######

####Midwest 1####
providerInfoMW1 <- as.data.frame(tbl(projectDB, "providersQuery") %>%
                                   filter(STATE %IN% c('IN', 'MI', 'OH', 'SD', 'ND', 'WI')) %>%
                                   select(provnum, STATE, ZIP, BEDCERT, CERTIFICATION, 
                                          PARTICIPATION_DATE, overall_rating,
                                          TOTHRD, exp_total, adj_total, WEIGHTED_ALL_CYCLES_SCORE, 
                                          incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT))
#latLongMW1<-geocode(providerInfoMW1$ZIP, output = "latlona")
#latLongMW1$ZIP <- unlist(rm_zip(latLongMW1$address,extract=TRUE))
#write.csv(latLongMW1,'latLongMW1.csv')
latLongMW1     <- read.csv("latLongMW1.csv", header = TRUE)
latLongMW1$ZIP <- as.character(latLongMW1$ZIP)

####Midwest 2####
providerInfoMW2 <- as.data.frame(tbl(projectDB, "providersQuery") %>%
                                   filter(STATE %IN% c('AR', 'CO', 'IA', 'ID', 'KS', 'MO', 'MT', 'NE', 'NV', 'UT', 'WY')) %>%
                                   select(provnum, STATE, ZIP, BEDCERT, CERTIFICATION, 
                                          PARTICIPATION_DATE, overall_rating,
                                          TOTHRD, exp_total, adj_total, WEIGHTED_ALL_CYCLES_SCORE, 
                                          incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT))

#latLongMW2<-geocode(providerInfoMW2$ZIP, output = "latlona")
#latLongMW2$ZIP <- unlist(rm_zip(latLongMW2$address,extract=TRUE))
#write.csv(latLongMW2,'latLongMW2.csv')
latLongMW2     <- read.csv("latLongMW2.csv", header = TRUE)
latLongMW2$ZIP <- as.character(latLongMW2$ZIP)

####West####
providerInfoW <- as.data.frame(tbl(projectDB, "providersQuery") %>%
                                 filter(STATE %IN% c('AK', 'AZ', 'CA', 'HI', 'LA', 'NM', 'OR', 'OK', 'WA')) %>%
                                 select(provnum, STATE, ZIP, BEDCERT, CERTIFICATION, 
                                        PARTICIPATION_DATE, overall_rating,
                                        TOTHRD, exp_total, adj_total, WEIGHTED_ALL_CYCLES_SCORE, 
                                        incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT))

#latLongW<-geocode(providerInfoW$ZIP, output = "latlona")
#latLongW$ZIP <- unlist(rm_zip(latLongW$address,extract=TRUE))
#write.csv(latLongW,'latLongW.csv')
#latLongW     <- read.csv("latLongW.csv", header = TRUE)
#latLongW$ZIP <- as.character(latLongW$ZIP)



######Full Leaflet######
joinLatLonSE  <- distinct(inner_join(providerInfoSE,  latLongSE, "ZIP") %>%  select(-X)) 
joinLatLonMW1 <- distinct(inner_join(providerInfoMW1, latLongMW1, "ZIP")%>%  select(-X))
joinLatLonMW2 <- distinct(inner_join(providerInfoMW2, latLongMW2, "ZIP") %>% select(-X))


dataAnalysis <- rbind(joinLatLonSE, joinLatLonMW1, joinLatLonMW2)

dataAnalysis %>%
  leaflet() %>% 
  addTiles() %>%  #you can also do "addProviderTiles(xyz)", where xyz is the basemap of your choice
  setView(-98.35, 39.7, zoom = 4) %>% #setting the zoom to AMERICA
  addCircleMarkers(lng=~lon, lat=~lat, # telling leaflet which var is the lat and long
                   clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=F)) 

#geocodeQueryCheck(userType = "free")


#########################################################################
#Bump #2 - Using iChoropleth function to plot deficiencies and penalities
#########################################################################

#library(devtools)
#install_github("ramnathv/rCharts")
#install_github("ramnathv/rMaps")
library(rCharts)
library(rMaps)

deficiencies<-as.data.frame(fread('Deficiencies_Download.csv', header=TRUE))
providerInfo<-as.data.frame(fread('ProviderInfo_Download.csv', header=TRUE))

numEvents <- as.data.frame(deficiencies %>%
                             mutate(year=year(survey_date_output)) %>%
                             group_by(year, state) %>%
                             summarise(numEvents = n()))

numProviders <- as.data.frame(providerInfo %>%
                                mutate(state=STATE) %>%
                                select(-STATE) %>%
                                group_by(state) %>%
                                summarise(numProviders = n()))  


longData <- inner_join(numEvents, numProviders, "state") %>%
  group_by(state, year) %>%
  summarise(defRatePer10 = (numEvents/numProviders)*10) %>%
  select(year,state,defRatePer10)

source('ichoropleth.R')
ichoropleth(defRatePer10 ~ state,
            data = longData,
            pal = "Reds",
            ncuts = 5,
            animate = 'year')


################################################################################
Bump #3 - Steps in the direction of an application that can be utilized in Shiny
################################################################################

projectDB    <- src_sqlite(path="FinalProjectDB.db")

providerInfoSE <- as.data.frame(tbl(projectDB, "providersQuery") %>%
                                  select(provnum, STATE, ZIP, BEDCERT, CERTIFICATION, 
                                         PARTICIPATION_DATE, overall_rating,
                                         TOTHRD, exp_total, adj_total, WEIGHTED_ALL_CYCLES_SCORE, 
                                         incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT))



stateSummary <- as.data.frame(providerInfoSE %>%
                                group_by(STATE) %>%
                                filter(STATE != '<NA>') %>%
                                summarize(n = n(), 
                                          avgBedCert  = mean(as.integer(BEDCERT)),
                                          avgRating   = mean(overall_rating),
                                          avgNurse    = mean(TOTHRD),
                                          avgExpNurse = mean(exp_total),
                                          avgAdjNurse = mean(adj_total),
                                          avgWgtScore = mean(WEIGHTED_ALL_CYCLES_SCORE),
                                          avgIncCnt   = mean(incident_cnt),
                                          avgCmpCnt   = mean(cmplnt_cnt),
                                          avgFineCnt  = mean(FINE_CNT),
                                          avgPenCnt   = mean(TOT_PENLTY_CNT)))



GeoStates <- gvisGeoChart(stateSummary, "STATE", "avgPenCnt", "avgWgtScore",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)