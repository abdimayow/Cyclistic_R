library(dplyr)

allstations <- read.csv("data/station_summary.csv")
allstations$lat<- jitter(allstations$lat)
allstations$lng<-jitter(allstations$lng)
