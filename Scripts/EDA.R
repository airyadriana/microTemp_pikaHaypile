


# Clean:
rm(list = ls(all.names = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)

#################
## Description ##
#################
# Exploratory Data Analysis (EDA)



##############
## Packages ##
##############
require(dplyr)
require (mgcv)
################
## Statements ##
################
sen.file.meta <- read.csv("data/montana-paired-occ-study-metadata.csv",stringsAsFactors=F) #reads the file with the sensor.year data we need
sen.file.meta <- sen.file.meta[1:22,2:13]


##--Clean files

for(i in 1:nrow(sen.file.meta)){
  # grab an id
  senFile <- paste("/Users/airy/Documents/R/Pika/microTemp_pikaHaypile/data",
                   "/",
                   sen.file.meta$sensor[i],
                   "-", 
                   sen.file.meta$sensorSN[i],
                   "-2022.csv", 
                   sep ="")
  # open file
  d <- read.csv(senFile,stringsAsFactors=F, skip = 1)
  # H or L series
  type <- strsplit(sen.file.meta$sensor[i], split = "")[[1]][1]
  #     if L use the placed and servicing datetimes to clean
  #     if H use a 5 hour window after placing and the servicing datetime
  
}










##--Plot each pair data together
sensorIDs <- sen.file.meta$sensor # list with sensor IDs
track <- NA # container to store those sensors that were plotted
for (s in 1:length(sensorIDs)) {
  sen <- sensorIDs[s] # grab a sensor
  if (sen %in% track == T){ # test if this sensor has already been processed
    next # if so, go to next ID
  } else { # if not, look for its pair
    matchSen <- sen.file.meta$pairedWith[s]
  }
  
  # investigate what kind of sensors we dealing with (RH or Temp/Light).
  #  if Temp/Light, we can use the time of deployment and service to clean the
}






