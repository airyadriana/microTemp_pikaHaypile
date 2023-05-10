


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
# require(dplyr)
# require (mgcv)
#require(ggplot2)
################
## Statements ##
################
sen.file.meta <- read.csv("data/montana-paired-occ-study-metadata.csv",stringsAsFactors=F) #reads the file with the sensor.year data we need
sen.file.meta <- sen.file.meta[1:22,2:13]
sen.file.meta <- sen.file.meta[-c(9,10,18,19, 21,22), ] # EL 4 was not serviced in 2021, but it's match was. Drop them for now
                                                 # the other two need to be read by me using the coupler

##--Plot each pair data together: Occupied vs unoccupied
sensorIDs <- sen.file.meta$sensor # list with sensor IDs
track <- NA # container to store those sensors that were plotted
#s<-13
for (s in 1:length(sensorIDs)) {
  sen <- sen.file.meta$sensor[s] # grab a sensor
  if (sen %in% track == T){ # test if this sensor has already been processed
    next # if so, go to next ID
  } else { # else, plot a pair in the same graph
    senMatch <- sen.file.meta$pairedWith[s] # get the pair's ID
    
    #open cleaned files
    fileName1 <- paste("/Users/airy/Documents/R/Pika/microTemp_pikaHaypile/data/temporaryClean", # get the Path for the sensor
                       "/",
                       sen,
                       "-",
                       sen.file.meta$sensorSN[s],
                       "-2022-IPWA.csv", 
                       sep ="")
    fileName2 <- paste("/Users/airy/Documents/R/Pika/microTemp_pikaHaypile/data/temporaryClean", # get the Path for the pair-sensor
                       "/",
                       senMatch,
                       "-",
                       sen.file.meta$sensorSN[which(sen.file.meta$sensor == senMatch)],
                       "-2022-IPWA.csv", 
                       sep ="")
    d.sensor <- read.csv(fileName1,stringsAsFactors=F, skip = 1)
    d.pair <- read.csv(fileName2,stringsAsFactors=F, skip = 1)
    colnames(d.sensor)[1:3] <- c("row", "datetime", "tempC" )
    colnames(d.pair)[1:3] <- c("row", "datetime", "tempC" )
    
    # add a column with sensor name
    d.sensor$ID <- rep(sen,
                       nrow(d.sensor))
    d.pair$ID <- rep(senMatch,
                     nrow(d.pair))
    
    # add treatment 
    d.sensor$occ <- rep(sen.file.meta$typicalOccOfHaySite[s],
                        nrow(d.sensor))
    d.pair$occ <- rep(sen.file.meta$typicalOccOfHaySite[which(sen.file.meta$sensor == senMatch)],
                        nrow(d.pair))
    
    # create one single dataset
    d.sensor <- as.data.frame(cbind(d.sensor$datetime, 
                d.sensor$tempC,
                d.sensor$ID,
                d.sensor$occ))
    colnames(d.sensor) <- c("datetime", "tempC", "id", "occ")
    d.pair <- as.data.frame(cbind(d.pair$datetime,
                d.pair$tempC,
                d.pair$ID,
                d.pair$occ))
    colnames(d.pair) <- c("datetime", "tempC", "id", "occ")
    
    # format datetime
    d.sensor$dt <- as.POSIXct(strptime(d.sensor$datetime,
                                format = "%m/%d/%y %I:%M:%S %p"))
    d.pair$dt <- as.POSIXct(strptime(d.pair$datetime,
                                       format = "%m/%d/%y %I:%M:%S %p"))

    # as numeric
    d.sensor$tempC <- as.numeric(d.sensor$tempC)
    d.pair$tempC <- as.numeric(d.pair$tempC)
    
    # plot overlapping
    
    if((min(d.sensor$tempC, na.rm = T) < min(d.pair$tempC, na.rm = T)) == T){
      minimo <- min(d.sensor$tempC, na.rm = T)-2
    } else {
      minimo <- min(d.pair$tempC, na.rm = T)-2
    }
   
    if((max(d.sensor$tempC, na.rm = T) < max(d.pair$tempC, na.rm = T)) == T){
      maximo <- max(d.sensor$tempC, na.rm = T)+2
    } else {
      maximo <- max(d.pair$tempC, na.rm = T)+2
    
    
    if((unique(d.sensor$occ) == "O") == T){
      plot(x = d.pair$dt,
           y = d.pair$tempC,
           type = "l",
           xlab = "DateTime",
           ylab = "Temperature (\u00B0C)",
           ylim = c(minimo,maximo))
      lines(x = d.sensor$dt,
            y = d.sensor$tempC,
            col = "red"
            )
      title("Microclimatic Temperature in Haypiles")
      legend("top",
             c(paste("Unoccupied-",
                     unique(d.pair$id),
                     sep = ""),
               paste("Occupied-",
                     unique(d.sensor$id),
                     sep = "")), 
             lwd=c(2,2), 
             col=c("black",
                   "red"), 
             y.intersp=1.5)
    } else {
      plot(x = d.sensor$dt,
           y = d.sensor$tempC,
           type = "l",
           xlab = "DateTime",
           ylab = "Temperature (\u00B0C)",
           ylim = c(minimo,maximo))
      lines(x = d.pair$dt,
            y = d.pair$tempC,
            col = "red")
      title("Microclimatic Temperature in Haypiles")
      legend("top",
             c(paste("Unoccupied-",
                     unique(d.sensor$id),
                     sep = ""),
               paste("Occupied-",
                     unique(d.pair$id),
                     sep = "")), 
             lwd=c(2,2), 
             col=c("black",
                   "red"), 
             y.intersp=1.5)
    }
    
    if(is.na(track[1]) == T){
      track <- c(sen,senMatch)
    } else {
      track <- c(track,sen,senMatch)
    }
  }
}
}

# coldest month (acute cold stress): January
  # Mean
  # Min
  # Max
  # days warmer than unoccupied haypile
# hottest month (acute hot stress): July
# coldest quarter (chronic): bioclim 11
# hottest quarter (chronic): bioclim 10












###################################################################


