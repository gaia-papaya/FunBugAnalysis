#THIS IS THE SCRIPT TO PROCESS AIR QUALITY DATA. THIS SCRIPT IS MEANT TO RUN <Third> IN THE SCRIPT SEQUENCE
#RUNNING CODE OUT OF SEQUENCE WILL RESULT IN WORKFLOW FAILURE

#PROGRAMMER'S NOTE:
#half way thru writing this code I realized the superiority of using data.tables instead of dplyr/tibbles, so yea :b

#dependencies----
require(tidyverse)

#Read in Data----
#read length of AirQuality folder into vector
AQ_files <- list.files("RData/input/AirQuality/")

#iterate over vector to read files, send output to list
AQ_data_list <- list()
for(f in AQ_files){
  AQ_data_list[[f]] <- read.csv(paste("RData/input/AirQuality/",f, sep = ""), header = T)
}

#merge list of dataframes into singular dataframe, reformat datetime to POSIXct format
AQ_FullData <- bind_rows(AQ_data_list) %>%
mutate(DATE = as.POSIXct(DATE))

#classify rows by site using time interval from Tree data ----
#initialize site and replicate columns with blank character values (using data.table format)
dt_AQ_FullData <- data.table::as.data.table(AQ_FullData)
dt_AQ_FullData[, SITE := ""]
dt_AQ_FullData[, REP := ""]
for(s in 1:nrow(AQ_SiteSorting)){
  #site and rep name for given row
  site_name <- AQ_SiteSorting$Site[s]
  rep_name <- AQ_SiteSorting$Rep[s]
  #Date time start and end intervals for given row
  dt_start <- AQ_SiteSorting$Datetime_Start[s]
  dt_end <- AQ_SiteSorting$Datetime_End[s] 
  #change site name based on given interval and site name
  dt_AQ_FullData[DATE >= as.POSIXct(dt_start) & DATE <= as.POSIXct(dt_end), SITE := site_name]
  #change rep name based on given interval and site name
  dt_AQ_FullData[DATE >= as.POSIXct(dt_start) & DATE <= as.POSIXct(dt_end), REP := rep_name]
}

#hand repair data for site R1A
dt_AQ_FullData[DATE >= as.POSIXct("2023-06-04 12:54:59") & DATE <= as.POSIXct("2023-06-04 13:40:00"), SITE := "RA"]
dt_AQ_FullData[DATE >= as.POSIXct("2023-06-04 12:54:59") & DATE <= as.POSIXct("2023-06-04 13:40:00"), REP := "1"]

#Calculate per Site Stats----
#replace date and site blanks with NA
dt_AQ_FullData[SITE  == "", SITE := NA]
dt_AQ_FullData[REP  == "", REP := NA]
#omit NA values
dt_AQ_FullData <- na.omit(dt_AQ_FullData)

#convert temp from farenheit (cringe) to celsius (based)
dt_AQ_FullData[, TEMPERATURE := ((TEMPERATURE-32)*5)/9]

#paste site and rep string together for ez plotting
dt_AQ_FullData[, SiteRep := paste(str_sub(SITE, 1L, 1L), REP, str_sub(SITE, -1L,-1L), sep = "")]

#average each AQ variable by site
AQ_Summary <- dt_AQ_FullData[is.na(SITE) != T , .(AVG_PM2.5 = mean(PM2.5),
                                              AVG_PM10 = mean(PM10),
                                              AVG_CO2= mean(CO2),
                                              AVG_TEMP = mean(TEMPERATURE),
                                              AVG_HUMID = mean(HUMIDITY)), by= .(SiteRep)]
#remove outliers
AQ_Summary <- data.table::as.data.table(AQ_Summary)
AQ_Summary<- AQ_Summary[AVG_PM2.5 < 50 & AVG_PM10 <50]

#remove intermediary objects
rm(AQ_files,dt_end, dt_start, f, s, rep_name, site_name,AQ_data_list, AQ_FullData, AQ_SiteSorting, dt_AQ_FullData)
