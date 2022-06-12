
# example of strava FIT file processing based on:
# https://msmith.de/FITfileR/articles/FITfileR.html
#
# Glenn Newnham
# 11.06.2022

# Currently FITfileR is only available on Github, and can be installed using the remotes package.
if(!requireNamespace("remotes")) {
	install.packages("remotes")
}
remotes::install_github("grimbough/FITfileR")

path = 'C:/My Files/'
# files = choose.files(paste(path, '*.fit', sep=''))
fitfile = paste(path, 'Morning_Ride.fit', sep='')


library(FITfileR)

#extract the data
fitData <- readFitFile(fitfile)
fitData

#what sort of metadata is recorded?
listMessageTypes(fitData)
getMessagesByType(fitData, message_type = 'user_profile')

#how many records in each of the activity tibbles/sectors
fitRecords <- records(fitData)
vapply(fitRecords, FUN = nrow, FUN.VALUE = integer(1))


#merge the tibbles into a single activity
library(dplyr)
allrecords <- records(fitData) %>%
	bind_rows() %>%
	arrange(timestamp)

allrecords = allrecords[allrecords$position_lat<=170,]

#plotting locations
library(leaflet)

coords <- allrecords %>%
	select(position_long, position_lat)

coords %>%
	as.matrix() %>%
	leaflet(  ) %>%
	addTiles() %>%
	addPolylines( )


#plot out the activity metrics

par(mfrow=c(2,2))
plot(allrecords$timestamp, allrecords$distance, type='l')
plot(allrecords$timestamp, allrecords$enhanced_speed, type='l')
plot(allrecords$timestamp, allrecords$enhanced_altitude, type='l')
plot(allrecords$timestamp, allrecords$heart_rate, type='l')
par(mfrow=c(1,1))
