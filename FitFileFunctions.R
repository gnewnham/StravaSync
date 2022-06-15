
######################################################################
# Funcitons for strava FIT file processing based on:
# https://msmith.de/FITfileR/articles/FITfileR.html
#
# Glenn Newnham
# 13.06.2022
######################################################################

# # Currently FITfileR is only available on Github, and can be installed using the remotes package.
# if(!requireNamespace("remotes")) {
# 	install.packages("remotes")
# }
# remotes::install_github("grimbough/FITfileR")
#

# path = 'C:/Users/new298/OneDrive - CSIRO/Code/Rscript/Strava/'
# fitfile = paste(path, 'Morning_Ride.fit', sep='')
# fitData = FitToDataframe(fitfile)
# FitFilePlots(fitData)


FitToDataframe = function(fitFileName) {

	library(FITfileR)
	library(dplyr)

	#extract the data
	fitData <- readFitFile(fitFileName)

	#merge the tibbles into a single activity
	allrecords <- records(fitData) %>%
		bind_rows() %>%
		arrange(timestamp)

	#seems some error records appear with 180 degree lat so remove them
	allrecords = allrecords[allrecords$position_lat<=170,]

	# convert enhanced_speed (m/s) to (km/h)
	allrecords$enhanced_speed = allrecords$enhanced_speed *3600 / 1000

	return(allrecords)
}


FitFileMap = function(fitRecords) {

	library(leaflet)

	#plotting locations
	coords <- fitRecords %>%
		select(position_long, position_lat)

	coords %>%
		as.matrix() %>%
		leaflet(  ) %>%
		addTiles() %>%
		addPolylines( )
}

#plot out the activity metrics
FitFilePlots = function(fitRecords) {

	avSpd = mean(fitRecords$enhanced_speed)
	avHr = mean(fitRecords$heart_rate)
	titleText = paste('Average Speed:' , round(avSpd,2), '      Average HR:', round(avHr))

	par(mfrow=c(2,2))

		plot(fitRecords$timestamp, fitRecords$distance, type='l', xlab='Time', ylab='Distance')
		mtext(titleText, side = 3, line = -2, outer = TRUE, font=2, cex=1.5)

		plot(fitRecords$timestamp, fitRecords$enhanced_speed, type='l', xlab='Time', ylab='Speed')
		abline(avSpd, 0, col='red')

		plot(fitRecords$timestamp, fitRecords$enhanced_altitude, type='l', xlab='Time', ylab='Altitude')

		plot(fitRecords$timestamp, fitRecords$heart_rate, type='l', xlab='Time', ylab='Heart Rate')
		abline(avHr, 0, col='red')

	par(mfrow=c(1,1))

}
