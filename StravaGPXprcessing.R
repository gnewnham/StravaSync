
# example of strava GPX file processing based on:
# https://rpubs.com/ials2un/gpx1
#
# Glenn Newnham
# 11.06.2022


path = 'C:/My Files/'
# files = choose.files(paste(path, '*.gpx', sep=''))
gpxfile = paste(path, 'Morning_Ride.gpx', sep='')


gpxDF = GpxToDataframe(gpxfile)
gpxDF = addMetrics(gpxDF)
PlotRide(gpxDF)
mapRide(gpxDF)



#############
#
# Function GpxToDataframe
# 11.06.2022
#
# Convert a Strava GPX file into a gpxDataFrame
#
#############

GpxToDataframe = function(inFile) {

	library(XML)

	xml_data = htmlTreeParse(file=inFile, error=function(...) {
		}, useInternalNodes = T)

	times = xpathSApply(xml_data, path = "//trkpt/time", xmlValue)
	times = strptime(times, format = "%Y-%m-%dT%H:%M:%OS")
	length(times)

	# Extract data
	elevations = as.numeric(xpathSApply(xml_data, path = "//trkpt/ele", xmlValue))
	coords = xpathSApply(xml_data, path = "//trkpt", xmlAttrs)
	lats = as.numeric(coords["lat",])
	lons = as.numeric(coords["lon",])

	# Put it all in a dataframe
	geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)

	rm(list=c("elevations", "lats", "lons", "xml_data", "times", "coords"))

	return(geodf)
}

#############
#
# Function addMetrics
# 11.06.2022
#
# Function to calculate distance and speed and add them to  a gpxDataFrame
#
#############

addMetrics = function(geodf) {

	library(raster)

	npoints = length(geodf)/2

	# Shift vectors for lat and lon so that each row also contains the next position.
	geodf$lat.p1 = c(geodf$lat[2:npoints], geodf$lat[npoints])
	geodf$lon.p1 = c(geodf$lon[2:npoints], geodf$lon[npoints])
	geodf$time.p1 = c(geodf$time[2:npoints], geodf$time[npoints])

	geodf$dist = pointDistance(data.matrix(geodf[c('lon.p1','lat.p1')]),
							   data.matrix(geodf[c('lon','lat')]), lonlat = T)

	geodf$time.diff <- as.numeric(difftime(geodf$time.p1, geodf$time))

	# Total ride distance
	td = sum(geodf$dist, na.rm=TRUE)
	print(paste("Total ride distance: ", round(td/1000, 2), " km"))

	# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
	geodf$speed.m.per.sec <- geodf$dist / geodf$time.diff
	geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
	geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
	geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
	geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y

	return(geodf)
}

#############
#
# Function PlotRide
# 11.06.2022
#
# Function to plot the elevation and speed from a GPX file derived Dataframe
#
# The Dataframe must contain variables "ele" and "lowess.ele" for elevation
# and "speed.km.per.h" and "lowess.speed" for speed and smoothed speed
#
#############

PlotRide = function(geodf) {

	par(mfrow=c(2,1))

	# Plot elevations and smoother
	plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
	lines(geodf$lowess.ele, col = "red", lwd = 3)
	legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
		   col = c("grey40", "red"), lwd = c(1,3), bty = "n")

	# Plot speeds and smoother
	plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
		 col = "grey40")
	lines(geodf$lowess.speed, col = "blue", lwd = 3)
	legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
		   col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
	abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")

	par(mfrow=c(1,1))
}

#############
#
# Function MapRide
# 11.06.2022
#
# Function to map a ride based on a GPX file derived Dataframe
#
# Various options exist but currently MapView is used as default.
#
# The Dataframe must contain variables "lat" and "lon"
# so it can be concerted to a spatial dataframe
#
#############

mapRide = function(geodf) {

	library(ggmap)
	library(mapview)
	library(leaflet)
	library(leafem)

	# # Plot the track without any map, the shape of the track is already visible.
	# plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")

	# bbox <- make_bbox(c(min(geodf$lon), max(geodf$lon)), c(min(geodf$lat), max(geodf$lat)))
	# b1<- get_map(bbox, maptype="watercolor", source="stamen")
	#
	# ggmap(b1) + geom_point(data = geodf, aes(lon,lat,col = ele), size=1, alpha=0.7)+
	# 	labs(x = "Longitude", y = "Latitude", title="Ride overlayed on OpenStreemap")

	spdf_geo <- geodf
	coordinates(spdf_geo) <- ~ lon + lat
	proj4string(spdf_geo) <- "+init=epsg:4326"

	mapview(spdf_geo)

	leaflet() %>%
		addTiles() %>%
		addFeatures(spdf_geo, weight = 1, fillColor = "grey", color = "black",
					opacity = 1, fillOpacity = 0.6)
}
