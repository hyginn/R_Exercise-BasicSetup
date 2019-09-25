# tempAnomaly.R
#
# Purpose:  Plot a temperature anomaly distribution from Berkeley Earth data.
#           See https://www.youtube.com/watch?v=xWpTGbZhZfQ for the inspiration
#           we are working from.
# Version:  0.1
# Version history:
#           0.1 Cleaned up after presenting in class
#
# Date:     2019-09-24
# Author:   boris.steipe@utoronto.ca
# License:  CC-BY
#
# ToDo:
# Notes:
#
# ==============================================================================

# WARNING: SIDE EFFECTS
# Executing this script will execute code it contains.

# ====  PARAMETERS  ============================================================

# ... you'll need to define your own
DATADIR <- "~/Documents/07.Teaching/36-BCH441 Bioinformatics 2019/data"





# ====  PACKAGES  ==============================================================

if (! requireNamespace("ncdf4", quietly=TRUE)) {
    install.packages("ncdf4")
}
library(ncdf4)
# Package information:  library(help = ncdf4)       # basic information




# ====  FUNCTIONS  =============================================================
#
n2YM <- function(x) {
	# Purpose: Convert a vector of numbers like 2017.04166666 to
	#          a vector of strings in a year:month format. The numbers
	#          come from the source NetCDF file, the strings are used for
	#          column names.
	# Parameters:
	#     x: vector of input numbers. Integer part is the year, fractional
	#        part is the midpoint of a 12-month interval mapped to [0, 1].
	# Value:
	#     vector of strings: YYYY:M

	year <- floor(x)
    month <- round(((x - year) * 12 ) + 0.5)
    v <- sprintf("%d:%d", year, month)

	return(v)
}


getMapPointIndex <- function(lat, lon, lats, lons) {
    # Purpose: Find the map point index which is closest to lat, lon given
    #          grid cell centres in two vectors of lats and lons
    # Parameters:
    #     lat: requested latitude
    #     lon: requested longitude
    #     lats: vector of latitudes
    #     lons: vector of longitudes
    # Value:
    #     index of closest map point

    # code ...

    return(i)
}




# ====  PROCESS  ===============================================================

# Define data source
#    from http://berkeleyearth.org/data/ .. gridded NetCDF data (~16,000
#    equal area grid cells)
#    Download Monthly Land + Ocean T_avg w. air temperatures at sea ice;
#    1850 - Recent; ~ 100 MB
#    http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_EqualArea.nc

# ==== Read data into R workspace
# (If you want the original, uncomment and execute; this part creates a small
#  version for development purposes.)

## Check that the file is in its expected location:
# file.exists(file.path(DATADIR, "Land_and_Ocean_EqualArea.nc"))
#
## Open dataset, assign to some temporary variable:
# tmp <- nc_open(file.path(DATADIR, "Land_and_Ocean_EqualArea.nc"))
# str(tmp)
# print(tmp) # Note: print() is a "generic" function that dispatches the
#            #       specific print.ncdf4() function that was installed with the
#            #       ncdf4 package, when it recognizes that the object tmp
#            #       is of class "ncdf4"  (cf. class(tmp) )
#
## Examine the contained data:
# head(ncvar_get(tmp, "longitude"), 100)
# head(ncvar_get(tmp, "latitude"), 100)
# range(ncvar_get(tmp, "latitude"), 100)
# x <- ncvar_get(tmp, "temperature")
# t <- ncvar_get(tmp, "time")
# tail(t)
#
#
## Extract 36 months worth of sample data from the last columns of the matrix
## and store in a list object:
# t36 <- list()
# t36$lon <- ncvar_get(tmp, "longitude")
# t36$lat <- ncvar_get(tmp, "latitude")
# t36$t   <- ncvar_get(tmp, "temperature")[ , 2001:2036]
# t36$tPoints <- ncvar_get(tmp, "time")[2001:2036]
# colnames(t36$t) <- ncvar_get(tmp, "time")[2001:2036]
# colnames(t36$t) # Hm. this is not well formatted. We write (and use) the
#                 # function n2YM() instead.
# colnames(t36$t) <- n2YM(t36$tPoints)
# rownames(t36$t) <- 1:nrow(t36$t)
#
## save() list object and write to current working directory
# save(t36, file = "t36.RDat")


# To work with the save()'d object, load() the data:
load("t36.RDat")
str(t36)  # Confirm basic structure

# Explore the data ... (randomly using column 17:  January 2018)
colnames(t36$t)[17]
range(t36$t[ , 17])
summary(t36$t[ , 17])
boxplot(t36$t[ , 17])

# Plot a histogram
hist(t36$t[ , 17])
hist(t36$t[ , 17], breaks = 50, col = "#0055BB") # More bins, and in color

# transparent colours - overlay January and July
hist(t36$t[ , "2018:1"], breaks = 15, col = "#0055BB55", ylim = c(0, 10000))
hist(t36$t[ , "2018:7"], breaks = 15, col = "#BB550055", add = TRUE)
# Add vertical lines for the medians
abline(v = median(t36$t[ , "2018:1"]), col = "#0055BB")
abline(v = median(t36$t[ , "2018:7"]), col = "#BB5500")
# Note: significant differences in the distributions are not as obvious in
#       the aggregate statistics.

# Adjust titles and legends, add labels.
hist(t36$t[ , 17], breaks = 40,
     freq = FALSE,
     col = "#00BB5588",
     main = "Temperature anomaly distribution",
     xlab = "Anomaly (°C)"),
     fraction of
text(10, 0.4, colnames(t36$t)[17])

# Plot an envelope of the histogram.
# Strategy: store histogram values in a variable, then plot an
# empty frame, then use lines() to plot the envelope.
myHist <- hist(t36$t[ , 17], breaks = 40,
               plot = FALSE,
               freq = FALSE,
               col = "#00BB5588",
               main = "Temperature anomaly distribution",
               xlab = "Anomaly (°C)")
plot(myHist$mids, myHist$density, type = "n",
     main = "Histogram envelope",
     xlab = "Anomaly (°C)")
lines(myHist$mids, myHist$density, col = "#FF7700", lwd = 3)
text(12, 0.5, colnames(t36$t)[17], col = "#FF7700", cex = 1.5)


# Compute averages; plot monthly global averages. Interpret the plot.

# Plot a time series for Toronto, with a color spectrum





# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
