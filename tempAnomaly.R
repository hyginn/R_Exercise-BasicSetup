# tempAnomaly.R
#
# Purpose:  Plot a temperature anomaly distribution from Berkeley Earth data.
#           See https://www.youtube.com/watch?v=xWpTGbZhZfQ for the inspiration
#           we are working from
# Version:
# Version history:
# Date:     2019-09-24
# Author:   boris.steipe@utoronto.ca
# License:  CC-BY
#
# Input:
# Output:
# Dependencies:
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
# TBD
n2YM <- function(x) {
	# Purpose: Convert a vector of numbers like 2017.04166666 to
	#          a vector of strings in a year:month format
	# Parameters:
	#     x: input number
	# Value:
	#     vector of strings

	year <- floor(x)
    month <- round(((x - year) * 12 ) + 0.5)
    v <- sprintf("%d:%d", year, month)

	return(v)
}

?floor


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

# Read data into R workspace
# Don't do this - just follow along:

# file.exists(file.path(DATADIR, "Land_and_Ocean_EqualArea.nc"))
# tmp <- nc_open(file.path(DATADIR, "Land_and_Ocean_EqualArea.nc"))
# str(tmp)
# print(tmp)
#
# head(ncvar_get(tmp, "longitude"), 100)
# head(ncvar_get(tmp, "latitude"), 100)
# range(ncvar_get(tmp, "latitude"), 100)
# x <- ncvar_get(tmp, "temperature")
# t <- ncvar_get(tmp, "time")
# tail(t)
#
# t36 <- list()
# t36$lon <- ncvar_get(tmp, "longitude")
# t36$lat <- ncvar_get(tmp, "latitude")
# t36$t   <- ncvar_get(tmp, "temperature")[ , 2001:2036]
# t36$tPoints <- ncvar_get(tmp, "time")[2001:2036]
# colnames(t36$t) <- ncvar_get(tmp, "time")[2001:2036]
# colnames(t36$t) <- n2YM(ncvar_get(tmp, "time")[2001:2036])
# rownames(t36$t) <- 1:nrow(t36$t)
# save(t36, file = "t36.RDat")

load("t36.RDat")
str(t36)

# Plot a histogram

# Adjust titles and legends, add labels

# Plot an envelope of the histogram

# Compute averages; plot monthly global averages

# Plot a time series for Toronto, with a color spectrum





# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and vaildation of analysis ...

}


# [END]
