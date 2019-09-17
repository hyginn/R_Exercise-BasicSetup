# anniversary.R
#
# Purpose: Calculate calndar date a specified number of days away from input.
# Version: 0.1
# Version history:
# Date:      2019-09-17
# Author:    Boris Steipe
# License:   MIT
#
# Input:    inDate   string   the date from which to calculate
#           interval int      desired offset interval
# Output:           string   the date after  interval  number of days
# Dependencies:
#
# ToDo:
# Notes:
#
# ==============================================================================

# NO SIDE EFFECTS:
# This script can be safely source()'d to define the functions it contains.


# ====  FUNCTIONS  =============================================================

# A lifedays calculator function

anniversary <- function(inDate, interval = 0) {
	# Purpose:
	#     Describe ...
	# Parameters:
	#     a: ...
	#     b: ...
	# Value:
	#     result: ...

	# code ...

    if (missing(inDate)) {
        print ("Enter the first date as a string in \"YYYY-MM-DD\" format.")
        return()
    }
    # convert inDate to time
    bd <- as.Date(inDate, "%Y-%m-%d") # convert string to time
    # add desired interval
    d <- bd + interval
    # print result
    print(sprintf("%d days from input is on %s.", interval, d))

}



# ====  EXAMPLE  ===============================================================

if (FALSE) {
   anniversary("1997-12-17", 10000)
}


# ====  TESTS  =================================================================
if (FALSE) {
# Enter your function tests here...

}


# [END]
