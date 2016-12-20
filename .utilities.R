# .utilities.R
#
# Miscellaneous R code to suppport the R_Exercise-BasicSetup project
#
# Version: 1.0
# Date:    2016 12
# Author:  Boris Steipe
#
# V 1.0    First code
#
# ToDo:
# Notes:
#
# ==============================================================================

objectInfo <- function(x) {
    # Function to combine various information items about R objects
    #
    # Input: an R object
    # Value: none - prints information as side-effect

    print(x, digits = 22)  # print value at maximal precision

    cat("str:    ", str(x), "\n")
    cat("mode:   ", mode(x), "\n")
    cat("typeof: ", typeof(x), "\n")
    cat("class:  ", class(x), "\n")

    # if the object has attributes, print them too
    if (! is.null(attributes(x))) {
        cat("attributes:\n")
        print(attributes(x))
    }
    # Done
}



# [END]
