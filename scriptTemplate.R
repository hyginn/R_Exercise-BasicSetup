# scriptTemplate.R
#
# Purpose:
# Version:
# Date:
# Author:
#
# Input:
# Output:
# Dependencies:
#
# ToDo:
# Notes:
# ==============================================================================

setwd("<your/project/directory>")

# ====  PARAMETERS  ============================================================
# Define and explain all parameters. No "magic numbers" in your code below.



# ====  PACKAGES  ==============================================================
# Load all required packages.

if (!require(RUnit, quietly=TRUE)) {
    install.packages("RUnit")
    library(RUnit)
}


# ====  FUNCTIONS  =============================================================

# Define functions or source external files
source("<myUtilityFunctionsScript.R>")





# ====  PROCESS  ===============================================================
# Enter the step-by-step process of your project here. Strive to write your
# code so that you can simply run this file and re-create all intermediate
# results.






# ====  TESTS  =================================================================
# Enter your function tests here...


# [END]
