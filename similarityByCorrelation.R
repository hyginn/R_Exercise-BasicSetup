# similarityByCorrelation.R
#
# Purpose:  Demonstrate how to find similar elements in high-dimensional
#           datasets.
# Version:  0.1
# Version history:
#           0.1 Initial code
#
# Date:     2019-10-22
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


# ====  PACKAGES  ==============================================================


# ====  FUNCTIONS  =============================================================


# ====  PROCESS  ===============================================================




# =    1  Correlation  =========================================================

# In principle, correlation between two variables measures the degree to which
# the value of one variable can be predicted from the value of the other. In
# particular, we usually refer to "linear correlation", i.e. predicting the
# value of one variable as a solution to a linear equation with the other
# variable. (Note that correlation does not necessarily imply causality.) In R,
# the function cov() measures covariance and cor() measures the Pearson
# coefficient of correlation (a normalized measure of covariance). Pearson's
# coeffecient of correlation values range from -1 to 1, with 0 indicating no
# correlation.

# Lets develop our intuition about what correlation values mean:

set.seed(12357)
x <- rnorm(50) # 50 random deviates from a N(0,1)
y <- rnorm(50) # again 50 random values
plot(x, y)
cor(x, y)
# This is uncorrelated data, our variables x and y are drawn from a normal
# ("Gaussian) distribution. cor(x, y) has a small value.

y <- -x
plot(x, y)
cor(x, y)
# This is perfectly correlated data, x is drawn from a normal ("Gaussian)
# distribution but y is just the same value with a change of sign. cor(x, y) is
# minus one, the two datasets are anticorrelated.

# Let's explore this with a bit more variety: here is a function that has a
# value r as an argument, and a function. We compute y as values that are to
# "r"-parts a function of x, and to (1-"r") parts random noise. Then we plot
# x vs. y, and compute cor(x,y)

plotCor <- function(x, r, f) {
    noise <- (1-r) * rnorm(length(x))
    y <- (r * f(x)) + noise
    plot(x, y)
    return(cor(x, y))
}

x <- rnorm(50)
plotCor(x, 0.99, function(x) { x })
plotCor(x, 0.90, function(x) { x })
plotCor(x, 0.80, function(x) { x })
plotCor(x, 0.40, function(x) { x })
plotCor(x, 0.01, function(x) { x })

# Correlations around 0.4 still correspond to a quite clearly visible trend. But
# note that cor() is not a good measure for non-linear correlations:


# periodic ...
plotCor(x, 0.9, function(x) { cos(x * pi) })

# polynomial ...
plotCor(x, 0.9, function(x) { x^2 })

# exponential
plotCor(x, 0.9, function(x) { exp(x) })

# circular ...
plotCor(cos(x*pi), 0.9, function(x) { sin(acos(x)) })

# In all of these cases, the clear functional relationship should have yielded a
# correlation of around 0.99 - but not a linear correlation, which is in fact
# much, much smaller.

# =    2  APPLYING REGRESSION TO EDA  ==========================================

# Let's load a real-world data set - yeast gene expression data. This data is
# derived from the GSE3635 expression set on GEO. Check it out:
#     https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE3635

load("ygProfiles.RData")   # yeast gene expression profiles

# To complement this, I have also prepared data for the genes, from data
# I downloaded from sgd. The dataset matches the expression profiles row by row
# so we can easily use it for annotation

load("ygData.RData")       # yeast gene data

ygProfiles[123, ]
rownames(ygProfiles)[123]
ygData[123, ]

# ==   3.01  Scenario  =========================================================

# We are interested in genes that have a certain behaviour - in this case, the
# behaviour is: "cyclically expressed". How do we find them?

# Can we just inspect and choose?

# TASK:
#  - Write a function that will plot ten randomly chosen expression profiles
#   from the data.

plot(ygProfiles[123, ], type="l")

plotProf <- function(x, n, col = "#00000") {
    # plot n randomly chosen expression profiles from expression matrix x
    iRows <- sample(1:nrow(x), n, replace = FALSE)

    plot(x[iRows[1], ],
         type="n",
         ylim = range(x[iRows, ]))

    for (idx in iRows) {
        points(x[idx, ], type = "l", col = col)
    }

    return()
}

plotProf(ygProfiles, 10)
plotProf(ygProfiles, 200,  "#0088DD33")
plotProf(ygProfiles, 1000, "#0088DD11")

# Inspect and choose? There are too many genes to work with, to use manual
# selection ... and most genes do not have cyclical expression patterns.

# How can we find "interesting" gene expression profiles?

# We need to define what we consider "interesting".

# For example, we can define a model for our expression profiles, and then
# search for genes that correlate with this model. Here is a model profile (with
# made-up parameters):

t <- seq(0, 120, by = 5)            # our timepoints
myModel <- cos((t / 60) * 2 *pi)    # two cycles
plot(t, myModel,                    # plot the values to visualize
     col="#CC0000", type = "l",
     xlab = "t (min.)", ylab = "expression log-ratio")

# We can easily calculate the correlation of a real expression profile with our
# synthetic profile - for example:
iRow <- 814
points(t, ygProfiles[iRow, ], type="b")
cor(ygProfiles[iRow, ], myModel)
# ... or
iRow <- 5571
points(t, ygProfiles[iRow, ], type="b", col="#00CC00")
cor(ygProfiles[iRow, ], myModel)

# Note that the _amplitude_ of our synthetic profile does not matter - the
# coefficient of correlation is high if one set of points can be transformed
# into the other with a linear equation - no matter what the coefficients of the
# equation are.

# Let's calculate correlations for all profiles ...

myCor <- numeric(nrow(ygProfiles))                 # an empty vector for results
for (iRow in 1:nrow(ygProfiles)) {                 # for each gene ...
    myCor[iRow] <- cor(ygProfiles[iRow, ], myModel)  # store correlation
}
# That's quite fast. What do we get?
hist(myCor, col = "#FFF3DE")

# Some correlations are very high. Let's plot the 10 highest correlations, and
# the 10 highest anticorrelations.

sel1 <- order(myCor, decreasing = TRUE)[1:10]
sel2 <- order(myCor, decreasing = FALSE)[1:10]


# ... list what genes these are ...
ygData[sel1, c("stdName", "alias")]
ygData[sel2, c("stdName", "alias")]

# ... and plot their expression profiles
plot(t, rep(0, 25), type = "n",
     ylim = c(-0.6, 0.6),
     xlab = "time", ylab = "log-ratio expression")

for (i in 1:10) {
    points(t, ygProfiles[sel1[i], ], type = "l", col = "#82C9B6")
    points(t, ygProfiles[sel2[i], ], type = "l", col = "#C27E7E")
}

# What do we learn? Model based correlations are easy to compute, and will of
# course find profiles that correlate with the models. But a linear correlation
# can be high even if the absolute values are not high - as long as they vary
# linearly with the model. Moreover, we would not find genes that are
# phase-shifted, because these have near-zero correlations. Consider:

myShiftedModel <- cos(((t + 15)/ 60) * 2 *pi)  # shift by 15 minutes
plot(t, myModel, col="#CC0000", type = "l",
     xlab = "t (min.)", ylab = "expression log-ratio")
points(t, myShiftedModel, type="l", col="#AACCFF")

# Apply to an expression profile I picked:
iRow <- 748
points(t, 5 * ygProfiles[iRow, ], type = "b", col = "black")

# calculate correlations:
cor(ygProfiles[iRow, ], myModel)           # No significant correlation
cor(ygProfiles[iRow, ], myShiftedModel)    # Excellent correlation

# Even though gene 748 (YDL030W, PRP9) is cyclically expressed, it only has a
# measly -0.03 coefficient of correlation with the our original model, whereas
# shifting the model profile by 15 minutes gives a high correlation of 0.81.
# However - we don't know what the best shift should be, and indeed, whether our
# assumed period of 60 minutes is even correct. In order to more generally find
# genes of interest, we need to consider fitting the data to a model with
# adjustable parameters. That will allow us to work with the information that
# we can distill from our profiles, such as: quality of fit, amplitude, phase
# etc. This will take us to the domain of curve fitting.

# Expand:  "real" cell-cycle genes:
s <- c("Cdc14", "Mbp1", "Swi6", "Swi4", "Whi5", "Cln1", "Cln2", "Cln3")



# TBC ...

# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
