# dimensionReduction.R
#
# Purpose:  Demonstrate dimensionality reduction.
# Version:  0.1
# Version history:
#           0.1 Initial code
#
# Date:     2019-10-15
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

if (! requireNamespace("MASS", quietly=TRUE)) {
    install.packages("MASS")
}
library(MASS)
# Package information:  library(help = MASS)

if (! requireNamespace(tsne, quietly=TRUE)) {
    install.packages("tsne")
}
library(tsne)
# Package information:  library(help = tsne)



# ====  FUNCTIONS  =============================================================
#
crabsPlot <- function(x, y, crabsData,
                      main = "",
                      xlab = "",
                      ylab = "") {
    # Purpose:
    #     Create a plot of crabs data in which individual points
    #     are colored by species, shaped by gender and scaled by
    #     the mean of morphometric measurements
    # Version:  1.1
    # Date:     2016 - 2019
    # Author:   Boris Steipe
    # History:  1.1 replace "age" by mean of measurements
    #           1.0 working version
    #
    # Parameters:
    #     x:          numeric       x-coordinates of point
    #     y:          numeric       y-coordinates of point
    #     crabsData:  data-frame ...
    #                      column    1: factors with level B and O
    #                      column    2: factors with levels M and F
    #                      column    3: index
    #                      columns 4:8: measurements
    #     main, xlab, ylab: character   plot and axis labels
    # Value:
    #     None. Creates plot as side-effect

    # 1. Create a vector of colours computed from species and sex factors.
    #    We taken the factors as integers, and they can take the values 1 or 2.
    #    We transform this into a number between 1 and 4, and use this to
    #    pick a color value from a vector of four colours. The first factor
    #    is transformed to (0, 2) , the second is (1,2), the index to get
    #    a color from is obtained by adding the two.

    colSet <- c("#33A1C9",   # colour 1: "peacock"
                "#6495ED",   # colour 2: "cornflowerblue"
                "#FF8C00",   # colour 3: "darkorange"
                "#FF7F50")   # colour 4: "coral"
    colIndex <- ((as.integer(crabsData[ , 1]) - 1) * 2) +
                  as.integer(crabsData[ , 2])
    crabCols <- colSet[colIndex]

    # 2. create a vector of plotting characters.
    #    pch 21: filled circle - F (1)
    #    pch 24: filled inverted triangle - M (2)
    pchSet <- c(21, 24)
    crabPch <- pchSet[as.integer(crabsData[ , 2])]

    # 3. create a scale vector from sMin to sMax
    sMin <- 0.5
    sMax <- 4
    s <- rowMeans(crabsData[ , 4:8])
    crabCex <- (s - min(s)) / (max(s) - min(s)) # transform to [0, 1]
    crabCex <- crabCex * ((sMax - sMin) + sMin)         # scale to [sMin, sMax]

    # 4. Plot ...
    plot(x, y,
         main = main, xlab = xlab, ylab = ylab,
         pch = crabPch,      #
         col = "#55555555",  # outline of symbol
         bg  = crabCols,     # fill color
         cex = crabCex)      # size

    return()
}





# ====  PROCESS  ===============================================================

# ==============================================================================
# =    1  Data  ================================================================
# ==============================================================================

# To discuss dimensionality reduction, we will work with a standard dataset in the field, the "crabs" data which is supplied together with the MASS package: "Morphological Measurements on Leptograpsus Crabs".

data(crabs)

head(crabs)
# Column 1 "sp"  - Two species: blue and orange crabs
# Column 2 "sex" - Female and male
# Column 3 "index"
# Column 4 - 8:
#      FL frontal lobe size (mm)
#      RW rear width (mm)
#      CL carapace length (mm)
#      CW carapace width (mm)
#      BD body depth (mm)

# The question is: after we have collected these measurements, can we identify
# species and sex of a crab from its "shape"? Or: do our crabs fall into
# distinct categories? Or: are there clusters of crabs that appear similar, and
# share features such as species and sex?

# First - working with the data: we need a convenient way to filter crabs from
# our data: using which()


# Zero order approach: histograms

hist(crabs[, 4])
hist(crabs[, 6])

# Overlay different measurements


# First order approach: regression

plot(crabs[, 4:5])
plot(crabs[, 5:6])
plot(crabs[, 4:8])

# Using a function that shapes, colours and scales ...
crabsPlot(crabs[ , 4], crabs[ , 5], crabs)


# In order to explore the data more deeply, we look for the associations
# between the measurements with PCA



# ==============================================================================
# =    2  the PCA concept  =====================================================
# ==============================================================================

# Synthetic data example
# 500 normally distributed samples each: uncorrelated
set.seed(2707)
x1 <- rnorm(500,0,1)
y1 <- rnorm(500,0,1)

# generate y2 corrleated with (dependent on) x1
y2 <- 2*x1 + y1
mean(y2)
y2 <- y2-mean(y2)
mean(y2)
sd(y2)
y2 <- y2 / sd(y2)
sd(y2)
print(sd(y2), digits=22)


# Create a lattice plot with two rows and two columns
oPar <- par(mfrow = c(2,2)) # set new and save old graphics state

# four plots ...
hist(x1)
hist(y2)
plot(x1, y1)
plot(x1, y2)

par(oPar) # restore graphics state parameters


# calculate a PCA of x1 and y2
pcaSample <- prcomp(cbind(x1,y2))

# here are the information items from the returned list of results
pcaSample
pcaSample$sdev
pcaSample$rotation
summary(pcaSample)
head(pcaSample$x)
plot(pcaSample$x, xlim=c(-5,5), ylim=c(-5,5))

# Compare the histograms before and after the rotation:
oPar <- par(mfrow = c(2,2))
hist(x1, xlim=c(-4,4), ylim=c(0,150), main="")
hist(y2, xlim=c(-4,4), ylim=c(0,150), main="")
hist(pcaSample$x[,1], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.86,0,0,0.5))
hist(pcaSample$x[,2], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.31, 0.5, 0.74, 0.5))
par(oPar) # restore graphics state parameters

# Plot the sample along the Principal Components as axes
plot(pcaSample$x[,1],pcaSample$x[,2], xlim=c(-4,4), ylim=c(-4,4))


?prcomp

# ==============================================================================
# =    2  EDA with PCA  ========================================================
# ==============================================================================

# ==   2.1  The relative importance of PCs  ====================================


# Apply principal components analysis to the five measured dimensions
head(crabs)
pcaCrabs <- prcomp(crabs[, 4:8])

plot(pcaCrabs)
summary(pcaCrabs)
str(pcaCrabs)

# Plot projections along the components into a scatterplot.
# Axes for points are scaled as values, for vectors as variance
# Default for biplot() is the first and second component.
biplot(pcaCrabs)

crabLabels <- ((as.integer(crabs[ , 1]) - 1) * 2) + as.integer(crabs[ , 2])

biplot(pcaCrabs,
       xlabs = crabLabels)

# Plot the first against the third principal component
biplot(pcaCrabs, xlabs = crabLabels, choices = c(1, 3))

# Plot the second against the third principal component
biplot(pcaCrabs, xlabs = crabLabels, choices = c(2, 3))


# ===  2.1.1  Task: identify categories of elements in a plot


crabsPlot(pcaCrabs$x[ , 1],
          pcaCrabs$x[ , 2],
          crabs)

crabsPlot(pcaCrabs$x[ , 2],
          pcaCrabs$x[ , 3],
          crabs,
          main = "Principal components 2 and 3 distinguish crabs",
          xlab = "PC2",
          ylab = "PC3")



# ======== Recreate data =======================================================

library(MASS)
data(crabs)
pcaCrabs <- prcomp(crabs[, 4:8])

# Study the code in crabsPlot.R for a more advanced solution

source("crabsPlot.R")
crabsPlot(pcaCrabs$x[,2], pcaCrabs$x[,3],
          crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
          )






# ============================================================
# =    3  t-SNE  ===============================================================
# ============================================================
# t-Stochastic Neighbour Embedding is a powerful dimension re-
# duction algorithm developed in the lab of Geoff Hinton at UofT.
#
# see: https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding
#
# Its implementation for flow cytometry - viSNE -is available
# from the Dana Pe'er lab (as Matlab code).
# http://www.c2b2.columbia.edu/danapeerlab/html/cyt.html
# It is the basis for a very powerful flow-cytometry exploration tool:
# Amir et al. (2013) Nature Biotechnology, doi:10.1038/nbt.2594
#
# Below we will try the t-SNE algorithm for exploration of
# the crabs data.

?tsne

# Apply tsne to the crabs data
# First: define a plotting function
tsnePlot <- function(x) {
    crabsPlot(x[,1], x[,2],
              crabs,
              main = "Crabs TSNE")
}

# make the run reproducible
set.seed(208)
set.seed(36)

# run tsne
tsneCrabs <- tsne(crabs[,4:8],
                  epoch_callback = tsnePlot,
                  perplexity = 100,
                  epoch = 25,
                  max_iter = 500)



# Done.


# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
