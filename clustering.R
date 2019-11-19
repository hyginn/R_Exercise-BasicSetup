# clustering.R
#
# Purpose:  Demonstrate clustering.
# Version:  0.1
# Version history:
#           0.1 Initial code
#
# Date:     2019-11
# Author:   boris.steipe@utoronto.ca
# License:  CC-BY
#
# ToDo:
#
# Notes:
#
# ==============================================================================

# WARNING: SIDE EFFECTS
# Executing this script will execute code it contains.

# ====  PARAMETERS  ============================================================


# ====  PACKAGES  ==============================================================

# ====  DATA  ==================================================================

# This demo code again uses a real-world data set of yeast gene expression data.
# The data is derived from the GSE3635 expression set on GEO:
#     https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE3635

load("ygProfiles.RData")   # yeast gene expression profiles

# loads the data frame "yProfiles" with 6228 rows and 25 columns, each column
# contains log expression changes and the values have been measured in time
# intervals of 5 minutes from t = 0 to t = 120 min.

# Annotations for this data have been compiled for each row of the expression
# profiles, from data I downloaded from sgd. The dataset matches the expression
# profiles row by row so we can easily use it to anotate rows of interest.


load("ygData.RData")       # yeast gene data

# loads the data frame "yGData" with the following columns:
#   SGD          (chr):  SGD database ID (e.g. S000002214)
#   sysName      (chr):  Systematic name (e.g. YDL056W)
#   stdName      (chr):  Standard name in all-caps (e.g. MBP1)
#   alias        (chr):  Common other names (e.g. "transcription factor MBP1")
#   description  (chr):  Free-text description of gene function.


load(file = "nlsParams.RData")
# loads the data frame "nlsParams" with the following columns:
#   Five curve fitting parameters:
#     A     (numeric): Amplitude of fitted function
#     phi   (numeric): Phase of fitted function
#     f     (numeric): Frequency of fitted function
#     k     (numeric): Exponential decay term  exp(-k * t)
#     B     (numeric): Vertical offset
#   cor   (numeric): Correlation of data and predicted values
#   call  (chr):     The fitting function that was used



# ====  FUNCTIONS  =============================================================


cycEx2 <- function(t, A, phi, f, k, B) {
    # cosine function with amplitude A, phase phi (in minutes), and
    # frequency f, scaled for one full cycle corresponding to 60 min,
    # with damping term exp(-k * t) and vertical offset B
    ( (exp(-k * t) *    # damping term
           (A *            # scaling term
                ( cos( ( ((t - phi) / 60) * 2 * pi )  * f) )
           )
    )
    ) + B               # vertical offset
}


bestFit <- function(y) {
    # Tries different parameter settings for nls() and returns the best
    # fit object.
    t <- seq(0, 120, length.out = length(y))
    myPars <- data.frame(A   = c(0.1, 0.1, 0.1, 0.1,  0.03,  0.03),
                         phi = c(0.1,  10,  30,  40,   0.1,   0.1),
                         f   = c(1.0, 1.0, 1.0, 1.0, 0.618, 1.618))
    nlsFits <- list()
    nlsCors <- numeric(nrow(myPars))
    myLower <- c(0.01,  -240, 0.01, -5, -2)
    myUpper <- c(2.00,   240, 20.0,  5,  2)
    names(myLower) <- c("A", "phi", "f", "k", "B")
    names(myUpper) <- c("A", "phi", "f", "k", "B")
    for (i in 1:nrow(myPars)) {
        myFit <- list()
        try(myFit <- nls(y ~ cycEx2(t, A, phi, f, k, B),
                         start = list(A = myPars$A[i],
                                      phi = myPars$phi[i],
                                      f = myPars$f[i],
                                      k = 0.01,
                                      B = 0.01),
                         algorithm = "port",
                         lower = myLower,
                         upper = myUpper),
            silent = TRUE)
        if (length(myFit) > 0) {
            nlsFits[[i]] <- myFit
            nlsCors[i] <- cor(y, predict(myFit))
        }
    }
    if (sum(nlsCors) != 0) {  # some fit converged
        best <- which(nlsCors == max(abs(nlsCors)))[1]
        return(nlsFits[[best]])
    } else {
        return(invisible(NULL))
    }
}


getReferencePeak <- function(iRow) {
    # Finds the first non-negative peak of the fitted function.
    # We don't know in which period of the function the phase-shift of the curve
    # fit has converged, so we calculate peaks, reducing the period counter i
    # until we find a peak that is < 0. Then we increase the period counter
    # until the peak becomes > 0. This is the reference peak. Note that this
    # peak is not necessarily the maximum of the function - if the fit is
    # significantly damped, the peak will lie to the right of the maximum. The
    # peak is however a maximum of the ideal, underlying periodic function.
    if (nlsParams$f[iRow] == 0) {
        return(NA)
    }
    i <- 0
    if (nlsParams$phi[iRow] >= 0) {
        # find first negative peak
        rPeak <- 1  # arbitrary positive
        while (rPeak >= 0) {
            i <- i - 1   # decrement period counter
            rPeak <- ( (i * 60) / nlsParams$f[iRow]) + nlsParams$phi[iRow]
        }
    }
    # find first non-negative peak
    rPeak <- -1
    while (rPeak < 0) {
        i <- i + 1      # increment period counter
        rPeak <- ( (i * 60) / nlsParams$f[iRow]) + nlsParams$phi[iRow]
    }

    return(rPeak)
}


markPeak <- function(iRow, myCol = "#1cacf3") {
    # mark the "Reference Peak" in a plot
    x <- getReferencePeak(iRow)
    if (is.na(x)) {
        return()  # No triangle to mark, had not converged
    }
    y <- cycEx2(x,
                nlsParams$A[iRow],
                nlsParams$phi[iRow],
                nlsParams$f[iRow],
                nlsParams$k[iRow],
                nlsParams$B[iRow])
    lim <- par("usr") # axis ranges of current plot
    dx <- abs(lim[1] - lim[2]) / 60
    dy <- abs(lim[3] - lim[4]) / 15
    polygon(c(x, x + dx, x - dx, x),
            c(y, y - dy, y - dy, y),
            border = myCol,
            lwd = 0.5)
}

checkFit <- function(iRow, fit) {
    t <- seq(0, 120, by = 5)
    y <- ygProfiles[iRow, ]
    plot(t, ygProfiles[iRow, ], col = "black", type = "b",
         xlab = "t (min.)", ylab = "expression log-ratio",
         main = sprintf("%d: %s (%s)",
                        iRow,
                        ygData$sysName[iRow],
                        ygData$stdName[iRow]))
    abline(h =  0, col = "#DDEEFF")
    abline(v = 60, col = "#DDEEFF")
    if (is.null(fit)) {
        return()
    }
    mtext(sprintf("Parameters: cor: %5.3f, %s",
                  cor(y, predict(fit)),
                  paste(names(coef(fit)),
                        sprintf("%5.3f", coef(fit))
                        , sep = ": ", collapse = ", ")),
          col = "#DD99CC", side = 1, line = 4)
    t2 <- seq(0, 120, by = 1)
    y2 <- data.frame(t = t2)
    points(t2, predict(fit, newdata = y2), col = "#DD99CC", type = "l")
    markPeak(iRow)
}



# ====  PROCESS  ===============================================================

# =    1  RECAP  ===============================================================

# We are working with yeast expression data and have discusse how to use
# non-linear least squares fitting to get the parameters of an expression
# profile, for example ...

iRow <- which(ygData$stdName == "MBP1")
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

# ... and how to display genes in parameter space to obtain a holistic
# overview of the cell cycle:
selCC <- which(nlsParams$A > 0.1 &
                   nlsParams$cor > 0.82 &
                   nlsParams$f > 0.9 &
                   nlsParams$f < 1.1 &
                   nlsParams$k < 0.01 &
                   nlsParams$k > -0.01)

# calculate the first positive peak
N <- length(selCC)
refPeaks <- numeric(N)
for (i in 1:N) {
    refPeaks[i] <- getReferencePeak(selCC[i])
}

names(refPeaks) <- selCC
refPeaks <- refPeaks[order(refPeaks, decreasing = FALSE)]
selCC <- as.integer(names(refPeaks)

hist(refPeaks,
     breaks = seq(0, 64, by = 2),
     col = colorRampPalette(c("#00FF66",
                              "#888888",
                              "#FF4400",
                              "#888888",
                              "#00FF66"))(33) )

# Let's create a dataframe with those "Genes Of Interest"
myGOI <- data.frame(iRow = selCC,
                    ID = ygData$sysName[selCC],
                    name = ygData$stdName[selCC],
                    A = nlsParams$A[selCC],
                    phi = nlsParams$phi[selCC],
                    f = nlsParams$f[selCC],
                    k = nlsParams$k[selCC],
                    B = nlsParams$B[selCC],
                    cor = nlsParams$cor[selCC],
                    peak = refPeaks,
                    stringsAsFactors = FALSE)

# ... and plot the scaled expression profiles as an image():
# ... initialize a matrix
N <- nrow(myGOI)
exVals <- matrix(numeric(ncol(ygProfiles) * N),
                 nrow = N, ncol = ncol(ygProfiles))

for (iRow in 1:N) {
    exVals[iRow, ] <- as.vector(scale(ygProfiles[myGOI$i[iRow], ]))
}
rownames(exVals) <- myGOI$name
sel <- which(rownames(exVals) == "")
rownames(exVals)[sel] <- myGOI$ID[sel]

colnames(exVals) <- colnames(ygProfiles)


# ... plot as image (for simplicity of the plot, only every third gene)
sel <- seq(1, N, by = 3)
image(exVals[sel, ],
      col = colorRampPalette(c("#1cacf3",
                               "#1cacf3",
                               "#0f8a94",
                               "#000000",
                               "#000000",
                               "#9f388a",
                               "#de2f5d",
                               "#de2f5d"))(256),
      xaxt = "n", yaxt = "n", ylab = "time (min.)", xlab= "rank of phase",
      main = "Cell cycle progression")
xTicks <- seq(1, 0, length.out = length(sel))
axis(1, at = xTicks, labels = rownames(exVals)[sel],
     cex.axis = 0.3, las = 1, lwd.ticks =  0.5)
axis(2, at = seq(0, 1, length.out = 25),
     labels = seq(0, 120, by = 5), cex.axis = 0.5)
abline(h = 0.5 + (1/50), col = "white", lwd = 0.5)

# We can look at this image, but we will surely next be interested in seeing if
# we can identify structure in our expression profiles.

# =    2  CLUSTERING  ==========================================================

# It's convenient for clustering to use data objects that are simple numeric
# matrices. We have prepared such a matrix with the exVals matrix: it contains
# the individual expression values, the column names are the times, and the
# rownames are the standard names.
str(exVals)

# =    2.1  HEATMAPS  ==========================================================

# Heatmaps are a staple of gene expression analysis.
# You can tweak many of the parameters, but for a first look
# we'll just heatmap the data with default parameters.

# This is a standard view that can be applied to all manners
# of multidimensional data, not just genes.
heatmap(exVals)

# TODO:
# TASK: In biology we are usually looking at red/black/green heatmaps ...
# what are these colours? Can we colour our data like that?
#
# Hint: https://www.r-graph-gallery.com/215-the-heatmap-function/
rgCols <- colorRampPalette(c("#EE0000",
                             "#000000",
                             "#00EE00"))(256)
x <- heatmap(exVals, col = rgCols)

# Study the heatmap, and consider what it tells you. What do the dendrograms
# along the x- and y- axis mean?

# The return value contains ordered row and column indices:

str(x)


# For example, there seem to be genes that are low around t.40, high at t.80 ...
set1 <- x$rowInd[15:29]
# ... and there are genes which are high at t.65 and low at t.35:
set2 <- x$rowInd[38:44]
# ... and the genes at the bottom appear to be the inverse of set1:
set3 <- x$rowInd[215:223]

# We can use a "parallel coordinates" plot - matplot()
# to look at the actual expression levels. Note that
# matplot expects the values column-wise ordered, thus
# we have to transpose - t() - the data!
matplot(t(exVals[set1, ]),
        type="l", lwd=2, col="skyblue", lty=1,
        xlab="conditions", ylab="log expression value")

# Then we can use lines() to superimpose the genes for set2.
# No transpose here :-)
for (i in 1:length(set2)) {
    lines(exVals[set2[i], ], type="l", lwd=2, col="firebrick")
}

# ... and set3:
for (i in 1:length(set3)) {
    lines(exVals[set3[i], ], type="l", lwd=2, col="plum")
}

# Indeed, these genes - visibly different in the heatmap
# have similar expression profiles and are different between groups


# =    3  HIERARCHICAL CLUSTERING  =============================================


# Hierarchical clustering is probably the most basic technique.
# The dendrograms on the rows and columns of the heatmap
# were created by hierarchical clustering.

# For hierarchical clustering, first we need to produce
# a distance table. There are many ways to define distances
# let's just go with the default: "Euclidian distance".
distDat <-dist(exVals)

# Then we use the clustering distance matrix to produce a
# dendrogram in which the most similar genes are connected, and then
# similar genes or connected groups are added. There are
# several ways to define "most-similar", lets just go with the
# default for now: "complete linkage" hierarchical clustering
hc <- hclust(distDat, cex = 0.5)

plot(hc)

# Not bad. But do note that  distance as well as clustering
# method matter, and there is not really a "best" way that
# works for all data. You'll need to explore: what you are looking for
# is a distance metric that gives the clearest block structure in the heatmap.

# ==   3.1  Explorig distance metrics  =========================================

dEu <- function(x) dist(x, method="euclidian")
heatmap(exVals, distfun = dEu, col = rgCols)

dCan <- function(x) dist(x, method="canberra")
heatmap(exVals, distfun = dCan, col = rgCols)

dMax <- function(x) dist(x, method="maximum")
heatmap(exVals, distfun = dMax, col = rgCols)

dMink <- function(x) dist(x, method="minkowski")
heatmap(exVals, distfun = dMink, col = rgCols)

# You are not confined to the default distance functions, it
# is quite straightforward to define your own, for example
# using correlation properties. Here is a distance function
# defined as 1 - abs(pearson correlation)...

dCorAbs <- function(x) as.dist(1 - abs(cor(t(x))))
heatmap(exVals, distfun = dCorAbs, col = rgCols)

# ... which is useful if you are interested in similarity of shape, regardless
# of the absolute value, or ...

dCor <- function(x) as.dist(2 - cor(t(x)))
heatmap(exVals, distfun = dCor, col = rgCols)

# ... calculating the entire range of the possible correlations, i.e.
# differentiating between correlated and anticorrelated samples.

# In the case of our dataset it does indeed seem that the
# dCor function (2 minus correlation) gives us the clearest block-structure.

# Let's use the function to produce a distance matrix:
# A distance matrix is simply a square matrix of elements, which contains the
# distance between them in the cells

N <- nrow(exVals)
myDist <- matrix(numeric(N*N), nrow = N)
rownames(myDist) <- rownames(exVals)
colnames(myDist) <- rownames(exVals)

for (i in 1:N) {
    for (j in i:N){
        d <- 2 - cor(exVals[i, ], exVals[j, ])
        myDist[i, j] <- d
        myDist[j, i] <- d
    }
}
myDist <- as.dist(myDist)
hc <- hclust(myDist)
plot(hc)

# ==   3.2  Clusters From Dendrograms  =========================================

# To get clusters from a dendrogram, we need to "cut" it at some
# level. The tree then falls apart into sub-trees and each of these
# is one "cluster"...

# Draw rectangles at different cut-levels, to give the desired number
# of clusters.
rect.hclust(hc, k = 2)
rect.hclust(hc, k = 5)
rect.hclust(hc, k = 10)
rect.hclust(hc, k = 20)
rect.hclust(hc, k = 50)

# Now retrieve the actual indices and use them to generate
# parallel coordinate plots.

class <- cutree(hc, k = 10)

# Explain the output...
class

# The table() function allows us to count the number of
# occurences in each class ...
table(class)
sort(table(class), decreasing = TRUE)

# Let's plot the four largest classes (in parallel, into the same window)
# Look at this carefully. See how the selection statement on the object "class"
# generates a logical vector: TRUE in all rows for which the statement is true,
# and how this is used to select the rows of exVals that we want to plot ...

oPar <- par(mfrow=c(2,2))
matplot(t(exVals[class==1,]), type="l", xlab="time", ylab="log expression value")
matplot(t(exVals[class==6,]), type="l", xlab="time", ylab="log expression value")
matplot(t(exVals[class==5,]), type="l", xlab="time", ylab="log expression value")
matplot(t(exVals[class==8,]), type="l", xlab="time", ylab="log expression value")
par(oPar)


# As an alternative, try Wards- linkage clustering (and read up on the
# options: single-, complete- and average-linkage clustering)
hc.ward <-hclust(distDat, method = "ward.D", members=NULL)

plot(hc.ward)


# draw rectangles
rect.hclust(hc.ward,k=9)

# This looks reasonable ...
# Now retrieve the actual indices and use them to generate
# paralell coordinate plots.

class.ward<-cutree(hc.ward, k = 9)
sort(table(class.ward))

# get some nice colors
if (!require(RColorBrewer, quietly=TRUE)) {
    install.packages("RColorBrewer")
    library(RColorBrewer)
}

# what spectra are there in the package .. ?
display.brewer.all()

niceCols <- brewer.pal(9, "Paired")

oPar <- par(mfrow=c(3,3))
for (i in 1:9) {
    matplot(t(exVals[class == i,]),
            type="l", col=niceCols[i],
            xlab="time",ylab="log expression value")
}
par(oPar)


# ===   3.2.1  Setting Cut-Levels Dynamically

# While this may be somewhat satisfactory, there is no guarantee that the clusters
# are naturally meaningfully separated if we cut them at the same level. This is a general problem with clustering methods that
# fix the number of cluster centres either directly as in Kmeans, or indirectly by cutting trees at a fixed level. It is also
# a problem with the data, where differences in absolute values might
# override separation into clusters that might better be defined in terms
# of relative values. Rather than "cutting" at a fixed level, we could
# adjust the level dynamically according to some objective function that
# may give us "better" clusters.

# Here is a package that adresses the dynamic range problem.
# Read about it here:
# http://cran.r-project.org/web/packages/dynamicTreeCut/dynamicTreeCut.pdf
if (!requireNamespace("dynamicTreeCut", quietly=TRUE)) {
    install.packages("dynamicTreeCut")
}

hc <- hclust(myDist)   # recreate hc
class.dynamic <- dynamicTreeCut::cutreeDynamic(dendro = hc,
                                               distM = as.matrix(myDist),
                                               cutHeight=100)
table(class.dynamic)

niceCols <- brewer.pal(7, "Spectral")


oPar <- par(mfrow=c(3,3))
for (i in 1:7) {
    matplot(t(exVals[class.dynamic == i,]),
            type="l",
            col=niceCols[i],
            xlab="time",
            ylab="log expression value")
}
par(oPar)

# Note that the number of members in each class is now much more homogenous,
# while the clusters do not seem to have become more noisy - thus the dynamic cut level has worked well for our case.



# =    4  PARTITIONING CLUSTERING  =============================================


# ==   4.1  K-means  ===========================================================

# K-means clusters by assigning elements to a fixed
# number of cluster centres, so that similarity
# within a cluster is maximized.

?kmeans

k <- 4
cl <- kmeans(exVals, k)

niceCols <- brewer.pal(k, "Spectral")

plot(exVals[ ,1], exVals[ ,4], col = niceCols[cl$cluster])
points(cl$centers[ ,1],
       cl$centers[ ,4],
       col = niceCols[1:k], pch = 19, cex = 3)

# But: be aware ...
# ... K-means does not guarantee a globally optimal solution,
# merely a locally converged one.

# ==   4.2  K-medoids  =========================================================

# load library "cluster" for K-medoid partitioning
if (!requireNamespace("cluster", quietly=TRUE)) {
    install.packages("cluster")
}

set.seed(112358)
k <- 4
cl <- cluster::pam(exVals, 4)
plot(exVals[, 1],exVals[, 4], col=niceCols[cl$cluster])
plot(cl) # shows boundary and silhouette plots


# =    5  AFFINITY PROPAGATION CLUSTERING  =====================================


# Based on B. J. Frey and D. Dueck. Clustering by
# passing messages between data points.
# Science, 315(5814):972–976, 2007

if (!requireNamespace("apcluster", quietly=TRUE)) {
    install.packages("apcluster")
}
library(apcluster)  # needs to override heatmap() and cuttree()

apRes <- apcluster::apcluster(apcluster::negDistMat(r=2), exVals)
apRes

heatmap(apRes)
length(apRes)
cutree(apRes)

oPar <- par(mfrow=c(3,3))
for (i in 1:9) {
    matplot(t(exVals[unlist(apRes[i]),]),
            type="l",xlab="time",ylab="log expression value")
}
par(oPar)



# =    6  CLUSTER QUALITY METRICS  =============================================


# So .. which method should we use?
# I don't think that there is an obvious biological
# criterium to decide. Typically we should take some
# orthogonal information (e.g. shared transcription
# factor binding site), so if functionally related
# genes end up in similar clusters, and judge our
# cluster success based on its biological predictive
# value.
#
# But we can certainly say something about whether
# our clusters look good in a mathematical sense.
if (!require(clValid, quietly=TRUE)) {
    install.packages("clValid")
    library(clValid)
}
# Package information:
#  library(help =   clValid)     # basic information
#  browseVignettes("clValid")    # available vignettes
#  data(package =  "clValid")    # available datasets

if (!require(kohonen, quietly=TRUE)) {
    install.packages("kohonen")
    library(kohonen)
}
# Package information:
#  library(help =   kohonen)     # basic information
#  browseVignettes("kohonen")    # available vignettes
#  data(package =  "kohonen")    # available datasets

if (!require(mclust, quietly=TRUE)) {
    install.packages("mclust")
    library(mclust)
}
# Package information:
#  library(help =   mclust)     # basic information
#  browseVignettes("mclust")    # available vignettes
#  data(package =  "mclust")    # available datasets

?clValid

# This is pretty nice: we _can_ use biological
# knowledge for validation...
# But for our example, we'll try internal validation
# on all available methods.

valClust <- clValid(exVals,
                    nClust = 2:20,
                    clMethods = c("hierarchical",
                                  "kmeans",
                                  "diana",
                                  "model",
                                  "sota",
                                  "pam",
                                  "clara",
                                  "agnes"),
                    validation = "internal")
summary(valClust)
plot(valClust)

vignette("clValid")

# Task:
# 1 - What appears to be the best clustering method?
# 2 - How can you actually apply it to the data?





# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
