# enrichment.R
#
# Purpose:  Demonstrate discovery of significantly enriched categories.
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

# Dynamic tree cut balances class sizes in hierarchical clustering
if (!requireNamespace("dynamicTreeCut", quietly=TRUE)) {
    install.packages("dynamicTreeCut")
}


# Yeast GO annotations are contained in a Bioconductor annotation package:
if (! requireNamespace("org.Sc.sgd.db", quietly = TRUE)) {
    BiocManager::install("org.Sc.sgd.db")
}

# Bioconductor annotation packages must be loaded to work stably:
library(org.Sc.sgd.db)
# (For details, see BIN_FUNC-Semantic_similarity in the ABC-units package.)


# GOSim in the Bioconductor project computes enrichment. It also loads topGO
# as a dependency.
if (! requireNamespace("GOSim", quietly = TRUE)) {
    BiocManager::install("GOSim")
}

library(GOSim)
library(topGO)

# GOSim loads human annotations initially, later we will override these
# with yeast annotations.

GOSim::setEvidenceLevel()        # takes about 15 sec. to process


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


load("nlsParams.RData")
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




# ====  PROCESS  ===============================================================

# =    1  RECAP  ===============================================================

# We are working with yeast expression data and previously discussed how to use
# non-linear least squares fitting to get the parameters of an expression
# profile, and we used clustering on the expression values. Here is a
# selection of cyclically expressed genes:

selCC <- which(nlsParams$A > 0.1 &
               nlsParams$cor > 0.82 &
               nlsParams$f > 0.9 &
               nlsParams$f < 1.1 &
               nlsParams$k < 0.01 &
               nlsParams$k > -0.01 &
               ygData$sysName != "")

length(selCC)   # selected 220 genes

# Calculate the time of the first positive peak from curve-fitting
# parameters for each selected gene, store in refPeaks ...
N <- length(selCC)
refPeaks <- numeric(N)
for (i in 1:N) {
    refPeaks[i] <- getReferencePeak(selCC[i])
}

# reorder selection by expression peak
names(refPeaks) <- selCC
refPeaks <- refPeaks[order(refPeaks, decreasing = FALSE)]
selCC <- as.integer(names(refPeaks))

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


peakCols <- colorRampPalette(c("#1cacf3",
                               "#1cacf3",
                               "#0f8a94",
                               "#000000",
                               "#000000",
                               "#9f388a",
                               "#de2f5d",
                               "#de2f5d"))(256)

# ... plot as image ...
image(x = (0:(ncol(exVals) - 1)) / (ncol(exVals) - 1), # map time to [0,1]
      y = (0:(nrow(exVals) - 1)) / (nrow(exVals) - 1), # map row index to [0,1]
      z = t(exVals[nrow(exVals):1, ]),                 # rows descending
      col = peakCols,
      xaxt = "n", yaxt = "n", xlab = "time (min.)",
      ylab= "genes, ordered by time of first positive peak (top to bottom)",
      main = "Cell cycle progression")
yTicks <- seq(1, 0, length.out = length(sel))
axis(1, at = seq(0, 1, length.out = 25),
     labels = seq(0, 120, by = 5), cex.axis = 0.5)
abline(v = 0.5 + (1/50), col = "white", lwd = 0.5)

# We can look at this image, but we will surely next be interested to see if
# we can retrieve genes according to the structure in our expression profiles.

# The 10 minute time gives a good separation of colors mapped to the phase
# when each gene peaks in the cell cycle, we add the color value to our
# GOI table.
myGOI$col <- numeric(nrow(myGOI))
xI <- min(exVals[ , "t.10"])
xA <- max(exVals[ , "t.10"])
for (i in seq_along(myGOI$col)) {
    myGOI$col[i] <- peakCols[round((255*(exVals[i, "t.10"]-xI)) / (xA-xI)) + 1]
}

# Next we applied hierarchical clustering to pick genes with similar expression
# profiles:

# calculate a distance matrix
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

# https://stackoverflow.com/questions/18802519/label-and-color-leaf-dendrogram

labelCol <- function(nod) {
    if (is.leaf(nod)) {
#        browser()
        lbl <- attr(nod, "label")                             # fetch label
        idx <- which(rownames(exVals) == lbl)                 # get index
        attr(nod,"nodePar") <- list(lab.col=myGOI$col[idx])   # set label colour
    }
    return(nod)
}

dend <- dendrapply(as.dendrogram(hclust(myDist)), labelCol)

plot(dend)

# The dendrogram shows that similar profiles can be discovered and grouped. We
# then used a dynmaic tree-cutting algorithm to separate the tree into clusters.

dynClass <- dynamicTreeCut::cutreeDynamic(dendro = hclust(myDist),
                                               distM = as.matrix(myDist),
                                               cutHeight=100)
table(dynClass)

# Add the classes to the myGOI dataframe

myGOI$class <- dynClass

nClass <- length(table(dynClass))   # number of classes
mPeak <- numeric(nClass) # vector to hold the mean peak-time for each class
for (i in seq_along(mPeak)) {
    sel <- which(dynClass == i)
    mPeak[i] <- median(myGOI$peak[sel])
}

(oPeak <- order(mPeak))

opar <- par(mar = c(5, 4, 0.5, 0.5))
plot(0, type = "n",
     xlim = c(1, 25), xlab = "time (min.)",
     ylim = c(-3, 35), ylab = "log expression ratio",
     axes = FALSE)

axis(1, at = seq(1, 25, by = 2),
     labels = seq(0, 120, by = 10), cex.axis = 0.6)

axis(2, at = c(seq(-3, 3, by = 1) + 28,
               seq(-3, 3, by = 1) + 21,
               seq(-3, 3, by = 1) + 14,
               seq(-3, 3, by = 1) +  7,
               seq(-3, 3, by = 1) +  0),
     labels = rep(c("", "-2", "", "0", "", "2", ""), 5), cex.axis = 0.5)
abline(h = seq(0, 28, length.out = 5), lwd = 0.5, col = "#777777")

for (i in seq_along(oPeak)) {
    iCol <- (255 * ((i - 1) / (length(oPeak) - 1))) + 1
    sel <- which(myGOI$class == oPeak[i])
    for (j in seq_along(sel)) {
        points(exVals[sel[j], ] + (35 - (i * 7)),
               type = "l",
               col  = peakCols[iCol])
    }
    lines(c(mPeak[oPeak[i]] / 5, mPeak[oPeak[i]] / 5),
          c(-4, 29),
          lwd = 1.5,
          col = peakCols[iCol])
}
par(opar)

# Now the question for enrichment is: can we attribute shared function (via GO
# annotations) to these sets, and discover what (if anything) they have in
# common?



# =    4 Enrichment  ===========================================================

# Enrichment is a property of a set of categorical values.  catgorical values
# are very frequent in bioinformatics and include: types of amino acids; silent,
# truncating, or missense mutations; SNPS in the coding region of a gene;
# presence of a gene in a species; GO annotations in a set of genes, ... and
# much, much more.

# Enrichment answers
# the question whether a certain category appears more (or less) frequent in
# a set then we would expect it to.

# Consider this example. 102 red squares in 1024 squares.

nRed <- 102
set.seed(112358)
X <- sample(c(rep(0, nRed), rep(1, 1024 - nRed)))
image(matrix(X, nrow = 32), axes = FALSE)
abline(v = seq(-1/64, 1 + (1/64), length.out = 33))
abline(h = seq(-1/64, 1 + (1/64), length.out = 33))

# Now I take a random sample of 32 squares, and ask: how many red squares will
# I see?

x1 <- sample(X, 32, replace = FALSE); sum(x1 == 0)

# Let's repeat that a lot and look at the distribution:
N <- 100000
nPicks <- numeric(N)
for (i in 1:N) {
    nPicks[i] <- sum(sample(X, 32, replace = FALSE) == 0)
}

hist(nPicks, breaks = seq(-0.5, 20.5, by = 1), col = "#BB000066")

table(nPicks)
# This is a classic problem of combinatorics and modelled by a hypergeometric
# distribution: p(x) = choose(m, x) choose(n, k-x) / choose(m+n, k)
#
# In our case: x is observed to be between 0 and 12, m is the number of
# red squares (positive events), n is the number of yellow squares
# (negative events) and k is the number of samples, we had 32
?Hypergeometric

dhyper(x = 0, m = nRed, n = 1024 - nRed, k = 32)
dhyper(x = 1, m = nRed, n = 1024 - nRed, k = 32)

# let's plot all of the densities:
for (i in 0:20) {
    p <- dhyper(x = i, m = nRed, n = 1024 - nRed, k = 32) * N
    segments(i-0.5, p, i+0.5, p, lwd = 3, col = "#BB0088")
}

# That's a really good fit! Note: when m+n is "large", the binomial coefficients
# may not be exact since their evaluation involves factorials. In that case
# , you can use a poisson distribution instead. A nice discussion is here ...
# https://www.mathxplain.com/probability-theory/discrete-and-continuous-distributions/binomial-poisson-and-hypergeometric ...
# and many other tutorials on the Web.

# With this, you can state exactly, if you have picked 15 red squares, what was
# the probability of this happeneing by random chance.



# =    5  GO Term Enrichment in Gene Sets  =====================================

# However, when we apply this to biological questions, there is a problem: this
# assumes that our positive events are randomly distributed, and we choose
# randomly among events. This is almost always a poor assumption for biological
# data. There are many correlations and redundancies that make genes, sequences,
# species, annotations etc. not INDEPENDENT in their properties. In particular,
# GO term annotations live on a graph (a DAG) with very non-random
# ancestor/child relationships. Alexa et al. (2006) take the GO term dependency
# structure into account with the topGO package which is widely used in GO
# enrichment analysis.


# Choose GOterms to use
GOSim::setEvidenceLevel(evidences = "all",
                        organism = org.Sc.sgdORGANISM,
                        gomap = org.Sc.sgdGO)

# Use Biological Process ontology
GOSim::setOntology("BP", loadIC = FALSE)

# confirm that we loaded the correct ontology
head(get("gomap", envir = GOSimEnv))


# Let's define a gene set: class 2
iClass <- 2
mySet <- myGOI$ID[myGOI$class == iClass]

allGenes <- ygData$sysName[ygData$sysName != ""]

(myEnr <- GOSim::GOenrichment(mySet, allGenes))



listEnrichments <- function(x,
                            iClass,
                            GOI,
                            allGenes = ygData$sysName[ygData$sysName != ""]) {
    # list data from GOSim::GOenrichment() results for "class" from
    # GOI (Genes of Interest) dataframe with a background of
    # "allGenes"
    oPvalues <- order(x$p.values)
    classIDs <- GOI$ID[GOI$class == iClass]

    for (i in seq_along(oPvalues)) {
        thisGO <- x[["GOTerms"]]$go_id[oPvalues[i]]
        nGOgenes <- length(x$genes[[thisGO]])
        nThisGOinClass <- length(intersect(classIDs, x$genes[[thisGO]]))
        cat(sprintf("%s: p = %5.4f %2d/%2d %s - %s",
                    thisGO,
                    x$p.values[oPvalues[i]],
                    nThisGOinClass,
                    length(classIDs),
                    sprintf("( %3d/%4d ) ",
                            nGOgenes,
                            length(allGenes)),
                    x[["GOTerms"]]$Term[oPvalues[i]]))
        cat("\n")
    }
}

listEnrichments(myEnr, iClass, myGOI)


for (i in seq_along(oPeak)) {
    iClass <- as.numeric(names(table(dynClass)[oPeak[i]]))
    cat(sprintf("\n\nClass: %d mPeak:%5.2f min.\n",
                iClass, mPeak[oPeak[i]]))
    cat("=========================\n")

    mySet <- myGOI$ID[myGOI$class == iClass]
    allGenes <- ygData$sysName[ygData$sysName != ""]
    tmp <- file(tempfile(), open = "wt")

    sink(tmp, type = "message")
    myEnr <- GOSim::GOenrichment(mySet, allGenes)
    sink()

    listEnrichments(myEnr, iClass, myGOI)
}



# Note: this is an active area of research, and a literature search will
# readily find newer (and perhaps improved) approaches.


# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
