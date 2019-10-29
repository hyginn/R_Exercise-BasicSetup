# parameterSpace.R
#
# Purpose:  Demonstrate linear and non-linear regression, and how to use the
#           results to analyze data in parameter space.
# Version:  0.1
# Version history:
#           0.1 Initial code
#
# Date:     2019-10-29
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


# ====  FUNCTIONS  =============================================================

normIA <- function(x, i = -1.0, a = 1.0) {
    # normalizes values in x by linear scaling [min(x), max(x)] to [i, a]
    d <- max(x) - min(x)
    x <- (((x - min(x)) / d) * 2.0) - 1.0
    return(x)
}


pBar <- function(i, l, nCh = 50) {
    # Draw a progress bar in the console
    # i: the current iteration
    # l: the total number of iterations
    # nCh: width of the progress bar
    ticks <- round(seq(1, l-1, length.out = nCh))
    if (i < l) {
        if (any(i == ticks)) {
            p <- which(i == ticks)[1]  # use only first, in case there are ties
            p1 <- paste(rep("#", p), collapse = "")
            p2 <- paste(rep("-", nCh - p), collapse = "")
            cat(sprintf("\r|%s%s|", p1, p2))
            flush.console()
        }
    }
    else { # done
        cat("\n")
    }
}


# ====  PROCESS  ===============================================================




# =    1  LINEAR REGRESSION  ===================================================

# We discussed correlation - the analysis of data in which values defined along
# one dimension of properties are predictive - to a certain degree - for what
# the values along another dimension my be. We then used this to define a notion
# of similarity, and we explored how to find genes in a large dataset that are
# either similar to each other, or similar to a preconceived model of cyclical
# expression. Then we ran into a problem: our guesses regarding the cyclical
# model might not be very good, but the correlation depends critically on the
# model, e.g. the phase or frequency of the trigonometric function we use to
# model expression along the cell cycle. To address this, we need to consider
# how to find the best idealization for a given dataset. This is a task for
# regression, or curve fitting.

# Linear regression fits data to a linear expression: y = ax + b with parameters
# a: the slope, and b: the intercept of the function. The task is, for a given data
# set, to infer what the parameters of it's linear idealization are.

# Let's construct a synthetic sample of observations that could come from measuring
# height and weight of a human cohort. We generate random heights in an
# interval, then calculate hypothetical weights according to a simple linear
# equation. Finally we add "errors" to the weights.

# The goal of our analysis is to recover the parameters of our synthetic data.
# Note: developing your analysis for synthetic data first is good practice: if
# you can't get the values for synthetic data correct, you can't expect to get
# correct values for real data.

n <- 50
set.seed(83)
hmin <- 1.5  # shortest proband
hmax <- 2.3  # tallest
HW <- data.frame(heights = numeric(n),
                 weights = numeric(n))
# generate a column of numbers in the interval
HW$heights <- runif(n, hmin, hmax)
# generate a column of "weights" with a linear model and add some noise
HW$weights <- 40 * HW$heights + 1 +  rnorm(n, 0, 15)

plot(HW$heights, HW$weights, xlab="Height (m)", ylab="Weight (kg)")

cor(HW$heights, HW$weights) # calculate correlation

# R provides lm() (linear model) for regression analysis:
# Remember: the true parameters were weight = 40 * height + 1
?lm
lm(HW$weights ~ HW$heights)

# TASK:
# What are these numbers?
# How do they relate to our question?
# Is the estimate any good?

# plot a regression line: abline() can take its coefficients directly from the
# output of lm()
abline(lm(HW$weights ~ HW$heights), col="firebrick", lwd=2)

# calculate residuals
res <- resid(lm(HW$weights ~ HW$heights))

# calculate idealized values
fit <- fitted(lm(HW$weights ~ HW$heights))

# plot differences
segments(HW$heights, HW$weights, HW$heights, fit, col="#AA000044")

# plot and analyze residuals. If the fit is good, the correlation of the
# residuals should be close to 0
plot(fit, res)
cor(fit, res)

# Calculate and plot prediction and confidence limits.

# PREDICTION limits give boundaries on future observations, they characterize
# how well the model is expected to accommodate new data, i.e. the boundaries in
# which we would expect new observations to fall.

# CONFIDENCE limits give boundaries on models. They characterize how much we can
# trust our model parameters, in our case, they describe the limits to where a
# line thorugh the data could be drawn and still describe it "well enough".

pc <- predict(lm(HW$weights ~ HW$heights), interval = "confidence")
pp <- predict(lm(HW$weights ~ HW$heights), interval = "prediction")
head(pc)

# Now plot pp and pc limits ...

# First order the data points into a monotonously increasing sequence. Why?
# Here is a simple example: Consider some points on the x-axis, with
# corresponding y-values ...
x <- c(0.42, 1.5, 1.71, 3, 2.78, 0.21, 2.57, 2.14, 0.85,
       1.07, 0.64, 0, 1.28, 1.92, 2.35)
y <- x^2  # <<<- y = x^2  ... clear and simple

# What do we get when we plot a line along the points?
# Did you think: "a parabola"?
plot(x, y, type = "l", col = "#A0BABD")
# ... no, because the points are not ordered. To get a line that represents
# their functional relationship (like pc and pp limits) we need to order the
# points, e.g. along the x-axis:
(o <- order(x))  # remember: indices of the values from smallest to largest
# plot the points in ordered sequence:
lines( x[o], y[o], type = "l", col = "#F8007D55", lwd = 3)   # lines
points(x[o], y[o], pch=21, col = "#F87D00", bg = "#FFFFFF")  # the actual points

# We do the same thing to plot boundaries for confidence and prediction of
# our linear model: order along the x-axis
o <- order(HW$heights) # o is an index vector


myLM <- lm(HW$weights ~ HW$heights)

# compute pp, pc in sorted order
pc <- predict(lm(HW$weights[o] ~ HW$heights[o]), interval = "confidence")
pp <- predict(lm(HW$weights[o] ~ HW$heights[o]), interval = "prediction")

# Then plot
plot(HW$heights, HW$weights,
     main = "Synthetic data: human heights and weights",
     xlab = "Height (m)", ylab = "Weight (kg)",
     sub = sprintf("Linear model with intercept %3.1f and slope %3.1f",
                   myLM$coefficients[1], myLM$coefficients[2]),
     ylim = range(HW$weights, pp))
matlines(HW$heights[o], pc, lty=c(1,2,2), col="slategrey")
matlines(HW$heights[o], pp, lty=c(1,3,3), col="firebrick")

# This is the proper way to plot a linear regression: the inner boundaries (pc)
# show the possible range of reasonable models. This is the 95% "confidence"
# interval, the range in which 95% of regression lines for different samples
# from the same population should fall. The outer boundaries (pp) show the
# possible range of individual predicted values, i.e. individual samples from
# the same distribution are expected to fall within these boundaries with a
# probability of 95%. Incidentally, the value of 95% (or p == 0.95) - which is
# commonly used as the minimal criterion for "significance" in biomedical
# research - is a parameter of predict(), you could set it e.g. to 0.99 by
# specifying "level = 0.99".

# TASK: write a function that takes as input two vectors and produces a plot
#       of their linear model and parameters.

# Why is this important? It is now easy to identify "outliers"


# =    2  NON-LINEAR REGRESSION  ===============================================


# What we have done previously, is to construct a model from a periodic function
# and apply it to find genes with higly correlated (or anticorrelated)
# expression profiles.

t <- seq(0, 120, by = 5)            # our timepoints
myModel <- cos((t / 60) * 2 *pi)    # two cycles in 120 minutes
plot(t, myModel,                    # plot the values to visualize
     col="#CC0000", type = "l",
     xlab = "t (min.)", ylab = "expression log-ratio")

iRow <- 5571  # correlated
points(t, normIA(ygProfiles[iRow, ]), type="b", col="#00CC00")
cor(ygProfiles[iRow, ], myModel)
# ... or
iRow <- 814  # anticorrelated
points(t, normIA(ygProfiles[iRow, ]), type="b", col="#CC8800")
cor(ygProfiles[iRow, ], myModel)

# ... but if there is a phase shift, we won't get significant correlations even
# if the gene is actually cyclically expressed:

iRow <- 748 # correlated with a phase shift
points(t, normIA(ygProfiles[iRow, ]), type="b", col = "#0088EE")
cor(ygProfiles[iRow, ], myModel)           # No significant correlation


# Let's define a cyclical expression model, and retrieve its parameters.
#

cycEx <- function(t, A, phi, f) {
    # cosine function with amplitude A, phase phi (in minutes), and
    # frequency 1/f, scaled for one full cycle corresponding to 60 min.
    A * (cos((((t - phi) * 2 * pi) / 60) / f) )
}

# time, as usual for our profiles, is in minutes
t <- seq(0, 120, by = 5)

# What does this function look like? Let's write a small function to
# conveniently explore the effect of parameters on the function:

# ==   2.1  A utility function to plot expression profiles  ====================
plotModel <- function(t, A, phi, f, thisCol = "#CC0000", add = FALSE) {

    ex <- cycEx(t, A, phi, f)
    if (! add) {
        plot(t, ex, col = thisCol, type = "l",
             xlab = "t (min.)", ylab = "expression log-ratio",
             main = "Model",
             sub = sprintf("A: %5.3f, f: %5.3f, phi: %5.3f", A, f, phi)
        )
        abline(h =  0, col = "#DDEEFF")
        abline(v = 60, col = "#DDEEFF")
    } else {
        points(t, ex, col = thisCol, type = "l")
    }
}

# Let's explore a few parameters of our conceptual expression model cycEx():

# Varying A
plotModel(t, A =  1.0, phi = 0, f = 1.0)
plotModel(t, A =  0.5, phi = 0, f = 1.0, thisCol = "#DD99CC", add = TRUE)
plotModel(t, A = -1.0, phi = 0, f = 1.0, thisCol = "#FFDDEE", add = TRUE)

# Varying 1/f
plotModel(t, A = 1.0, phi = 0, f = 1.0)
plotModel(t, A = 1.0, phi = 0, f = 2.0, thisCol = "#DD99CC", add = TRUE)
plotModel(t, A = 1.0, phi = 0, f = 4.0, thisCol = "#FFDDEE", add = TRUE)
plotModel(t, A = 1.0, phi = 0, f = 0.5, thisCol = "#990000", add = TRUE)

# Varying phi
plotModel(t, A = 1.0, phi =  0, f = 1.0)
plotModel(t, A = 1.0, phi =  5, f = 1.0, thisCol = "#DD99CC", add = TRUE)
plotModel(t, A = 1.0, phi = 15, f = 1.0, thisCol = "#EEBBDD", add = TRUE)
plotModel(t, A = 1.0, phi = 30, f = 1.0, thisCol = "#FFDDEE", add = TRUE)
plotModel(t, A = 1.0, phi = 60, f = 1.0, thisCol = "#EEBBDD", add = TRUE)


# ==   2.2  Apply the model to actual observations  ============================

# Let's consider a profile we found in our linear regression analysis: gene 5571
# (YOR229W, WTM2), a replication stress response gene.
iRow <- 5571
plot(t, ygProfiles[iRow, ], col = "black", type = "b",
     ylim = c(-0.4, 0.4),
     xlab = "t (min.)", ylab = "expression log-ratio",
     main = sprintf("%s (%s)", ygData$sysName[iRow], ygData$stdName[iRow]))
abline(h =  0, col = "#DDEEFF")
abline(v = 60, col = "#DDEEFF")
text(0, 0.35, label = "Observed", pos = 4, cex = 0.8)


# Our default parameters are not bad - after all, we discovered the gene using
# this model (with fixed parameters). (I'm manually entering A = 0.2 here, for
# the optics, because that's what I see in the data):
plotModel(t, A =  0.2, phi = 0, f = 1.0, thisCol = "#DD99CC", add = TRUE)
text(0, 0.30, label = "Conceptual model", pos = 4, cex = 0.8, col = "#DD99CC")

# calculate correlation of the data with the rturn values of our model :
cor(ygProfiles[iRow, ], cycEx(t, A =  0.2, phi = 0, f = 1.0)) # 0.865


# ==   2.3  Improve the fit with nls()  ========================================

# Let's see if we can improve the fit:

#  1: assign the data to a variable
(y <- ygProfiles[iRow,])

#   (Note: why is this necessary? Otherwise the interpretation as a formula gets
#    confused what exactly we are trying to do. Cf: ?formula )


#  2: Use nls() to calculate a non-linear least squares fit. While linear
#  least-squares fits have an analytical solution, non-linear fits need to be
#  optimized numerically. This is typically done by varying parameters while
#  calculating the fit, then reporting the results once the best possible choice
#  of parameters has been found. As a numerical (not analytic) procedure, this
#  is subject to problems of convergence (no solution can be found in reasonable
#  time), and to getting stuck in local minima ... as we'll see later.
myFit <- nls(y ~ cycEx(t, A, phi, f),
             start = list(A = 0.2,
                          phi = 0,
                          f = 1.0) )



myFit
coef(myFit)  # a named vector that we can use to extract the fitted parameters

# ... and we can plot the fitted function
plotModel(t, A = coef(myFit)["A"],
          phi = coef(myFit)["phi"],
          f = coef(myFit)["f"],
          thisCol = "#00CC77", add = TRUE)
text(0, 0.25, label = "Fitted model", pos = 4, cex = 0.8, col = "#00CC77")

cor(ygProfiles[iRow, ], predict(myFit)) # 0.901

# You can see that the curve is closer to the data points, and that the already
# good correlation has improved a bit more.


# ===   2.3.1  Function to plot observations and models

# Here is a function to plot a profile, and its fitted curve

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
    mtext(sprintf("Parameters: cor: %5.3f, %s",
                  cor(y, predict(fit)),
                  paste(names(coef(fit)),
                        sprintf("%5.3f", coef(fit))
                        , sep = ": ", collapse = ", ")),
          col = "#DD99CC", side = 1, line = 4)
    t2 <- seq(0, 120, by = 1)
    y2 <- data.frame(t = t2)
    points(t2, predict(fit, newdata = y2), col = "#DD99CC", type = "l")
}


checkFit(iRow, myFit)


# Using nls(), we can calculate curve-fits of our model for all expression
# profiles, then select those that best match "interesting" parameters.

N <- nrow(ygProfiles)                          # number of rows in our data
nlsResults <- data.frame(A = numeric(N),       # a data frame to collect results
                         phi = numeric(N),
                         f = numeric(N),
                         cor = numeric(N))
for (i in 1:N) {                               # for each gene ...
    pBar(i, N)                                   #   print a progress bar
    y <- ygProfiles[i,]                          #   get the observations ...

    try(myFit <- nls(y ~ cycEx(t, A, phi, f),    #   try fitting, but don't
                     start = list(A = 0.15,      #   crash if the fit doesn't
                                  phi = 0.1,     #   converge ...
                                  f = 1.0) ), silent = TRUE)
    if (length(myFit) > 0) {                     #   if the fit converged ...
        nlsResults$A[i] <- coef(myFit)["A"]        #     save the parameters
        nlsResults$phi[i] <- coef(myFit)["phi"]
        nlsResults$f[i] <- coef(myFit)["f"]
        nlsResults$cor[i] <- cor(y, predict(myFit))
    }
}

# What are some good fits? For example, we could look for high amplitudes, and
# good correlations:
plot(nlsResults$A,nlsResults$cor,
     col = densCols(nlsResults$A,nlsResults$cor),
     pch = 16,
     cex = 0.8)
abline(h = 0, col = "#00EEEE44")
abline(v = 0, col = "#00EEEE44")

# Interpret this plot. What do negative amplitudes mean? What do negative
# correlations mean?

# Replot:

plot(abs(nlsResults$A), abs(nlsResults$cor),
     col = densCols(abs(nlsResults$A), abs(nlsResults$cor)),
     pch = 16,
     cex = 0.8)



# select some interesting genes and check what they are ...
( sel <- which(abs(nlsResults$A) > 0.3 & abs(nlsResults$cor > 0.85)) )
ygData[sel, c("stdName", "alias")]
# Interesting ... mostly genes involved in DNA packaging and replication

# Plot these profiles in a window that highlights the cycle phases
plot(seq(0, 120, by = 5), rep(0, 25), type = "n",
     ylim = c(-1.5, 1.5),
     xlab = "time", ylab = "log-ratio expression")
rect( -2.5, -2,   7.5, 2, col = "#f4dfdf", border = NA)   # G1
rect( 22.5, -2,  37.5, 2, col = "#dfeaf4", border = NA)   # G0
rect( 52.5, -2,  67.5, 2, col = "#f4dfdf", border = NA)   # G1
rect( 82.5, -2,  97.5, 2, col = "#dfeaf4", border = NA)   # G0
rect(112.5, -2, 122.5, 2, col = "#f4dfdf", border = NA)   # G1
text(c(2, 30, 60, 90, 118), rep(1.25, 5), c("G1", "G0", "G1", "G0", "G1"))

for (i in 1:length(sel)) {
    points(seq(0, 120, by = 5), ygProfiles[sel[i], ], type = "b", col = "black")
}

# Let's review what we have done here: rather than rely on any specific model of
# "cyclical expression", we have fitted cyclical models to the data, according
# to parameters of amplitude, frequency and phase, and recorded how good a fit
# we achieve. This has allowed us, for example, to discover genes with a high
# amplitude of differential expression that are well modelled by a cyclical
# model. As you can see in the plot, some of these would have had very poor
# correlations with our original model, because they are phase shifted.
# Consider:
iRow <- sel[1]
points(seq(0, 120, by = 5), ygProfiles[iRow, ], type = "b", col = "green")
cor(ygProfiles[iRow, ], myModel)
# vs:
iRow <- sel[6]
points(seq(0, 120, by = 5), ygProfiles[iRow, ], type = "b", col = "red")
cor(ygProfiles[iRow, ], myModel)


# Beyond finding genes, we can use the parameters to get gloabl information
# about the experiment - for example: what is the period of the yeast cell
# cycle? We just need to select genes that *are* cyclically expressed,
# (e.g. abs(cor) >= 0.75 & abs(A) >= 0.1 & abs(f) in [0.5, 2.0]), and look at
# thevalue f in the fit.

sel <- which(abs(nlsResults$cor) >= 0.75 &
             abs(nlsResults$A) >= 0.1 &
             nlsResults$f > 0.5 &
             nlsResults$f <= 2.0)
hist(nlsResults$f[sel], col = "maroon", breaks = 50)
median(nlsResults$f[sel])  # one hour three minutes and twentythree seconds


# TASK:
# Explore different aspects of the fits. Are there genes with
# signifcantly higher frequency? Do we see genes with phase-shifts across the
# entire range of time-points?

# TASK:
# How would you use this data to define genes that are, and genes that are not
# cyclically expressed? Where would you draw the line?
#

# ... [TBC]

# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
