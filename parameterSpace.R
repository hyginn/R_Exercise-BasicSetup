# parameterSpace.R
#
# Purpose:  Demonstrate linear and non-linear regression, and how to use the
#           results to analyze data in parameter space.
# Version:  0.2
# Version history:
#           0.2 Add more sophisticated curve fitting
#           0.1 Initial code
#
# Date:     2019-10 - 2019-11
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
# a: the slope, and b: the intercept of the function. The task is, for a given
# data set, to infer what the parameters of it's linear idealization are.

# Let's construct a synthetic sample of observations that could come from
# measuring height and weight of a human cohort. We generate random heights in
# an interval, then calculate hypothetical weights according to a simple linear
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
    if (is.null(fit)) {
        return()
    }
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


# Beyond finding genes, we can use the parameters to get global information
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

# ====  MORE SOPHISTICATED CURVE FITTING  ======================================

# Quite a few of our profiles have rather poor correlations. We could just
# discard them and say: these are not cyclically expressed genes. But it's of
# course better to spend some time and ask what is really going on there. For
# examle, here are genes with high amplitude and poor correlations:

sel <- which(nlsResults$A > 0.3 & nlsResults$cor < 0.4)
nlsResults[sel,]

# Let's write a nice function to plot the profiles and the fitted parameters so
# we can explore them more easily:
#

plotFit <- function(iRow, myA, myPhi, myF) {
    # Without parameters, we just plot the current fit with parameters
    # from nlsResults.
    # With parameters, we try a new fit with the given starting values.
    t <- seq(0, 120, by = 5)
    y <- ygProfiles[iRow,]
    origCor <- nlsResults$cor[iRow]
    origA <- nlsResults$A[iRow]
    origPhi <- nlsResults$phi[iRow]
    origF <- nlsResults$f[iRow]
    plot(t, y, type="b",
         xlab = "", ylab = "log-ratio",
         main = sprintf("%d: %s (%s)",
                        iRow,
                        ygData$sysName[iRow],
                        ygData$stdName[iRow]))
    mtext(sprintf("Original fit:  cor: %5.3f, A: %5.3f, phi: %5.3f, f: %5.3f",
                  origCor,
                  origA,
                  origPhi,
                  origF),
          col = "#AA0000", side = 1, line = 3)
    points(0:120, cycEx(0:120, origA, origPhi, origF),
           type="l", col="#AA0000")
    if (! missing(myA)) { # Try a new fit with these parameters
        myFit <- nls(y ~ cycEx(t, A, phi, f),
                     start = list(A = myA,
                                  phi = myPhi,
                                  f = myF),
                     control = nls.control(maxiter = 200))
        points(0:120, cycEx(0:120,
                            coef(myFit)["A"],
                            coef(myFit)["phi"],
                            coef(myFit)["f"]),
               type="l", col="#00DD88")
        mtext(sprintf("New fit:  cor: %5.3f, A: %5.3f, phi: %5.3f, f: %5.3f",
                      cor(y, predict(myFit)),
                      coef(myFit)["A"],
                      coef(myFit)["phi"],
                      coef(myFit)["f"]),
              col = "#00DD88", side = 1, line = 4)
    }
}

iRow <- 4966  # sel[4], when I ran the code
plotFit(iRow)
# Clearly, this fit has not converged. But if we guess possible parameters ...
plotFit(iRow, 0.1, -20, 0.9)
# ... we get a pretty decent fit with _much_ better correlation.


iRow <- 5058
plotFit(iRow)
# Another case of non-convergence
plotFit(iRow, 0.04, 10, 1.5)


iRow <- 5059
plotFit(iRow)
# Again, non-convergence.
plotFit(iRow, 0.02, 30, 0.5)



# == IMPROVING OUR FITTING STRATEGY:
#    (I) Try different parameters and select the best result

# This is pretty trivial - we'll just write a function that tries starting our
# parameter search from a few different options, then checks which one has the
# best correlation and returns these values. The function could be:

bestFitSimple <- function(y) {
    # Tries different parameter settings for nls() and returns the best
    # fit object.
    nlsFits <- list()
    nlsCors <- numeric()
    myPars <- data.frame(A   = c(0.1, 0.1, 0.04, 0.04, 0.05,  0.03,  0.03),
                         phi = c(0.1,  15,   -2,   10,   45,   0.1,   0.1),
                         f   = c(1.0, 1.0, 0.95,  1.5,  1.0, 0.618, 1.618))
    for (i in 1:nrow(myPars)) {
        try(myFit <- nls(y ~ cycEx(t, A, phi, f),
                         start = list(A = myPars$A[i],
                                      phi = myPars$phi[i],
                                      f = myPars$f[i]) ),
            silent = TRUE)
        if (length(myFit) > 0) {
            nlsFits[[i]] <- myFit
            nlsCors[i] <- cor(y, predict(myFit))
        }
    }
    best <- which(nlsCors == max(abs(nlsCors)))[1]
    return(nlsFits[[best]])
}

# Let's try the procedure for our three problem profiles, and plot the results
iRow <- 4966
( newFit <- bestFitSimple(ygProfiles[iRow, ]) )
checkFit(iRow, newFit)

iRow <- 5058
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- 5059
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

# No magic here - not all fits converged - but we get much more reasonable
# results than we had before. We also increase our processing requirements by a
# factor of seven, because we are trying seven parameter combinations for every
# fit. Let's see if this is worth the trouble:

N <- nrow(ygProfiles)
nlsBestFitResults <- data.frame(A = numeric(N),
                                phi = numeric(N),
                                f = numeric(N),
                                cor = numeric(N))
for (i in 1:N) {
    pBar(i, N)  # print a progress bar (function in .utilities.R)
    y <- ygProfiles[i,]
    myFit <- bestFitSimple(y)
    if (length(myFit) > 0) {
        nlsBestFitResults$A[i]   <- coef(myFit)["A"]
        nlsBestFitResults$phi[i] <- coef(myFit)["phi"]
        nlsBestFitResults$f[i]   <- coef(myFit)["f"]
        nlsBestFitResults$cor[i] <- cor(y, predict(myFit))
    }
}

# make a backup copy ( we could also load that if we don't want to wait for
# the code to finish ...)
save(nlsBestFitResults, file = "nlsBestFitResults.RData")
# load("nlsBestFitResults.RData")

# Let's plot correlation / Amplitude side by side. For ease of comparison, we'll
# flip all nlsResult amplitudes to be negative, and we'll flip all
# nlsBestFitResults to be positive. And we'll plot them as solid dots, with high
# transparency to better visualize the density.

plot(0, 0, type = "l",
     xlim = c(-0.5, 0.5), ylim = c(-1, 1),
     xlab = "A", ylab = "cor")
points(-abs(nlsResults$A), nlsResults$cor, pch=19, col="#CC555505")
points(abs(nlsBestFitResults$A), nlsBestFitResults$cor, col="#33DD3F07")

# Qualitatively, we see great improvement, and quantitatively, eg. considering
# the number of gene expression profiles we have fit with a coefficient of
# correlation better than 0.8 ...

sum(nlsResults$cor > 0.8)
sum(nlsBestFitResults$cor > 0.8)

# For good measure, let's inspect some of the profiles of the "worst of the
# best", eg. ranked at position 600:

sel <- order(nlsBestFitResults$cor, decreasing = TRUE)[600:605]
sel  # [1] 4385  948 3024 2335 1523 3975

nlsBestFitResults[sel, ]  # What do the parameters mean?

# Inspect the profiles and fits ...
iRow <- sel[1]
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- sel[2]
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- sel[3]
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- sel[4]
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- sel[5]
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

# The last  rows show an important limitation on our fit: our model is
# constrained to be symmetric about 0, and if the data is not symmetric but
# shifted, we can't get a good fit. Which leads us directly to:

# == IMPROVING OUR FITTING STRATEGY:
#    (II) Add parameters to our model, for flexibility

# A model can have too many parameters, and that will give rise to "overfitting"
# - satisfying the mathematics of the model, but not the physics or the biology
# of the data. But in our case, with 25 data points and only three parameters to
# fit, adding one or two parameters to the model should be fine. I would like to
# add two parameters: (I) a vertical offset, to release the constraint of our
# fitted model to be symmetric around 0, and (II) a damping function, to handle
# attenuation of expression - after all we saw a large component of global
# change in our first Principal Component.

# Let's see what mathematical form such parameters can take.
# We were considering points along a time axis of two hours in 5 min. intervals:
t <- seq(0, 120, by = 5)

# This was our original function ...
cycEx <- function(t, A, phi, f) {
    # cosine function with amplitude A, phase phi (in minutes), and
    # frequency 1/f, scaled for one full cycle corresponding to 60 min.
    A * (cos((((t - phi) * 2 * pi) / 60) / f) )
}

# ... and here we add a vertical offset B and an exponential damping term,
# exp(-k * t)...
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

# Let's overwrite plotModel()  to
# conveniently explore parameters:
plotModel <- function(t, A = 1.0, phi = 0, f = 1.0, k = 0, B = 0,
                      thisCol = "#CC0000", plt = TRUE) {

    ex <- cycEx2(t, A, phi, f, k, B)
    if (plt) {
        plot(t, ex, col = thisCol, type = "l",
             ylim = c(min(ex) * 1.2, max(ex) * 1.2),
             xlab = "t (min.)", ylab = "expression log-ratio",
             main = "Model",
             sub = sprintf("A: %5.3f, f: %5.3f, phi: %5.3f, k: %5.3f, B: %5.3f",
                           A, f, phi, k, B)
        )
        abline(h =  0, col = "#DDEEFF")
        abline(v = 60, col = "#DDEEFF")
    } else {
        points(t, ex, col = thisCol, type = "l")
    }
}

# Varying B .. trivial
plotModel(t, B = 0)
plotModel(t, B = 0.2,  thisCol = "#DD99CC", plt = FALSE)
plotModel(t, B = -0.2, thisCol = "#FFDDEE", plt = FALSE)
plotModel(t, A = 0.5, B = -0.5, thisCol = "#CC99DD", plt = FALSE)

# Varying k
plotModel(t, k = 0)
plotModel(t, k = 0.01, thisCol = "#DD99CC", plt = FALSE)
plotModel(t, k = 0.02, thisCol = "#FFDDEE", plt = FALSE)
plotModel(t, A = 0.5, k = -0.008, thisCol = "#22EE66", plt = FALSE)


# Ok ... but does it fit? Let's update our bestFit function - and let's see if
# we can get away with not having to try additional parameter combinations.
# Also, we'll add lower and upper bounds to ensure that the amplitudes are
# always positive, and initial up- or down- regulation is fitted as phase shift,
# not inversion of amplitude. To define upper- and lower- bounds of parameters,
# we need to use the "port" algorithm of nls().


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
        return(NULL)
    }

}


# Evaluate a few fits...

iRow <- 1207
( newFit <- bestFit(ygProfiles[iRow, ]) )
checkFit(iRow, newFit)

iRow <- 2578
checkFit(iRow, bestFit(ygProfiles[iRow, ]))
# cor is 0.892 ... and with the old function:
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))

iRow <- 4428
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

iRow <- 281
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

iRow <- 2501
checkFit(iRow, bestFit(ygProfiles[iRow, ]))
# This was a problem fit, quite good now ... Compare to the old fit
checkFit(iRow, bestFitSimple(ygProfiles[iRow, ]))
# Once again ...
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

# Some of our standards: Mbp1, Swi4, Swi6 ...

iRow <- which(ygData$stdName == "MBP1")
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

iRow <- which(ygData$stdName == "SWI4")
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

iRow <- which(ygData$stdName == "SWI6")
checkFit(iRow, bestFit(ygProfiles[iRow, ]))

# === Model data for data mining

# We can now recalculate all the fits, and mine the results for genes with
# similar parameters (coexpressed?), phase shifted (causally related), or other
# interesting parameter combinations.

N <- nrow(ygProfiles)
nlsParams <- data.frame(A = numeric(N),
                        phi = numeric(N),
                        f = numeric(N),
                        k = numeric(N),
                        B = numeric(N),
                        cor = numeric(N),
                        call = character(N),
                        stringsAsFactors = FALSE)
for (i in 1:N) {
    pBar(i, N)  # print a progress bar (function in .utilities.R)
    y <- ygProfiles[i,]
    myFit <- bestFit(y)
    if (length(myFit) > 0) {
        nlsParams$A[i]   <- coef(myFit)["A"]
        nlsParams$phi[i] <- coef(myFit)["phi"]
        nlsParams$f[i]   <- coef(myFit)["f"]
        nlsParams$k[i]   <- coef(myFit)["k"]
        nlsParams$B[i]   <- coef(myFit)["B"]
        nlsParams$cor[i] <- cor(y, predict(myFit))
        nlsParams$call[i] <- as.character(myFit$call)[1]
    }
}

# This takes about 7 minutes.

# ... the results are saved in "nlsParams.RData" and can be loaded from there if
# you don't want to wait for the processing.
# save(nlsParams, file = "nlsParams.RData")
load(file = "nlsParams.RData")

head(nlsParams)

sum(nlsParams$call == "nls")


# Again, how much has the overall fit improved?
plot(0, 0, type = "l",
     xlim = c(-0.5, 0.5), ylim = c(-1, 1),
     xlab = "A", ylab = "cor")
points(-abs(nlsBestFitResults$A),
       nlsBestFitResults$cor, pch=19, col="#33DD3F07")
points(abs(nlsParams$A), nlsParams$cor, col="#333FDD07")

# Qualitatively, we see great improvement, and quantitatively, eg. considering
# the number of gene expression profiles we have fit with a coefficient of
# correlation better than 0.8 ...

sum(nlsResults$cor > 0.8)
sum(nlsBestFitResults$cor > 0.8)
sum(nlsParams$cor > 0.8)

# ... we now get a very significant fit for more than 1/3 of all genes! Note
# however that significant fit does not mean that we have discovered cyclical
# expression in all of those genes, but that we can now get reasonable fits also
# for genes that do _not_ follow the model of a cyclical varying function well.
# It becomes our task now to consider which parameters actually _do_ support
# annotating a gene as a cell-cycle gene.

# For further plotting, we construct two helper functions: a function that
# returns the first positive peak, and a function that marks its position on a
# plot with a triangle.
#

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

# Here is a function that will help us visualize some plots and fits - we pass a
# selection of rows, and the function plots the profile and its fit for a random
# row from the selection, again, and again - as long as we press enter.

exploreFits <- function(sel) {
    myKey <- ""
    while (myKey == "") {
        iRow <- sample(sel, 1)
        checkFit(iRow, bestFit(ygProfiles[iRow, ]))
        markPeak(iRow)
        myKey <- readline(prompt = "<enter> for next, any other key to stop >")
    }
}


# Try a few random sampled profiles ...
exploreFits(1:nrow(ygProfiles))

# == PARAMETER DISTRIBUTIONS

# Since we now have reasonable fits for most expression profiles, we can explore
# and evaluate the parameter distributions.

# === f: frequency
# A cycle period of 60 minutes corresponds to f == 1 in our parameters.
hist(nlsParams$f, breaks = 100, xlim = c(0, 6))

# ... perhaps better to interpret if we transform f to period:
hist(60/nlsParams$f, breaks = 1000, xlim=c(0, 120))

# This shows us that indeed a large number of genes have been fitted with a
# frequency that approximates the cell-cycle - but there is also a lot of noise.
# ( periods < 30 min. and > 90 min.)

sel <- which(nlsParams$f > 0.9 & nlsParams$f < 1.1)
exploreFits(sel)

sel <- which(nlsParams$f < 0.5)
exploreFits(sel)

sel <- which(nlsParams$f > 2.0)
exploreFits(sel)


# Do the fits that are centred around 60
# min. periods have higher correlations?

plot(60/nlsParams$f, nlsParams$cor, xlim=c(0, 120), cex = 0.7,
     pch = 19, col = "#0099CC12")
abline(v = 60, col = "#009966", lwd = 0.5)
abline(v = c(50, 70), col = "#55AA88", lwd = 0.5, lty = 2)

# ... in general, that seems to be the case - with correlations +- 10 minutes of
# the 60 min. peak dropping off measurably. Do the fits centred around 60 min.
# periods have higher amplitudes?

plot(60/nlsParams$f, nlsParams$A, xlim=c(0, 120), cex = 0.7,
     pch = 19, col = "#00CC9912")
abline(v = 60, col = "#006699", lwd = 0.5)
abline(v = c(50, 70), col = "#5588AA", lwd = 0.5, lty = 2)

# That definitely seems to be the case. Let's get better resolution of the
# low amplitudes by plotting log-values:
#

plot(60/nlsParams$f, log10(nlsParams$A), xlim=c(0, 120), cex = 0.7,
     pch = 19, col = "#00CC9912")
abline(v = 60, col = "#006699", lwd = 0.5)
abline(v = c(50, 70), col = "#5588AA", lwd = 0.5, lty = 2)

# This is very informative: amplitudes around 0.01 correspond to our parameter
# bounds - and more or less correspond to non-cyclically expressed genes ...

sel <- which(nlsParams$A < 0.011 & nlsParams$cor > 0.8)
exploreFits(sel)

# What about fits that have (sustained) high amplitudes?

sel <- which(nlsParams$A > 0.5 &
                 nlsParams$k < 0.03 &
                 nlsParams$f > 0.8 &
                 nlsParams$f < 1.25)
exploreFits(sel)

# ... in general, these fits appear to match out notion of a cell-cycle gene
# quite well.



# ==== Ordering by expression peak

# How do we express the timing of the first expression peak? Our parameter phi
# was constrained to lie between -240 and 240 minutes ...

hist(nlsParams$phi, breaks = 50, xlim = c(-240, 240))
abline(v = c(-240, 240), col = "#66EEAA")

# ... and we see edge effects at the boundaries of these parameters. Plotting
# phase-shifts shows us a core of profiles that are modelled reasonably well
# with cyclical expression, and that profiles that are fitted with "anomalous"
# frequencies have much larger phase shifts.

plot(nlsParams$phi, log10(nlsParams$f), cex = 0.7,
     pch = 19, col = "#CC009909")
abline(h = log10(c(0.75, 1.333)), col = "#66EEAA", lwd = 0.5)


# Obviously, expression profiles fitted with good amplitudes, reasonable
# correlations, periods close to 60 minutes, and low damping are expected to
# have phase shifts in a much narrower range:

sel <- which(nlsParams$A > 0.1 &
                 nlsParams$cor > 0.75 &
                 nlsParams$f > 0.75 &
                 nlsParams$f < 1.333 &
                 nlsParams$k < 0.03 &
                 nlsParams$k > -0.0005)

# Good choices?
exploreFits(sel)

hist(nlsParams$phi[sel], breaks = 50, xlim = c(-240, 240))
abline(v = c(-240, 240), col = "#66EEAA")

# Since expression is cyclic, we can define a reference point in the cycle by
# recording the first positive peak after t == 0 min. (The plots produced with
# exploreFits() mark this peak with a blue triangle.) Let's compute all of the
# peaks, so that we can explore which genes get expressed early in the
# cell-cycle, and which genes late.

N <- nrow(ygProfiles)
refPeaks <- numeric(N)
for (i in 1:N) {
    refPeaks[i] <- getReferencePeak(i)
}

# With this, we can select bona fide cell-cycle genes, and order them according
# to their peak expression:

selCC <- which(nlsParams$A > 0.1 &
                   nlsParams$cor > 0.75 &
                   nlsParams$f > 0.75 &
                   nlsParams$f < 1.333 &
                   nlsParams$k < 0.03 &
                   nlsParams$k > -0.0005)
selCC <- selCC[order(refPeaks[selCC], decreasing = FALSE)]

hist(refPeaks[selCC],
     breaks = seq(0, 80, by = 2),
     col = colorRampPalette(c("#00FF00",
                              "#0066FF",
                              "#0000FF",
                              "#AA0088",
                              "#FF0000",
                              "#FFBBBB",
                              "#FFFFFF"))(40) )
# Here we clearly see how the cell cycle is initiated with an early set of
# transcription and a subsequent wave of effector genes transcribed in the first
# third of the cycle, and a second wave of expression that initiates the end of
# the cycle. This is significant - this bimodal distribution of expression peaks
# suggests that the cycle can be roughly subdivided into two major components of
# concerted expression: replication and division; it is not the case that gene
# expression peaks uniformly over the cycle.

# Let's build a dataframe of cell-cycle genes and save it for future reference.

CCgenes <- data.frame(i = selCC,
                      ID = ygData$sysName[selCC],
                      A = nlsParams$A[selCC],
                      phi = nlsParams$phi[selCC],
                      f = nlsParams$f[selCC],
                      k = nlsParams$k[selCC],
                      B = nlsParams$B[selCC],
                      cor = nlsParams$cor[selCC],
                      peak = refPeaks[selCC],
                      stringsAsFactors = FALSE)

# save(CCgenes, file = "CCgenes.RData")
# load("CCgenes.RData")

# === Plotting cell-cycle progression

# A rather informative view of profiles through cell-cycle progression is shown
# in Figure 1 of Pramila (2006). To reproduce a similar plot, we use the image()
# function:

# ... initialize a matrix
exVals <- matrix(numeric(ncol(ygProfiles) * nrow(CCgenes)),
                 nrow = ncol(ygProfiles), ncol = nrow(CCgenes))

# ... load it with (scaled) expression profiles, from CCgenes - i.e. ordered by
#     expression peak
N <- nrow(CCgenes)
for (iRow in 1:N) {
    exVals[ , N - iRow + 1] <- scale(ygProfiles[CCgenes$i[iRow], ])
}
rownames(exVals) <- colnames(ygProfiles)
colnames(exVals) <- CCgenes$ID

# ... plot as image
image(exVals,
      col = colorRampPalette(c("#1cacf3",
                               "#1cacf3",
                               "#0f8a94",
                               "#000000",
                               "#000000",
                               "#9f388a",
                               "#de2f5d",
                               "#de2f5d"))(256),
      xaxt = "n", yaxt = "n", xlab = "time (min.)", ylab= "rank of phase",
      main = "Cell cycle progression")
axis(1, at = seq(0, 1, length.out = 25),
     labels = seq(0, 120, by = 5), cex.axis = 0.5)
abline(v = 0.5 + (1/50), col = "white", lwd = 0.5)
yTicks <- 1 - (c(100, 300, 500, 700) / N)
axis(2, at = yTicks, labels = c("100", "300", "500", "700"))

# ... or plotting only every twentieth gene:

idx <- seq(1, nrow(CCgenes), by = 20)
image(exVals[ , idx],
      col = colorRampPalette(c("#1cacf3",
                               "#1cacf3",
                               "#0f8a94",
                               "#000000",
                               "#000000",
                               "#9f388a",
                               "#de2f5d",
                               "#de2f5d"))(256),
      xaxt = "n", yaxt = "n", xlab = "time (min.)", ylab= "",
      main = "Cell cycle progression")
axis(1, at = seq(0, 1, length.out = 25),
     labels = seq(0, 120, by = 5), cex.axis = 0.5, lwd.ticks =  0.5)
abline(v = 0.5 + (1/50), col = "white", lwd = 0.5)
yTicks <- seq(1, 0, length.out = 44)
axis(2, at = yTicks, labels = ygData$stdName[CCgenes$i[idx]],
     cex.axis = 0.4, las = 1, lwd.ticks =  0.5)


# In the end, what have we learned through this?

# Non-linear modelling gives us a flexible way to query data for internal
# structure. We can now easily find expression profiles that correspond to
# "interesting" models, given our understanding of amplitude, phase shift and
# attenuation of the expression. Where we first were looking for structure in 25
# time-points each, and later in a handful of principal components, we can now
# query five parameters, e.g. to find genes with significant, or similar
# expression profiles.

# But to actually find "the most interesting" genes cannot be automated. Our
# tools help us view the data, they do not interpret the results. As always:
# data does not interpret itself. We have constructed some nicely sophisticated
# tools, but in a sense that has only shifted our task: from looking at raw
# data, to looking at parameter values. I would argue that much is gained by
# being able to query the data in more principled ways than just visual
# appearance, but still, the problem of biological relevance does not solve
# itself.





# ====  VALIDATION  ============================================================
if (FALSE) {
# Enter function tests and validation of analysis ...

}


# [END]
