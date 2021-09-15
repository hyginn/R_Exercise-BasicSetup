# tocID <- "subsettingPractice.R"
#
# Purpose:  Practice filtering of data via R's subsetting methods
#
# Version: 1.2.1
#
# Date:    2020-09 - 2021-09
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 1.2.1  2021 Maintenance; introducing the "sel" idiom
# V 1.2    Maintenance and more discussion of grepl()
# V 1.1    Update to include code in R-Exercise-BasicSetup
# V 1.0    First code
#
# TODO:
#
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                               Line
#TOC> -------------------------------------------------------------------
#TOC>   1        REVIEW                                                47
#TOC>   1.1        Sample data                                         50
#TOC>   2        THE SIXFOLD PATH TO SUBSETS                           95
#TOC>   2.1        Subsetting by index                                 97
#TOC>   2.2        Using negative indices for subsetting              141
#TOC>   2.3        Subsetting by boolean                              159
#TOC>   2.4        String-matching expressions for subsetting         211
#TOC>   2.5        Subsetting by name                                 284
#TOC>   2.6        Using the "$" operator                             317
#TOC>   3        APPLICATION                                          337
#TOC>   3.1        Reading and preprocessing a dataset                339
#TOC>   3.2        Your turn ...                                      398
#TOC> 
#TOC> ==========================================================================


# A significant portion of your efforts in any project will be spent on
# preparing data for analysis. This includes reading data from various sources,
# preprocessing it, and extracting subsets of interest. R has powerful functions
# that support these tasks.


# =    1  REVIEW  ==============================================================


# ==   1.1  Sample data  =======================================================

# Let's start with a small datframe of synthetic data to go through the main
# principles of subsetting. The same principles apply to matrices and vetors -
# however, data frames are more flexible because their columns can contain data
# of different types (character, numeric, logical ...). Values in vectors and
# matrices must always have the same type.

# Imagine you are a naturalist who has collected some living things and keeps
# observations in a data frame ...

set.seed(112358)
N <- 10

dat <- data.frame(name = sample(LETTERS, N, replace = TRUE),
                  legs = sample(c(2 * (0:5), 100), N, replace = TRUE),
                  type = character(N),
                  matrix(rnorm(5 * N), ncol = 5),
                  stringsAsFactors=FALSE)

#Some auxiliary data ...

dict <- c("fish", "bird", "beast", "bug", "spider", "crab", "centipede")
names(dict) <- c(seq(0, 10, by = 2), 100)
dict

#... to populate the >>type<< column:
dat$type <- dict[as.character(dat$legs)]

# If you already understand the expression above, you're doing pretty well with
# the topic of this tutorial. If you don't, don't worry - by the end of the
# tutorial you will.

# Now let's see what we have:

head(dat)
objectInfo(dat)

# Note that we have given names to some columns, but R made names for the five
# columns of random values that were created as a matrix. Let us look at the
# basic ways to subset such objects. Basically, all these methods work with the
# subsetting operator "[".

?"["

# =    2  THE SIXFOLD PATH TO SUBSETS  =========================================

# ==   2.1  Subsetting by index  ===============================================

# Elements can be uniquely identified by indices in the range of their length
# (for vectors), or their rows and columns (in dataframes and matrices). The
# order is row, column.

dat[2,3]   # one element
dat[2, ]   # empty columns: use all of them
dat[ , 3]  # empty rows, use all of them

#If you want a particular set of row and columns, pass a vector of positive
#integers.
dat[c(2, 3), c(1, 2, 3)]

# Any function that returns a vector of integers can be used. Most frequently we
# use the range operator ":" . Retrieving ranges of rows and/or columns from a
# matrix or data frame is also called "slicing".

dat[1:4, 1:3]
dat[4:1, 1:3]   # same in reverse order
dat[seq(2, N, by=2), ]   # even rows

# But we can do more interesting things, since the indices don't have to be
# unique, or in any order:

dat[c(1, 1, 1, 2, 2, 3), 1:3]

# In particular we can select random subsets...
dat[sample(1:N, 3), 1:3]
dat[sample(1:N, 3), 1:3]
dat[sample(1:N, 3), 1:3]

# ... or sort the dataframe. Sorting requires the order() function, not sort().

sort(dat[ , 2])    # ... gives us the sorted values

order(dat[ , 2])   # ... tells us in which row the sotrted values are
dat[order(dat[ , 2]), 1:3]  # ordered by number of legs
dat[order(dat[ , 1]), 1:3]  # ordered by lexical order of names

# Note: I am indenting expressions so you can first evaluate the expressions
# individually, then see how they fit into the brackets to subset the data.


# ==   2.2  Using negative indices for subsetting  =============================

# If you specify a negative index, that element is excluded.

dat[-1, 1:3]   # not the first row
dat[-N, 1:3]   # not the last row

dat[-1:-3, 1:3]
dat[-(1:3), 1:3]  # same effect

# be careful! a very common mistake, when using the range operator is to
# type -1:3 instead of -(1:3). The problem is that the "-" operator has
# higher precedence then the ":" (range) operator, it turns 1 into -1 and,
# well, execute the two expressions and see for yourself: ...

-(1:3)    # what you wanted
-1:3      # what you get

# ==   2.3  Subsetting by boolean  =============================================

# Instead of indices, we can specify sets of rows or columns by boolean values
# (type: logical): TRUE or FALSE. If we place a vector of logicals into the
# square brackets, only the rows resp. columns for which the expression is TRUE
# are returned.

dat[1:3, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]

# You need to take care that the number of elements exactly matches the number
# of rows or columns, otherwise "vector recycling" will apply and this is
# probably unintended. Thus explicitly specifying a boolean selection like above
# is not all that useful. However, many R functions are "vectorized" and
# applying a logical expression or function to an entire column gives a vector
# of TRUE and FALSE values of the same length. If we place this vector into the
# square brackets, only the rows resp. columns for which the expression is TRUE
# are returned.

dat[ , 2]
dat[ , 2] > 4          # See how this creates a vector
dat[dat[ , 2] > 4, 1:3]

# Expressions can be combined with the "&" (and) and the "|" (or) operators.

dat[ , 4] > 0
dat[ , 5] < 0
dat[ , 4] > 0 & dat[ , 5] < 0
dat[dat[ , 4] > 0 & dat[ , 5] < 0, ]

# In this context, the any() and all() functions may be useful. But take care -
# you can't simply apply them to a range of columns: that would apply the
# condition to all elements of a selection at once. You need to use the apply()
# function to first return a vector. apply()'s second argument switches between
# row-wise and column-wise evaluation. Here, 1 means operate on rows.

apply(dat[ , 4:8], 1, max)           # row-wise, fetch the maximum
apply(dat[ , 4:8], 1, max) > 1       # max() > 1 ?
dat[apply(dat[ , 4:8], 1, max) > 1, ]

"To use any() and all(), we define our own function."

myF <- function(x){any(x > 1.5)}
myF(dat[3, 4:8])

apply(dat[ , 4:8], 1, myF)
dat[apply(dat[ , 4:8], 1, myF), ]

'But we can also write the definition "in place"... '
apply(dat[ , 4:8], 1, function(x){all(x < 0.5)})
#-------------------------
dat[apply(dat[ , 4:8], 1, function(x){all(x < 0.5)}), ]

# ==   2.4  String-matching expressions for subsetting  ========================

# The function grep(), grepl(), and the %in% operator can be used to subset
# via string matching:

grep("r", dat[ , 3])          # types that contain "r"
dat[grep("r", dat[ , 3]), 1:3]

grep("^c", dat[ , 3])         # types that begin with "c"
dat[grep("^c", dat[ , 3]), 1:3]

# grep() has a logical sister, grepl(), that does not return matching indexes,
# but a logical vector of the same length as the input, which is TRUE where
# the pattern matches. This is usually even more useful than grep() because:
#   -  the results can be combined with "|", and "&" operators;
#   -  it will also work in if () conditions when there is _no_ match!
#      Because if there is no match, grep() will return a vector of length 0,
#      and that will not work in an if () condition subsetting. Whereas
#      grepl() will return a vector of FALSEs, which is perfectly fine.
#      Try searching for spiders in our data:

myType <- "spider"
grep(myType, dat$type)
dat[grep(myType, dat$type), 1:3]

grepl(myType, dat$type)
dat[grepl(myType, dat$type), 1:3]

s <- "I like aardvarks, bats, and capibaras."
if (   grep("bats", s)) { cat("Likes bats.\n") }              # pattern present
if (  grepl("bats", s)) { cat("Likes bats.\n") }
if (!  grep("spiders", s)) { cat("Doesn't like spiders.\n") } # pattern absent
if (! grepl("spiders", s)) { cat("Doesn't like spiders.\n") }

# Here is how the %in% operator works. You need it whenever you need to find
# more than one type of something in a longer vector:
scary <- c("spider", "centipede")    # Not bugs. Bugs are cute.
dat[ , 3] %in% scary
dat[dat[ , 3] %in% scary, 1:3]

# This is extremely useful, but you need to remember: the returned logical
# vector has the length of the _first_ vector. Try to remember it as
# "A %in B means: element of A contained %in% vector B"

# One more example to study: extract all vowels
s <- "Entia non sunt multiplicanda praeter necessitatem. (Occam)"
( v <- unlist(strsplit(tolower(s), "")) )
( sel <- v %in% c("a", "e", "i", "o", "u") )
paste(v[sel], collapse = "")

# Read this carefully!
# --------------------

# This is the idiom I most frequently use for subsetting. Whatever I want to
# select is assigned to a variable that I call "sel". This may be Booleans
# (TRUE/FALSE), or indices of elements in a vector. Once this variable is
# assigned, you can inspect it and see what exactly got selected If your code
# is important, this will go a long way to help analyze and VALIDATE its
# correctness.

#     Nb: The parentheses around the expressions cause the contained value to
#     be printed. This is not necessary for program flow, but just to show
#     what has benn assigned to a variable. Compare:

x <- exp(1)      # assignment only - nothing gets printed

x <- exp(1)
print(x)         # assign, then print, in two separate statements

(x <- exp(1))    # assignment and print in one expression



# ==   2.5  Subsetting by name  ================================================

# If rownames and/or columnnames have been defined, we can use these for
# selection. If not defined, they default to the row/column numbers as character
# strings(!).

rownames(dat)  # the row numbers, but note that they are strings!
colnames(dat)  # the ones we have defined

# If we place a string or a vector of strings into the brackets, R matches the
# corresponding row/ column names:

dat[1:5, "name"]
dat[1:5, c("name", "legs")]
dat[1:5, "eyes"]   # error, that name does not exist

# We can combine the techniques e.g. to flexibly select columns. Here we select
# the X1 to X5 columns:

colnames(dat)
grep("^X", colnames(dat))
colnames(dat)[grep("^X", colnames(dat))]
dat[1:3, colnames(dat)[grep("^X", colnames(dat))]]

# This is very useful when the exact position of columns may have changed during
# the analysis. Actually, rows and columns should really never be selected by
# number even though we have done so above. Such numbers are "magic numbers" and
# code that relies on  magic numbers is heard to read and very hard to
# maintain. It is always better to expose the logic with which your columns are
# selected and to make the selection explicit and robust. An exception may be
# when you need a slice of the data for testing purposes, but even then it may
# be preferable to use the head() or tail() functions.

# ==   2.6  Using the "$" operator  ============================================

#The "$" operator returns a single column as a vector. It is not strictly
#necessary - the column can just as well be named in quotation marks within the
#brackets - but I think it makes for more readable code.
dat[1:3, "legs"]
dat$legs[1:3]    # same result. This is the preferred version.
dat$"legs"[1:3]  # works, but isn't necessary
dat[1:3, legs]   # this returns an error - hopefully; but if for any
                 # reason the object >>legs<< DOES exist, you'll get an un-
                 # expected result. Know when to quote!


#Three more functions that I use all the time for data manipulation:
?which
?unique
?duplicated



# =    3  APPLICATION  =========================================================

# ==   3.1  Reading and preprocessing a dataset  ===============================

# After this introduction/review, it is your turn to put things into practice. I
# have included a dataset with this project, a .csv file taken from
# supplementary data of a paper on tissue definition by single cell RNA seq, by
# Jaitin et al. (2014).

# http://www.ncbi.nlm.nih.gov/pubmed/24531970

# This data set contains log values of expression changes in different cell
# types, responding to lipopolysaccharide stimulation. It was posted as an Excel
# file and I am using it here not for its particular contents, but because it
# is quite typical of the way you will encounter data "in the wild".
# I have simply opened that file, and saved it as .csv, unchanged.

# First we open the file and have a look what it contains. Then we will properly
# read it into an R object.

rawDat <- read.csv("Jaitin_2014-table_S3.csv",
                   header = FALSE)

# The object "rawDat" should appear in the Data section of the Environment tab
# in the top-right pane. It has a spreadsheet symbol next to it. Click that - or
# type View(rawDat), and study the object. You should find:
# - all columns are named Vsomething
# - rows 1 to 6 do not contain data
# - there is not a single row that could be used for column names
# - type str(rawDat): all columns are characters.

# This all needs to be fixed.


LPSdat <- rawDat[-(1:6), ]  # drop first six rows
colnames(LPSdat) <- c("genes",      # gene names
                      "B.ctrl",     # Cell types are taken from
                      "B.LPS",      # Figure 4 of Jaitin et al.
                      "MF.ctrl",    # .ctrl and .LPS refer to control
                      "MF.LPS",     #   and LPS challenge
                      "NK.ctrl",    # The cell types are:
                      "NK.LPS",     #   B:    B-cell
                      "Mo.ctrl",    #   MF:   Macrophage
                      "Mo.LPS",     #   NK:   Natural killer cell
                      "pDC.ctrl",   #   Mo:   Monocyte
                      "pDC.LPS",    #   pDC:  plasmacytoid dendritic cell
                      "DC1.ctrl",   #   DC1:  dendritic cell subtype 1
                      "DC1.LPS",    #   DC2:  dendritic cell subtype 2
                      "DC2.ctrl",   #
                      "DC2.LPS",    #
                      "cluster")    # Gene assigned to cluster by authors
rownames(LPSdat) <- 1:nrow(LPSdat)

for (i in 2:ncol(LPSdat)) { # convert number columns to numeric
  LPSdat[,i] <- as.numeric(LPSdat[ ,i])
}

# confirm
head(LPSdat)
str(LPSdat)

# ==   3.2  Your turn ...  =====================================================

# Here are questions for you to code. My suggested answers are included ... you
# need to scroll down to see them. Obviously this is pointless unless you really
# try to solve this yourself.

# get rows 1:10 of the first two columns of LPSdat


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
LPSdat[c(1,2,3,4,5,6,7,8,9,10), c(1,2)]  # Awkward solution, not wrong but ...
1:10    # for sanity's sake! Use range operators instead.
1:2

LPSdat[1:10, 1:2]


# output rows 1:10 of the first two columns in reverse order


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
       10:1
LPSdat[10:1, 1:2]

# rows 1:10 of the first two columns in reverse order,
# but not the third row of the result



#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
 LPSdat[10:1, 1:2]             # as before
(LPSdat[10:1, 1:2])[-3, ]      # then exclude the third row ...
LPSdat[(10:1)[-3], 1:2]        # ... or the third element of your selector
                               # (If you think: "Hey, row # 3 is still there!"
                               #  then think some more :-)


# rows 1:10 of the first two columns in random order
#     hint: use sample()


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
       sample(1:10)
LPSdat[sample(1:10), 1:2]

# rows 1:10 of the first two columns, ordered by
# the value in the second column, ascending.
#     hint: use order()
# (This is the typical use-case of sorting a table by the value in a
# specific column. But don't use sort()!!!
# You need to subset by the result of order() to do this.)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
             LPSdat[1:10,2]
       order(LPSdat[1:10,2])
LPSdat[order(LPSdat[1:10,2]), 1:2]

# rows 1:10 of the column named Mo.LPS


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
LPSdat[1:10, "Mo.LPS"]   # two possibilities
LPSdat$Mo.LPS[1:10]      # I prefer this one


# rows 1:10 of the columns named Mo.LPS and Mo.ctrl


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
LPSdat[1:10, c("Mo.LPS", "Mo.ctrl")]              # this will do

# all genes with gene-names that are three characters long
# hint: use the function nchar()


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
                   LPSdat$genes
             nchar(LPSdat$genes)
             nchar(LPSdat$genes) == 3
LPSdat$genes[nchar(LPSdat$genes) == 3]

# column 1:2 of all rows with gene-names that contain
# the string "Il" (i.e. an interleukin)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Try this first:
grep("Il", "IlInThisString")
grep("Il", "NoneInThisString")   # not


       grep("Il", LPSdat$genes)
LPSdat[grep("Il", LPSdat$genes), 1:2] # or use grepl()


# column 1:2 of all rows with gene-names that contain
# both the string "Il" and "ra"  (receptor subunit alpha)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Here, grepl() is the better choice
# Try this first:
grepl("ra", "raInThisString")
grepl("ra", "NoneInThisString")   # not
# combine the results with the "&" (and) operator


       grepl("Il", LPSdat$genes) & grepl("ra", LPSdat$genes)
LPSdat[grepl("Il", LPSdat$genes) & grepl("ra", LPSdat$genes), 1:2]


# all genes for which B-cells are stimulated by LPS by
# more than 2 log units.


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
        LPSdat$B.LPS - LPSdat$B.ctrl
       (LPSdat$B.LPS - LPSdat$B.ctrl) > 2
LPSdat[(LPSdat$B.LPS - LPSdat$B.ctrl) > 2, 1:3]


# That's it. If you have any interesting ideas about further subsetting and
# filtering, or simply questions about any of this material, drop me a line so
# we can work this out and improve the tutorial.




# [END]
