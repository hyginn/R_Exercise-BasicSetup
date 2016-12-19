# R_Exercise-BasicSetup.R
#
# Purpose: Configure and verify the basic setup of R projects
#
# Note:    This project lives on Github:
#          https://github.com/hyginn/R_Exercise-BasicSetup
#
# Version: 1.0
#
# Date:    2016  12
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 1.0    Golden Spiral example
# V 0.2    Additions to integrate with tutorial
# V 0.1    First code
#
# TODO:
#
#
# == TEST YOUR SETUP ======================================
#

#  BRAVA! You have successfully loaded the project file from GitHub.

# Now go through the tasks below, then return to the Wiki tutorial. But keep
# RStudio open so you can continue to use it in the tutorial.

# ==== The RStudio layout ======================================================

# In the default layout of RStudio's panes, this script file has been opened in
# the top-left pane; I refer to this as the "Script Pane". This is where you
# read and edit files.

# The pane below (bottom-left) is the "Console". This is where you type
# commands, or execute them from a script. This is also where RStudio produces
# its output.

# The pane at the top-right is the "Environment Pane". This is where you can see
# which variables or functions have currently been defined ("assigned") and what
# their values are.

# The pane at the bottom-right displays different kinds of information in its
# tabs: In the "Files Tab" you see a directory listing of the contents of the
# current working directory. The "Plots Tab" shows plots and graphs that R has
# created. And the "Help Tab" displays the information in R's help system. (We
# probably won't be using the other tabs.)


# ==== Execute some code =======================================================
#  You can execute code by typing it into the console.
#  Type: 3 + 4  and then hit <enter>
#
#  Now type: 1 + sqrt(5) / 2
#  (This is NOT the Golden Ratio, phi)
#
#  To change this to the formula for the Golden Ratio, you need to use
#  parentheses to enforce the proper operator precedence. Use the up-arrow key
#  to get the expression back on the command-line, then use the left and right
#  arrow keys to edit the expression and enter the correct parentheses: the
#  expression should read (1 + sqrt(5)) / 2
#  Then press <enter> to execute the corrected expression.
#  R should respond with
#       [1] 1.618034
#

# Another way to access previously typed code is through the History tab in the
# upper-right pane. Scroll to find a line that you want to execute or edit,
# double-click it, and it will appear in the Console. Try this.


# You can ALSO execute code by selecting and pressing <command><enter> (or
# <ctrl><enter> on Windows). This passes code from the Script Pane to the
# console and executes it automatically. This is very convenient - in fact, this
# is the preferred way to work with lengthy scripts. There are several variants:

#  - When nothing is selected in the Script Pane, pressing <command><enter> will
#    execute the current line and move the cursor to the next line. You can
#    "walk" through code line by line in this way.
#  - This is the same as if you would have selected an entire line.

#  - If you select more than one line, you can execute an entire block of code
#    at once. Try this: select the block of code below (lines 87 to 95),
#    and hit <command><enter> to execute it. The code calculates successive
#    approximations of the Golden Ratio from Fibonacci numbers, and then prints
#    the "true" value.

FibPrev <- 1
FibCurr <- 1
for (i in 1:35) {
    print(FibCurr / FibPrev, digits = 17)
    tmp <- FibCurr
    FibCurr <- FibPrev + FibCurr
    FibPrev <- tmp
}
print((1 + sqrt(5)) / 2, digits = 17)   # The real Golden Ratio (approximately)

# - But it's also really useful to be able to select _less_ than one line of
#   code. For example, this allows us to analyze complex, nested R expressions
#   from the inside out.


# ==== Change some code and save the change ====================================

# Here is a bit of code that plots 1.5 turns of the Golden Spiral. Don't worry
# if you don't understand the details - that's not the point right now. Execute
# the code.

turns <- 1.5                                 # number of turns to plot
p <- seq(0, (turns * 2 * pi),
         length.out = round(turns * 100))    # intervals
plot(exp(0.3061868562 * p) * cos(p),         # x-coordinates
     exp(0.3061868562 * p) * sin(p),         # y-coordinates
     type = "l", col = "#CC0000",            # draw as a red line
     xaxt = "n", yaxt = "n",                 # turn tick-marks off
     xlab = "", ylab = "",                   # turn axis labels off
     asp = 1.0)                              # plot with 1:1 aspect ratio
abline(v = 0, col = "#E6EEFF")               # add pale-blue vertical ...
abline(h = 0, col = "#E6EEFF")               # ... and horizontal line

# Now change the value of "turns" from 1.5 to 2.75 in the code above. Note that
# the filename in the Script Pane changes colour: it turns red and has a star
# behind it. This means: this script has unsaved changes.

# Execute the modified block of code. The number of the turns increases.

# Now save the change. The filename changes to black again.

# ==== Quit, and restart where you left off ====================================

# Finally, we'll quit RStudio and restart the project. It should be obvious how
# to quit. But here's how to reload a project.

# Choose: File -> Recent Projects -> <the project you need>

# Note: Take care to choose the "Recent Projects" menu option,
#       not "Recent Files".

# Try this out, use the File -> Quit Session... option to quit - and if you are
# being asked if you want to save anything, for the purpose of this course or
# workshop _always_ answer "No". Then reload the project, note that the value of
# turns should now be what you had changed it to.

# Plot another beautiful spiral for good measure.

# ==== Typing shortcuts and autocomplete =======================================

# RStudio's editor has a number of useful shortcuts and aids. Try them.
#
#   - Typing an opening parenthesis automatically types the closing parenthesis.
#   - Selecting text and typing a quotation mark quotes the text. This also
#     works with single quotation marks, parentheses, square brackets
#     and curly braces.
#
#   - Typing a newline character automatically indents the following line.
#
#   - Typing a newline inside a comment automatically prefixes the line
#     with a comment character.
#
#   - Typing the first three letters of a variable or function name lists
#     possible choices to autocomplete. Use the arrow keys to select, hit
#     <enter> or <tab> to execute. Note that this is not case sensitive.
#     Autocomplete is therefore especially useful if you are not sure about
#     the correct case. Try it: type  Tur  and you should get the variable
#     "turns" selected.

# Review: - The layout of the Panes
#         - Typing code
#         - Editing code in the Console, using the arrow keys
#         - Retrieving code via the History tab
#         - Selecting code in the Script Pane and executing it
#           with <command><enter>
#         - Editing code in the script and saving the change
#         - Quitting RStudio and restarting a recent project.
#         - Some typing shortcuts

# Finally, return to the tutorial Wiki page.


# [END]
