pkgname <- "wbCorr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('wbCorr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_ICC")
### * get_ICC

flush(stderr()); flush(stdout())

### Name: get_ICC
### Title: Return all ICCs for the original variables.
### Aliases: get_ICC get_ICCs get_icc

### ** Examples

# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns the ICCs:
ICCs <- get_ICC(correlations)
print(ICCs)




cleanEx()
nameEx("get_matrix")
### * get_matrix

flush(stderr()); flush(stdout())

### Name: get_matrix
### Title: Return matrices for within- and/or between-cluster correlations.
### Aliases: get_matrix get_matrices summary,wbCorr-method summary.wbCorr

### ** Examples

# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns a correlation matrix with stars for p-values:
matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
print(matrices)

# Access specific matrices by:
# Option 1:
matrices$within
# Option 2:
within_matrix <- summary(correlations, which = 'w') # or use 'within'
merged_within_between <- summary(correlations, which = 'wb')
print(within_matrix) # could be saved to an excel or csv file (e.g., write.csv)




cleanEx()
nameEx("get_table")
### * get_table

flush(stderr()); flush(stdout())

### Name: get_table
### Title: Retrieve full tables for both within- and/or between-cluster
###   correlations for a wbCorr object.
### Aliases: get_table get_tables

### ** Examples

# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns a list with full detailed tables of the correlations:
tables <- get_table(correlations) # the get_tables() function is equivalent
print(tables)

# Access specific tables by:
# Option 1:
tables$between
# Option 2:
within_table <- get_tables(correlations, which = 'w') # or use 'within' or 'between'
print(within_table) # within_table could be saved to an excel or csv file (e.g., write.csv)




cleanEx()
nameEx("print.wbCorr")
### * print.wbCorr

flush(stderr()); flush(stdout())

### Name: print,wbCorr-method
### Title: Print Method for the wbCorr Class
### Aliases: print,wbCorr-method print.wbCorr

### ** Examples

# Example
data("simdat_intensive_longitudinal")
correlations <- wbCorr(simdat_intensive_longitudinal,
                       cluster = 'participantID',
                       confidence_level = 0.95,
                       method = 'spearman',
                       weighted_between_statistics = FALSE)
print(correlations)




cleanEx()
nameEx("show.wbCorr")
### * show.wbCorr

flush(stderr()); flush(stdout())

### Name: show,wbCorr-method
### Title: Show Method for the wbCorr Class
### Aliases: show,wbCorr-method show.wbCorr

### ** Examples

# Example using the iris dataset
cors <- wbCorr(iris, iris$Species, weighted_between_statistics = TRUE)
show(cors)



cleanEx()
nameEx("to_excel")
### * to_excel

flush(stderr()); flush(stdout())

### Name: to_excel
### Title: Saves the passed summary or table to excel
### Aliases: to_excel

### ** Examples

# Importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# Create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# Returns a correlation matrix with stars for p-values:
matrices <- get_matrix(correlations) # summary(correlations) works too.

to_excel(matrices)




cleanEx()
nameEx("wbCorr")
### * wbCorr

flush(stderr()); flush(stdout())

### Name: wbCorr
### Title: wbCorr
### Aliases: wbCorr wbcorr

### ** Examples

# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create a wbCorr object:
correlations <- wbCorr(simdat_intensive_longitudinal,
                     'participantID')

# optionally compute sample-size weighted between-cluster correlations:
weighted_correlations <- wbCorr(simdat_intensive_longitudinal,
                     'participantID',
                     between_weighting = 'cluster_size')

# optionally estimate cluster means from all rows available for each variable:
all_available_correlations <- wbCorr(simdat_intensive_longitudinal,
                     'participantID',
                     centering_rows = 'all_available')

# returns a list with full detailed tables of the correlations:
tables <- get_table(correlations) # the get_tables() function is equivalent
print(tables)

# returns a correlation matrix with stars for p-values:
matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
print(matrices)

# Plot the centered variables against each other
plot(correlations, 'within')
plot(correlations, which = 'b')

# Store the list of correlation matrices to excel
to_excel(matrices)





### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
