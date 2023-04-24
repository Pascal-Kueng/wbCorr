# wbCorr: Bivariate Within- and Between-Cluster Correlations

The `wbCorr` package separates the variance of variables provided 
into their within- and between-cluster components, and calculates the respective 
bivariate correlations for both levels separately. This is especially useful
for longitudinal analyses.
By centering the variables within and between clusters and then calculating
the respective correlations, this package helps users gain an initial 
understanding of their data before employing more 
sophisticated multilevel analyses. This approach allows for a more in-depth 
examination of relationships at both the within- and between-cluster 
levels, which can be essential for making informed decisions based on the 
structure of the data.

## Installation
You can install this package by running the following inside an R-terminal:

``` R
install.packages('devtools')
devtools::install_github('Pascal-Kueng/wbCorr')
```

## wbCorr()
### Usage
```R
?wbCorr # view documentation
wbCorr(
  data = NULL, 
  cluster = NULL, 
  alpha_level = 0.95, 
  method = "pearson"
)
```
### Arguments
- `data` A dataframe containing numeric variables for which correlations will be calculated.  
- `cluster`	A vector representing the clustering variable or a string with the name of the column in data that contains the clustering variable. 

- `alpha_level`	A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).  

- `method`	A string indicating the correlation method to be used. Supported methods are 'pearson', 'kendall', and 'spearman' (default: 'pearson').  

## get_correlations()
### Usage
```R
get_correlations(
  object = NULL, 
  which = c("within", "between")
)
```
### Arguments
- `object` A `wbCorr` object, created by the `wbCorr()` function.  

- `which` A character vector indicating which correlation table to return. Options are 'within' or 'w', and 'between' or 'b'.

## summary()
### Usage
```R
## S4 method for signature 'wbCorr'
summary(
  object = NULL, 
  which = c("within", "between", "merge"), 
  ...
)
```
### Arguments
- `object` A `wbCorr` object, created by the `wbCorr()` function.  

- `which` A string or a character vector indicating which summaries to return. Options are 'within' or 'w', 'between' or 'b', and various merge options like 'merge', 'm', 'merge_wb', 'wb', 'merge_bw', 'bw'. Default is c('within', 'between', 'merge').  

- `...` Additional arguments passed to the base summary method.  

## Example
```R
myObject <- wbCorr(iris, iris$Species)
print(myObject) 

tables <- get_correlations(myObject)
matrices <- summary(myObject)

print(tables)
print(matrices)
```

## Citation
Please cite this package using:

*Küng P. (2023). wbCorr: Bivariate Within- and Between-Cluster Correlations. University of Zürich. R package version 0.0.0.9000. https://github.com/Pascal-Kueng/wbCorr.*

BibTeX-Entry:
```BibTeX
  @Manual{,
    title = {wbCorr: Bivariate Within- and Between-Cluster Correlations},
    author = {Pascal Küng},
    organization = {University of Zürich},
    year = {2023},
    note = {R package version 0.0.0.9000. https://github.com/Pascal-Kueng/wbCorr},
  }
  ```
