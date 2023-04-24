# wbCorr: Bivariate Within- and Between-Cluster Correlations

The `wbCorr` package separates the variance of variables provided 
into their within- and between-cluster components, and calculates the respective 
bivariate correlations for both levels separately. This is especially useful
for longitudinal analyses.  

## Installation
You can install this package by running the following inside an R-terminal:

``` R
install.packages('devtools')
devtools::install_github('Pascal-Kueng/wbCorr')
```

## Usage
1. Create an object using the `wbCorr()` function. Printing the object shows the head of the tables.  
2. To access the full tables, use the `get_tables()` function on the object. 
3. To retrieve correlation matrices, use `summary()` on the object.  

### 1. wbCorr()
```R
?wbCorr # view documentation
wbCorr(
  data = NULL, 
  cluster = NULL, 
  alpha_level = 0.95, 
  method = "pearson"
)
```
- `data` A dataframe containing numeric variables for which correlations will be calculated.  
- `cluster`	A vector representing the clustering variable or a string with the name of the column in data that contains the clustering variable. 

- `alpha_level`	A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).  

- `method`	A string indicating the correlation method to be used. Supported methods are 'pearson', 'kendall', and 'spearman' (default: 'pearson').  

### 2. get_table()
equivalent alias: `get_tables()`
```R
get_table(
  object = NULL, 
  which = c("within", "between")
)
```
- `object` A `wbCorr` object, created by the `wbCorr()` function.  

- `which` A character vector indicating which correlation table to return. Options are 'within' or 'w', and 'between' or 'b'.
#### Sample Output
``` 
# Sample output
> get_table(wbCorrObject)
$within
  Parameter1 Parameter2    r       95% CI t(1598)         p
1       Var1       Var2 0.08 [0.03, 0.13]    3.25   0.001**
2       Var1       Var3 0.25 [0.21, 0.30]   10.44 < .001***
3       Var2       Var3 0.79 [0.76, 0.82]   50.89 < .001***

$between
  Parameter1 Parameter2     r         95% CI t(78)         p
1       Var1       Var2 -0.59 [-0.77, -0.41] -6.48 < .001***
2       Var1       Var3 -0.38 [-0.59, -0.17] -3.65 < .001***
3       Var2       Var3 -0.03  [-0.25, 0.20] -0.24     0.814
```

### 3. summary()
equivalent alias: `get_matrix()` or `get_matrices()`
```R
## S4 method for signature 'wbCorr'
summary(
  object = NULL, 
  which = c("within", "between", "merge"), 
  ...
)
```
- `object` A `wbCorr` object, created by the `wbCorr()` function.  

- `which` A string or a character vector indicating which summaries to return. Options are 'within' or 'w', 'between' or 'b', and various merge options like 'merge', 'm', 'merge_wb', 'wb', 'merge_bw', 'bw'. Default is c('within', 'between', 'merge'). The 'merge_wb' option returns a correlation matrix with within-correlations above the diagonal and between-correlations below the diagonal, while the 'merge_bw' option does the opposite, displaying between-correlations above the diagonal and within-correlations below the diagonal.

- `...` Additional arguments passed to the base summary method.  

#### Sample Output
```
> summary(wbCorrObject)
$within
        Var1    Var2    Var3
Var1    1.00  0.08** 0.25***
Var2  0.08**    1.00 0.79***
Var3 0.25*** 0.79***    1.00

$between
         Var1     Var2     Var3
Var1     1.00 -0.59*** -0.38***
Var2 -0.59***     1.00    -0.03
Var3 -0.38***    -0.03     1.00

$merged_wb
         Var1   Var2    Var3
Var1     1.00 0.08** 0.25***
Var2 -0.59***   1.00 0.79***
Var3 -0.38***  -0.03    1.00

$merged_bw
        Var1     Var2     Var3
Var1    1.00 -0.59*** -0.38***
Var2  0.08**     1.00    -0.03
Var3 0.25***  0.79***     1.00

```

## Example
```R
myObject <- wbCorr(iris, iris$Species)
print(myObject) 

tables <- get_table(myObject)
matrices <- summary(myObject)

print(tables)
print(matrices)
```

## Citation
Please cite this package using:

*Küng, P. (2023). wbCorr: Bivariate Within- and Between-Cluster Correlations. University of Zürich. R package version 0.0.0.9000. https://github.com/Pascal-Kueng/wbCorr.*

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
