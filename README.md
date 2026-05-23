# wbCorr: Bivariate Within- and Between-Cluster Correlations

The wbCorr package computes bivariate within- and between-cluster correlations for clustered data, such as repeated measures nested in persons, dyads, teams, or other groups. Results can be inspected as tables, matrices, and plots.

For every variable pair, wbCorr first keeps only rows where both variables and the cluster variable are observed. This means missing data are handled pairwise.

The within-cluster correlation is the pooled residual correlation: each observed value is centered around its cluster mean for that same variable pair, and the correlation is computed on those residuals. For Pearson within-cluster correlations, analytic tests use `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of complete observation pairs and `k_pair` is the number of clusters contributing at least one complete pair.

The between-cluster correlation is computed from pair-specific cluster means. By default, `between_weighting = "equal_clusters"` gives every cluster the same weight. Use `between_weighting = "cluster_size"` to compute a sample-size weighted correlation of cluster means, where the weight is the number of complete observation pairs in each cluster. Weighted between-cluster p-values and confidence intervals are marked as approximate; use `between_inference = "none"` to report only the weighted coefficient.

Note. In most cases this is not appropriate for categorical data. Only use data than can be meaningfully centered around it's mean (e.g., interval and ratio data). 

## Installation
You can install this package by running the following inside an R-terminal:

``` R
install.packages('remotes')
remotes::install_github('Pascal-Kueng/wbCorr')
```

## Usage
1. Create an object using the `wbCorr()` function. Printing the object shows the head of the tables.  
2. To access the full tables, use the `get_tables()` function on the object. 
3. To retrieve correlation matrices, use `summary()` on the object. 
4. Plot all correlations in a grid, using `plot()` on the object.  

### Check documentation
```R
?wbCorr # view documentation
```

### Main estimand choices
```R
# Default: pooled within-cluster residual correlations and equal-cluster
# between-cluster correlations.
wbCorr(data, cluster = "person_id")

# Between-cluster correlations weighted by the number of complete pairs in
# each cluster.
wbCorr(data, cluster = "person_id", between_weighting = "cluster_size")

# Same weighted coefficient, but no approximate between-cluster p-values or CIs.
wbCorr(data, cluster = "person_id",
       between_weighting = "cluster_size",
       between_inference = "none")
```

#### Sample Output for get_table()
using function `get_table()` on a 'wbCorr' object will provide you with two tables, one for within- and one for between- cluster correlations.
see ?get_table() for more information and arguments. 
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

#### Sample Output for summary() or get_matrix()
using `summary()` or `get_matrix` on a 'wbCorr' object will create matrices. The merged matrices provide within- and between- correlations with one above, and one below the diagonal. 
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

plot(myObject, 'w')
plot(myObject, 'b')
```

## Citation
Please cite this package using:
```
citation('wbCorr')
```
