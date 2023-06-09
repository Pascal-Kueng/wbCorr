# wbCorr: Bivariate Within- and Between-Cluster Correlations

The wbCorr package efficiently decomposes the variance of numeric variables into their within- and between-cluster components, and subsequently computes the corresponding bivariate correlations for each level independently. Results can be plotted.
This functionality is particularly beneficial for longitudinal data analysis.

The package accomplishes this by centering the variables at both within- and between-cluster levels, followed by computing the correlations on the transformed dataframes. To determine significance tests and confidence intervals, the degrees of freedom for within-cluster correlations rely on the total number of complete pairs across all observations, while the degrees of freedom for between-cluster correlations depend on the total number of clusters.  

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
