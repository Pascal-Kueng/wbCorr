# Plot within- and between associations

Plots the centered variables of the provided dataframe against each
other. Choose whether to plot the between-centered variables
(representing the between-cluster correlations by plotting cluster
means) or the within-centered variables (representing the within-cluster
correlations by plotting deviations from person-means). Panels below the
diagonal show scatterplots with a regression line. Panels above the
diagonal show the correlation from the wbCorr object with its
significance stars.

## Usage

``` r
# S3 method for class 'wbCorr'
plot(x, y, ...)

# S4 method for class 'wbCorr'
plot(
  x,
  y,
  which = NULL,
  plot_NA = TRUE,
  standardize = TRUE,
  outlier_detection = "zscore",
  outlier_threshold = "recommended",
  type = "p",
  pch = 20,
  dot_lwd = 2,
  reg_lwd = 2,
  ...
)
```

## Arguments

- x:

  A wbCorr object to be plotted.

- y:

  Choose which correlations to plot ('within' / 'w' or 'between' / 'b');
  can be used as a positional argument.

- ...:

  further options to be passed to the base plot (pairs) function.

- which:

  Can be used as an alternative to 'y' (e.g., which = 'w'). It has the
  same functionality as 'y', but takes precedence if both are specified.

- plot_NA:

  Boolean. Whether variables that have no variation on the selected
  level should be plotted or not.

- standardize:

  Boolean. Whether the dataset should be standardized before plotting.

- outlier_detection:

  If FALSE, outliers will not be marked in red. Otherwise you may
  provide the method. Choose from: 'zscore', 'mad', or 'tukey'.

- outlier_threshold:

  If 'recommended', the threshold for 'zscore' and 'mad' will be set to
  3, and for 'tukey' to 1.5. You can provide and other numeric here.

- type:

  points, lines, etc. see ?base::plot for available types).

- pch:

  Graphical parameter. Select which type of points should be plotted.

- dot_lwd:

  Graphical parameter. Set size of the points.

- reg_lwd:

  Graphical parameter. Set thickness of the regression line.

## Value

Called for the side effect of drawing a pairs plot; returns `NULL`
invisibly.

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)
