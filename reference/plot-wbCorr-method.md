# Plot within- and between associations

Plots the centered variables of the provided data frame against each
other. Choose either cluster means (`"between"`) or deviations from
cluster means (`"within"`). Every panel uses the same pair-specific
rows, centering rule, method, and between-cluster weights as the fitted
object. Pearson plots draw a corresponding regression line and annotate
the stored correlation; weighted between-cluster panels use weighted
least squares. Spearman plots report the stored rho without a
linear-regression overlay. Significance stars are shown only when the
fitted wbCorr object contains a p-value for that pair.

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

  Boolean. Whether each plotted pair should be standardized using the
  same weights as its fitted coefficient. For Pearson panels, this makes
  the displayed regression slope equal to the stored correlation.

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

Invisibly returns the supplied `wbCorr` object. Called for the side
effect of drawing a pairs plot of the selected within- or
between-cluster centered variables.

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)
