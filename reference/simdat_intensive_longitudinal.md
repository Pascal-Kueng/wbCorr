# Simulated Intensive Longitudinal Dataset

A simulated intensive longitudinal dataset to test the package
capabilities. This dataset contains 5,000 observations from 100
participants measured on 50 days, plus three variables (var1, var2, and
var3) that are correlated at both the within- and between-person levels.

## Format

A data frame with 5,000 rows and the following five columns:

- participantID:

  Identifier for each participant (integer)

- day:

  Day variable varying only within-person (integer)

- var1:

  Variable 1 (numerical)

- var2:

  Variable 2 (numerical)

- var3:

  Variable 3 (numerical)

## Source

A simulated dataset by P. Küng

## Details

The within-person correlations are all positive:

- var1 & var2: 0.1

- var1 & var3: 0.3

- var2 & var3: 0.8

The between-person correlations are all negative:

- var1 & var2: -0.5

- var1 & var3: -0.4

- var2 & var3: -0.2

Time trends (within):

- var1 & time: 0.0

- var2 & time: 0.0

- var3 & time: 0.4
