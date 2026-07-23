# Check for updates of wbCorr

This function checks if there is a newer version on GitHub by comparing
the version numbers in the local and remote DESCRIPTION files. It only
runs when called explicitly by the user and does not install updates.

## Usage

``` r
update_wbCorr(ask = FALSE)
```

## Arguments

- ask:

  Deprecated and ignored.

## Value

An integer: 1 if there's a newer version available, 0 if the current
version is the latest, or NULL if there was an error accessing the
remote DESCRIPTION file.
