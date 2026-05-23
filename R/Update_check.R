#' Check for updates of wbCorr
#'
#' This function checks if there is a newer version on GitHub by comparing
#' the version numbers in the local and remote DESCRIPTION files. It only runs
#' when called explicitly by the user and does not install updates.
#' @param ask Deprecated and ignored.
#' @return An integer: 1 if there's a newer version available, 0 if the current version is the latest,
#'         or NULL if there was an error accessing the remote DESCRIPTION file.
#' @importFrom utils packageVersion
#' @export
update_wbCorr <- function(ask = FALSE) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required to check for updates. Install it with install.packages('httr').",
         call. = FALSE)
  }

  local_version <- packageVersion('wbCorr')
  # URL to check newest build
  url <- "https://raw.githubusercontent.com/Pascal-Kueng/wbCorr/main/DESCRIPTION"

  # Get the DESCRIPTION file content from GitHub
  response <- list(status_code = 1)
  tryCatch({
    response <- httr::GET(url)
  }, error = function(e) {
    return(1)
  })


  # Check if the request was successful
  if (!response$status_code == 200) {
    warning("Could not check for updates.\n")
  } else {
    # Get the remote DESCRIPTION file content as a character string
    remote_description <- httr::content(response, as = "text", encoding = "UTF-8")
    remote_split <- strsplit(remote_description, split = "Version: ")[[1]][2]
    remote_version <- as.package_version(strsplit(remote_split, split = "\n")[[1]][1])

    # Print the local and remote version numbers
    cat("Current package version: ", as.character(local_version), "\n", sep = "")
    cat("Latest package version on GitHub: ", as.character(remote_version), "\n", sep = "")

    # Compare the version numbers and print a message
    if (remote_version > local_version) {
      cat("There is a newer version available on GitHub.\n")
      cat("Install the latest GitHub version manually if you want to update.\n")
      return(1)
    } else {
      cat("Latest version already installed.\n")
      return(0)
    }
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nwbCorr Loaded\n")
}
