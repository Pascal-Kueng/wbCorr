#' Check for updates of wbCorr
#'
#' This function checks if there is a newer version of a given R package on GitHub by comparing
#' the version numbers in the local and remote DESCRIPTION files. It prints the current package
#' version, the new version available on GitHub, and a message indicating whether an update is
#' available.
#' @return An integer: 1 if there's a newer version available, 0 if the current version is the latest,
#'         or NULL if there was an error accessing the remote DESCRIPTION file.
#' @importFrom utils packageVersion
#' @export
update_wbCorr <- function() {

  local_version <- packageVersion('wbCorr')
  # URL to check newest build
  url <- "https://raw.githubusercontent.com/Pascal-Kueng/wbCorr/main/DESCRIPTION"

  # Get the DESCRIPTION file content from GitHub
  response <- httr::GET(url)

  # Check if the request was successful
  if (!response$status_code == 200) {
    warning("Error: Could not check for updates.\n")
    return(NULL)
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

      response <- readline("\n\nDo you want to update the package? (y/n): ")
      if (tolower(response) == "y") {
        detach("package:wbCorr", unload = TRUE)
        unloadNamespace("wbCorr")

        remotes::install_github("Pascal-Kueng/wbCorr")
        library('wbCorr')
      } else {
        cat("Update canceled.\n")
      }
      return(1)
    } else {
      cat("Latest version installed.\n")
      return(0)
    }
  }
}

.onAttach <- function(libname, pkgname) {
  update_wbCorr()
  packageStartupMessage("\nwbCorr Loaded\n")
}
