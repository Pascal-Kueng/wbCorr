#' Check for package updates and update if available
#'
#' This function checks for updates of the package from its GitHub repository
#' and prompts the user to update the package if a new version is available.
#'
#' @return Returns TRUE if an update is available, FALSE otherwise.
#' @export
update_wbCorr <- function() {
  pkgname <- "wbCorr"
  cat("\nChecking for package updates...\n")

  # Replace these with your GitHub username and repository name
  github_username <- "Pascal-Kueng"
  github_repository <- "wbCorr"

  # Get the remote package's DESCRIPTION content
  remote_description_url <- paste0("https://raw.githubusercontent.com/",
                                   github_username, "/",
                                   github_repository, "/main/DESCRIPTION")

  remote_description <- tryCatch({
    readLines(url(remote_description_url, "r"))
  }, error = function(e) {
    message("Error: Unable to check for updates. Check your internet connection.")
    NULL
  })

  if (!is.null(remote_description)) {
    # Extract the remote package's version number
    remote_version <- sub("^Version: (.*)", "\\1", remote_description[grepl("^Version: ", remote_description)])

    if (length(remote_version) > 0 && nchar(remote_version) > 0) {
      local_version <- packageVersion(pkgname)

      if (package_version(remote_version) > local_version) {
        cat("Update available for package '", pkgname, "'.\n")
        cat("Local version: ", local_version, "\n", "Remote version: ", remote_version, "\n", sep = "")

        update_decision <- readline(prompt = "Do you want to update the package? (y/n): ")

        if (tolower(update_decision) %in% c("y", "yes")) {
          devtools::install_github(
            paste0(github_username, "/", github_repository),
            upgrade = "never",
            quiet = FALSE,
            force = TRUE
          )
          cat("Package updated successfully.\n")
        } else {
          cat("Package update skipped.\n")
        }
      } else {
        cat("Your package '", pkgname, "' is up-to-date.\n")
      }
    } else {
      cat("Error: Unable to retrieve the remote package version.\n")
    }
  }

  invisible(NULL)
}


# Load required packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

.onAttach <- function(libname, pkgname) {
  # Call the original .onAttach function if it exists
  if (exists(".onAttach.original", mode = "function", envir = parent.env(environment()))) {
    .onAttach.original(libname, pkgname)
  }

  # Run the update checker function
  update_wbCorr()

  # Add a message to inform users that the package has been loaded
  packageStartupMessage("Package '", pkgname, "' loaded. Run 'check_for_updates()' to check for updates manually.")
}

