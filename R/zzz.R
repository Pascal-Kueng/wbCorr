#' Check for package updates and update if available
#'
#' This function checks for updates of the package from its GitHub repository
#' and prompts the user to update the package if a new version is available.
#'
#' @return Returns TRUE if an update is available, FALSE otherwise.
#' @export
update_wbCorr <- function() {
  pkgname <- parent.env(environment())$pkgname
  cat("\nChecking for package updates...\n")

  # Repo
  github_username <- "Pascal-Kueng"
  github_repository <- "wbCorr"

  # Check for updates and update the package if necessary
  is_update_available <- devtools::install_github(
    paste0(github_username, "/", github_repository),
    upgrade = "ask",
    quiet = FALSE,
    force = FALSE
  )

  invisible(is_update_available)
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

