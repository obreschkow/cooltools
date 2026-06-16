#' Install a GitHub Package
#'
#' @importFrom devtools install_github
#' @importFrom gitcreds gitcreds_delete
#'
#' @description
#' Installs an R package from GitHub using \code{devtools::install_github()}.
#' If installation fails with GitHub HTTP error 401 / bad credentials, the
#' stored GitHub credentials are deleted using \code{gitcreds::gitcreds_delete()},
#' and the installation is attempted once more.
#'
#' @param reponame Character string. GitHub repository name in the form
#'   \code{"owner/repo"}, e.g. \code{"obreschkow/cooltools"}.
#' @param ... Additional arguments passed to \code{devtools::install_github()}.
#'
#' @return Invisibly returns the result of \code{devtools::install_github()}.
#'
#' @examples
#' \dontrun{
#' getgit("obreschkow/cooltools")
#' }
#'
#' @export
getgit = function(reponame, ...) {

  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required.")
  }

  if (!requireNamespace("gitcreds", quietly = TRUE)) {
    stop("Package 'gitcreds' is required.")
  }

  tryCatch(
    {
      invisible(devtools::install_github(reponame, ...))
    },
    error = function(e) {

      msg = conditionMessage(e)

      bad_credentials =
        grepl("HTTP error 401", msg, fixed = TRUE) ||
        grepl("Bad credentials", msg, fixed = TRUE)

      if (!bad_credentials) {
        stop(e)
      }

      message("GitHub bad credentials detected. Deleting stored git credentials and trying again...")

      gitcreds::gitcreds_delete()

      invisible(devtools::install_github(reponame, ...))
    }
  )
}
