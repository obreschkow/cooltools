#' Clear memory
#'
#' @importFrom grDevices dev.list
#'
#' @description Clears the shared environment, plots and console
#'
#' @param environment logical flag indicating whether to clear the environment
#' @param plots logical flag indicating whether to clear the RStudio plots
#' @param console logical flag indicating whether to clear the RStudio console
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export

clear = function(environment=TRUE, plots=TRUE, console=TRUE) {
  if (plots) dev.off(dev.list()["RStudioGD"])
  if (console) cat("\014")
  if (environment) rm(list = ls())
}
