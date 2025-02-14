#' @title Package Loader
#' 
#' @description
# Function to check for package installation, then install (if necessary) and load libraries.
# Adapted from code developed by Caitlin Mothes, PhD.
#'
#' @param x A character string of the package name.
#' 
#' @return The package is loaded into the R environment.
#' 
#' @examples
#' package_loader("dplyr")

package_loader <- function(x) {
  if (x %in% installed.packages()) {
    library(x, character.only = TRUE)
  } else {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}
