#' Knitr engine for Stata
#'
#' @param bin Path to Stata bin directory, if \code{NULL} then statarepl
#' will set it to \code{getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))}, if you don't have \code{options("RStata.StataPath")} setted, you can search and set it use \code{\link{find_stata}}
#' @examples
#' \dontrun{
#' statarepl::set_stata_engine()
#' }
#' @export
set_stata_engine <- function(bin = getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))){
  knitr::opts_chunk$set(engine.path = list(
    stata = bin
  ))
}
