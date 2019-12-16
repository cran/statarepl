#' @importFrom cli cat_line
#' @importFrom crayon blue
handle_res <- function(res) {
  if (res == "undefined") {
    res <- blue(res)
  }
  cat_line(res)
}

handle_reses <- function(reses) {
  if (reses[length(reses)] != "... ") {
    sapply(reses[-length(reses)], handle_res)
  }
  np <- gsub("> >", ">", reses[length(reses)])
  return(np)
}


#' Find and set Stata binary path
#'
#' Set Stata binary (among found alternatives) path. These settings are
#' lost when R is closed, therefore you should consider adding a
#' \code{options("RStata.StataPath")} line in your \code{.Rprofile}.
#' @examples
#' \dontrun{
#' find_stata()
#' }
#' @export
find_stata <- function(){
  OS <- Sys.info()["sysname"]
  OS.type <- .Platform$OS.type
    if (OS %in% "Linux" | OS %in% "Darwin") {
      m <- c(
        `Stata MP` = "stata-mp",
        `Stata SE` = "stata-se",
        `Stata IC` = "stata",
        `Small Stata` = "stata-sm"
      )

      bin <- Sys.which(m)
      names(bin) <- names(m)
      nApps <- length(availProg <- bin[ "" != bin])

      if (0 == nApps) {
        stop(
          "No application (detected) availables.\n",
          "Set options('RStata.StataPath'), instead."
        )
      } else if (1 == nApps) {
        cat("Only", names(availProg), "found; I'll use it.\n")
        unnprog <- unname(availProg)
        options(RStata.StataPath = unnprog)
        return(unnprog)
      } else if (nApps > 1) {
        if (!interactive()) {
          stop(
            "Cannot choose a Stata bin path non-interactively.\n",
            "Set options('RStata.StataPath'), instead."
          )
        }
        res <- utils::menu(names(availProg), title = "Stata availables")
        if (res > 0L) {
          unnprog <- unname(availProg[res])
          options(RStata.StataPath = unnprog)
          return(unnprog)
        }
      } else {
        stop("Unexpected error")
      }
      ## ------------------------------
    } else if (OS %in% "Windows") {
      prog <- file.choose()
      prog <- shQuote(tools::file_path_sans_ext(prog))
      options(RStata.StataPath = prog)
      return(prog)
    } else {
      ""
    }
}

#' Stata REPL
#'
#' @param bin Path to Stata bin directory, if \code{NULL} then statarepl
#' will set it to \code{getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))}, if you don't have \code{options("RStata.StataPath")} setted, you can search and set it use \code{\link{find_stata}}
#' @return a Stata REPL
#' @examples
#' \dontrun{
#' stata_repl()
#' }
#' @export
stata_repl <- function(bin = getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))) {
  OS <- Sys.info()["sysname"]
  if (OS %in% "Windows") {
    stop("Stata REPL can't be used on Windows OS!")
  }
  StataREPL$new(bin)
}
