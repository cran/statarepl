#' Stata Session
#'
#' Launch a Stata Session
#'
#' @importFrom subprocess spawn_process process_read process_write PIPE_STDOUT process_kill process_state process_terminate
#' @importFrom utils savehistory loadhistory
#' @importFrom cli cat_rule cat_line
#' @importFrom rlang enquos quo_name
#' @export
#'
#' @field bin Path to Stata bin directory.
#' @field handle A handle as returned by \link[subprocess]{spawn_process}.
StataSession <- R6::R6Class(
  "StataSession",
  public = list(
    bin = NULL,
    handle = NULL,
#' @details
#' Initialise a Stata session
#'
#' @param bin Path to Stata bin directory, if \code{NULL} then statarepl
#' will set it to \code{getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))}, if you don't have \code{options("RStata.StataPath")} setted, you can search and set it use \code{\link{find_stata}}
#' @param params Additional parameters to pass to the initialisation.
#'
#' @examples
#' \dontrun{
#' n <- StataSession$new()
#' n$eval("display 1 + 1")
#' }
    initialize = function(
      bin = NULL,
      params = "-q" # suppress logo
    ){

      if (is.null(bin)){
        bin <- getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))
      }
      self$bin <- bin
      self$handle <- spawn_process(self$bin, params)
      process_read(self$handle, PIPE_STDOUT, timeout = 10000)
    },
#' @details
#' Terminate a Stata session
    finalize = function(){
      self$kill()
    },
#' @details
#' Evaluate Stata code
#'
#' @param code The code to evaluate.
#' @param wait Whether to re-attempt to evaluate if it first fails.
#' @param print Whether to print the result to the R console.
#'
#' @examples
#' \dontrun{
#' n <- StataSession$new()
#' n$eval("display 1 + 1")
#' }
    eval = function(code, wait = TRUE, print = TRUE){
      process_write(self$handle, paste(code, "\n"))
      res <- process_read(self$handle, PIPE_STDOUT, timeout = 0)
      if (wait){
        while (length(res) == 0){
          res <- process_read(self$handle, PIPE_STDOUT, timeout = 0)
        }
        if (print){
          sapply(res[-length(res)], handle_res)
        }
        return(invisible(res[-length(res)]))
      }
    },
#' @details
#' Retrieve Stata state
#'
#' @examples
#' \dontrun{
#' n <- StataSession$new()
#' n$state()
#' n$kill()
#' n$state()
#' }
    state = function(){
      process_state(self$handle)
    },
#' @details
#' Kill Stata
#'
#' @examples
#' \dontrun{
#' n <- StataSession$new()
#' n$kill()
#' n$state()
#' }
    kill = function(){
      if (self$state() != "terminated"){
        process_kill(self$handle)
      } else {
        cli::cat_line("Process not running:")
        self$state()
      }

    },
#' @details
#' Terminate Stata
#'
#' @examples
#' \dontrun{
#' n <- StataSession$new()
#' n$terminate()
#' n$state()
#' }
    terminate = function(){
      if (self$state() != "terminated"){
        process_terminate(self$handle)
      } else {
        cli::cat_line("Process not running:")
        self$state()
      }
    }
  )
)

StataREPL <- R6::R6Class(
  "StataREPL",
  inherit = StataSession,
  public = list(
    np = NULL,
    initialize = function(
      bin = NULL
    ){
      super$initialize(
        bin,
        params = NULL
      )
      self$np <- "Stata > "
      cat_rule("Welcome to Stata REPL")
      cat_line("Press ESC to quit")

      private$hist <- tempfile()
      file.create(private$hist)

      self$prompt(
        self$np
      )
    },
    prompt = function(
      prompt
    ){
      savehistory()
      on.exit(loadhistory())

      repeat {
        loadhistory(
          private$hist
        )
        x <- readline(self$np)
        write(x, private$hist, append = TRUE)
        process_write(self$handle, paste(x, "\n"))
        res <- process_read(self$handle, PIPE_STDOUT, timeout = 0)
        while (length(res) == 0){
          Sys.sleep(0.1)
          res <- process_read(self$handle, PIPE_STDOUT, timeout = 0)
        }
        np <- res[length(res)]
        bod <- res[-length(res)]
        np <- gsub("> >", ">", np)
        if (!grepl("\\.\\.\\.", np)){
          sapply(bod, handle_res)
          self$np <- paste("Stata", np)
        } else {
          self$np <- np
        }
      }
    }
  ),
  private = list(
    hist = NULL
  )
)
