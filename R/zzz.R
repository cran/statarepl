.onAttach <- function(libname = find.package("statarepl"),
                      pkgname = "statarepl") {
  OS <- Sys.info()["sysname"]
  if (OS %in% "Windows") {
    packageStartupMessage(
    "Note: statarepl can't be used on Windows OS."
  )
  }
}
