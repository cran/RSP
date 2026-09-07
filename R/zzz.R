.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "www/img",
      package = "RSP"
    )
  )
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}

utils::globalVariables(c(
  "Bartlett_Interpretation", "Category", "Correct", "KMO_Interpretation",
  "MSA", "Option", "Rate", "Result", "Statistic", "Statistics"
))
