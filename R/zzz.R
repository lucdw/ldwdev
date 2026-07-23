.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
      file = system.file("DESCRIPTION", package = pkgname),
      fields = "Version"
  )
  packageStartupMessage(
      "This is ",
      paste(pkgname, version),
      "\n",
      pkgname,
      " is a package with simple tools for package development."
  )
  if (length(find.package("lavaan", quiet = TRUE)) > 0L) {
    x <- lavaan::lav_matrix_upper2full(c(30, 16, 5, 10, 3, 1))
    x <- 2 * x
  }
}
