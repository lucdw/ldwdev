.onAttach <- function(libname, pkgname) {
    version <- read.dcf(
        file = system.file("DESCRIPTION", package = pkgname),
        fields = "Version"
    )
    packageStartupMessage(
        "This is ", paste(pkgname, version), "\n",
        pkgname, " is a package with simple tools for package development."
    )
}
