.onAttach <- function(libname, pkgname) {
    version <- read.dcf(
        file = system.file("DESCRIPTION", package = pkgname),
        fields = "Version"
    )
    packageStartupMessage(
        "Dit is ", paste(pkgname, version), "\n",
        pkgname, " is een ontwikkelingshulpmiddelen pakket."
    )
}
