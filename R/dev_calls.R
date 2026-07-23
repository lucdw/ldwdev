#' Gets a list of function calls with the named parameters
#'
#' This function creates a list of the function call for
#' all functions called in .R-files with the named parameters specified.
#'
#' @param files a character vector naming the files to search in
#'
#' @return named list with for each file a named list with for each called
#'  function in the file a named list of named arguments with a tuple line,
#'  position where this named argument is found
#'
#' @examples
#' filename <- tempfile(fileext = ".R")
#' writeLines(c(
#'  "f1 <- function(a, s3list, s4obj, ...) {",
#'  " b <- paste(\"sum:\", s3list$teller + s4obj@teller)",
#'  " return(paste0(a, b))",
#'  "}"
#' ), filename)
#' tmp <- get_calls(filename)
#' for (ifile in seq_along(tmp)) {
#'   cat(names(tmp)[ifile], "\n")
#'   thefile <- tmp[[ifile]]
#'   for (ifunc in seq_along(thefile)) {
#'     cat(" ", names(thefile)[ifunc], "\n")
#'     thefunc <- thefile[[ifunc]]
#'     for (ai in seq_along(thefunc)) {
#'       cat("   ", names(thefunc)[ai], " ", thefunc[[ai]], "\n")
#'     }
#'   }
#' }
#' unlink(filename)
#'
#' @author Luc De Wilde
#' @name get_calls
#' @rdname get_calls
#' @export
get_calls <- function(files = "") {
  stopifnot(is.character(files), length(files) > 0L)
  retval <- list()
  for (f in files) { # f = file name full
    retval1 <- list()
    parseddata <- dev_parsed(f)
    fcalls <- parseddata$id[parseddata$token == "SYMBOL_FUNCTION_CALL"]
    for (i in seq_along(fcalls)) {
      func <- parseddata$text[parseddata$id == fcalls[i]]
      exprid <- parseddata$parent[parseddata$id == fcalls[i]]
      exprexprid <- parseddata$parent[parseddata$id == exprid]
      named_args <- parseddata[parseddata$parent %in% exprexprid &
                               parseddata$token == "SYMBOL_SUB", ]
      if (nrow(named_args) > 0L) {
        tmp <- list()
        for (j in seq.int(1L, nrow(named_args))) {
          tmp[[named_args$text[[j]]]] <- c(named_args$line1[j],
                                           named_args$col1[j])
        }
        retval1[[func]] <- tmp
      }
    }
    if (length(retval1) > 0L) {
      retval1list <- list(retval1)
      names(retval1list) <- basename(f)
      retval <- c(retval, retval1list)
    }
  }
  retval
}
