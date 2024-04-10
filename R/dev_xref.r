#' Gets a cross-reference of variables used in top-level defined functions
#'
#' This function makes creates a cross-reference of the variabels for
#' all functions defined on the top-level in a .R-file.
#'
#' @param file a character item naming the file to search in
#'
#' @return list with for each top.level function a list of variables used
#'  and where they are used
#'
#' @examples
#' filename <- tempfile(fileext = ".R")
#' writeLines(c(
#'  "f1 <- function(a, s3list, s4obj, ...) {",
#'  " b <- paste(\"sum:\", s3list$teller + s4obj@teller)",
#'  " return(paste0(a, b))",
#'  "}"
#' ), filename)
#' tmp <- get_xref(filename)
#' for (j in seq_along(tmp)) {
#'   cat("function ", names(tmp)[j], ":\n")
#'   tmp1 <- tmp[[j]]
#'   for (k in seq_along(tmp1)) {
#'     cat("  ", names(tmp1)[k], ":",
#'     tostring(tmp1[[k]], "none"), "\n")
#'   }
#' }
#'
#' @author Luc De Wilde
#' @name get_xref
#' @rdname get_xref
#' @export
get_xref <- function(file = "") {
  stopifnot(is.character(file), length(file) == 1)
  retval <- list()
  parseddata <- get_parsed(file)
  exprs <- parseddata$id[parseddata$parent == 0 & parseddata$token == "expr"]
  for (i in seq_along(exprs)) {
    subs <- parseddata[parseddata$parent == exprs[i], ]
    asg <- which(subs$token == "LEFT_ASSIGN")
    for (j in asg) {
      if (j > 1L && j < length(subs) && subs$token[j - 1L] == "expr" &&
          subs$token[j + 1L] == "expr") {
        subs1 <- parseddata[parseddata$parent == subs$id[j - 1L], ]
        subs2 <- parseddata[parseddata$parent == subs$id[j + 1L], ]
        if (nrow(subs1) == 1L && subs1$token[1] == "SYMBOL" &&
            subs2$token[1] == "FUNCTION") {
          # found a function on top-level
          funcname <- subs1$text
          env <- new.env(parent = emptyenv())
          get_vars(subs2, parseddata, env, TRUE)
          retval[[funcname]] <- as.list(env, all.names = TRUE)
        }
      }
    }
  }
  retval
}
get_vars <- function(subs, parseddata, env, first) {
  if (first) {
    symbolformals <- which(subs$token == "SYMBOL_FORMALS")
    for (k in symbolformals) assign(subs$text[k], subs$line1[k], env)
  }
  for (j in seq_along(subs$token)) {
    if (subs$token[j] == "SYMBOL") {
      vname <- subs$text[j]
      lines <- get0(vname, env)
      if (is.null(lines)) {
        lines <- subs$line1[j]
      } else {
        lines <- c(lines, subs$line1[j])
      }
      assign(vname, lines, env)
    }
    if (subs$terminal[j] == FALSE) {
      subsubs <- parseddata[parseddata$parent == subs$id[j], ]
      if (nrow(subsubs) == 3L && any(subsubs$token[2L] == c("'@'", "'$'"))) {
        vname <- get_composite_name(subsubs, parseddata)
        lines <- get0(vname, env)
        if (is.null(lines)) {
          lines <- subs$line1[j]
        } else {
          lines <- c(lines, subs$line1[j])
        }
        assign(vname, lines, env)
      } else {
        get_vars(subsubs, parseddata, env, FALSE)
      }
    }
  }
  invisible(NULL)
}
get_composite_name <- function(subsubs, parseddata) {
  if (nrow(subsubs) == 1L) return(subsubs$text[1])
  subsubsubs <- parseddata[parseddata$parent == subsubs$id[1], ]
  coll <- "@"
  if (subsubs$token[2] == "'$'") coll <- "$"
  paste0(get_composite_name(subsubsubs, parseddata),
         coll,
         subsubs$text[3L])
}
