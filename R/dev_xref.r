#' Gets a cross-reference of variables used in top-level defined functions
#'
#' This function makes creates a cross-reference of the variabels for
#' all functions defined on the top-level in a .R-file.
#'
#' @param file a character vector item naming the files to search in
#'
#' @return list with for each top.level function a named list with:
#'  'FUNC__FILE_' the name of the file where function is found
#'  'FUNC__OFFSET_' number of lines before the function definition
#'  the variables used and the relative line where they are used
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
#'   cat("function ", names(tmp)[j])
#'   tmp1 <- tmp[[j]]
#'   cat(" found in", tmp1$FUNC__FILE_, "at offset", tmp1$FUNC__OFFSET_, ".\n")
#'   tmp1$FUNC__FILE_ <- NULL
#'   tmp1$FUNC__OFFSET_ <- NULL
#'   for (k in seq_along(tmp1)) {
#'     cat("  ", names(tmp1)[k], ":",
#'     tostring(tmp1[[k]], "none"), "\n")
#'   }
#' }
#' unlink(filename)
#'
#' @author Luc De Wilde
#' @name get_xref
#' @rdname get_xref
#' @export
get_xref <- function(file = "") {
  stopifnot(is.character(file), length(file) > 0L)
  retval <- list()
  for (f in file) {
    parseddata <- get_parsed(f)
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
            assign("FUNC__FILE_", basename(f), env)
            assign("FUNC__OFFSET_", subs1$line1[1] - 1L, env)
            get_vars(subs2, parseddata, env, TRUE)
            retval[[funcname]] <- as.list(env, all.names = TRUE, sorted = TRUE)
          }
        }
      }
    }
  }
  retval
}
get_vars <- function(subs, parseddata, env, first) {
  offset <- env$FUNC__OFFSET_
  if (first) {
    symbolformals <- which(subs$token == "SYMBOL_FORMALS")
    for (k in symbolformals) assign(subs$text[k], subs$line1[k] - offset, env)
  }
  for (j in seq_along(subs$token)) {
    if (subs$token[j] == "SYMBOL") {
      vname <- subs$text[j]
      lines <- get0(vname, env)
      if (is.null(lines)) {
        lines <- subs$line1[j] - offset
      } else {
        lines <- c(lines, subs$line1[j] - offset)
      }
      assign(vname, lines, env)
    }
    if (subs$terminal[j] == FALSE) {
      subsubs <- parseddata[parseddata$parent == subs$id[j], ]
      if (nrow(subsubs) == 3L && any(subsubs$token[2L] == c("'@'", "'$'"))) {
        vname <- get_composite_name(subsubs, parseddata)
        lines <- get0(vname, env)
        if (is.null(lines)) {
          lines <- subs$line1[j] - offset
        } else {
          lines <- c(lines, subs$line1[j] - offset)
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
