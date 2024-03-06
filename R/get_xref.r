#' The get_xref function
#'
#' This function creates a cross-reference for all identifiers found
#' in an r file, grouped per expression on main level.
#'
#' @param filename a character item naming the file to search in
#'
#' @return list with, per expression at the highest level
#' (typically the functions defined on the highest level in
#'  a package r-file), a list of object names with their references
#'
#' @examples
#' \dontrun{
#'  temp <- get_xref("R/xxx_lavaan.R")
#'  mainxref <- as.environment(temp[[1]]) # environment to view all
#'  values when Viewed in RStudio
#' }
#'
#' @author Luc De Wilde
#' @name get_xref
#' @rdname get_xref
#' @export
get_xref <- function(filename) {
  temp <- get_parsed(filename)
  highest.level <- temp[temp$parent == 0, ]
  highest.level.expr <- highest.level[highest.level$token == "expr", ]
  retval <- lapply(seq.int(nrow(highest.level.expr)), function(i.expr) {
    temp1 <- temp[(temp$line1 >= highest.level.expr$line1[i.expr]) &
                    (temp$line1 <= highest.level.expr$line2[i.expr]), ]
    welke <- temp1$token == "SYMBOL" |
      temp1$token == "SLOT" |
      temp1$token == "SYMBOL_FUNCTION_CALL" |
      temp1$token == "SYMBOL_FORMALS" |
      temp1$text == "$"
    slots <- which(temp1$token == "SLOT")
    if (any(slots)) temp1$text[slots] <- paste0("@", temp1$text[slots])
    symbols1 <- temp1$text[welke]
    lijnen1 <- temp1$line1[welke]
    symbols2 <- character(length(symbols1))
    lijnen2 <- integer(length(symbols1))
    s1i <- 1L
    s2i <- 0L
    append <- FALSE
    for (s1i in seq_along(symbols1)) {
      if (symbols1[s1i] == "$") {
        append <- TRUE
      } else if (substr(symbols1[s1i], 1L, 1L) == "@") {
        symbols2[s2i] <- paste0(symbols2[s2i], symbols1[s1i])
      } else {
        if (append) {
          symbols2[s2i] <- paste0(symbols2[s2i], "$", symbols1[s1i])
          append <- FALSE
        } else {
          s2i <- s2i + 1L
          symbols2[s2i] <- symbols1[s1i]
          lijnen2[s2i] <- lijnen1[s1i]
        }
      }
    }
    symbols2 <- symbols2[1:s2i]
    lijnen2 <- lijnen2[1:s2i]
    symbols <- sort(unique(symbols2))
    symbollines <- lapply(symbols, function(identifier) {
      lijnen2[symbols2 == identifier]
    })
    names(symbollines) <- symbols
    symbollines
  })
  names(retval) <- sapply(seq.int(nrow(highest.level.expr)), function(i.expr) {
    paste("from", highest.level.expr$line1[i.expr], "to",
          highest.level.expr$line2[i.expr])
  })
  return(retval)
}
