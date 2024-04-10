#' Creates a character representation of an R object
#'
#' Transforms a value to a character representation e.g. for use in messages.
#'
#' @param x The object to transform.
#' @param sep The type of display when lenght of x > 1L,
#'        'array' displays the items seperated with comma's and in parentheses,
#'        'none' is the same as 'array' but without parentheses, 'and' and
#'        'or' is the same as 'none' but with the als comma replaced by this
#'        conjunction. Values other than 'array' are applied only to the
#'        highest level (if x is a list).
#' @param quote The type of quotes to use to surround character items.
#'
#' @return A character vector of length 1 is returned.
#'
#' @examples
#' opt <- paste0("option", 1:4)
#' cat(tostring(opt, "or", "single"), "\n")
#' cat(tostring(opt, "none", "none"), "\n")
#'
#' a <- list(aaa = c(1, 2.4, 5.8), b = letters[1:5], cc = "C",
#'           d = c("KLERF", "nothing"))
#' cat(tostring(a), "\n")
#' # compare with toString function (in base package) :
#' cat(toString(a), "\n")
#'
#' @author Luc De Wilde
#' @name tostring
#' @rdname tostring
#' @export
tostring <- function(
    x,
    sep = c("array", "none", "and", "or"),
    quote = c("double", "single", "none")) {
  if (missing(x)) {
    return("NULL")
  }
  sep <- match.arg(sep)
  quote <- match.arg(quote)
  xn <- names(x)
  if (is.list(x)) {
    xx <- sapply(x, function(el) tostring(el, "array", quote))
  } else {
    if (is.character(x)) {
      xx <- switch(quote,
                   double = dQuote(x, q = FALSE),
                   single = sQuote(x, q = FALSE),
                   none = x)
    } else {
      xx <- as.character(x)
    }
    xx[is.na(x)] <- "NA"
  }
  if (!is.null(xn)) xx <- paste0(xn, ":", xx)
  if (length(xx) == 1) {
    rv <- xx
  } else {
    if (sep == "array") rv <- paste0("(", paste(xx, collapse = ", "), ")")
    if (sep == "none") rv <- paste(xx, collapse = ", ")
    if (sep == "and") rv <- paste(paste(xx[-length(xx)], collapse = ", "), gettext("and"), xx[length(xx)])
    if (sep == "or") rv <- paste(paste(xx[-length(xx)], collapse = ", "), gettext("or"), xx[length(xx)])
  }
  rv
}
