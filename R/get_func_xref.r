#' The get_func_xref function
#'
#' This function makes creates a cross-reference for all function.defs found
#' in a group of r files.
#'
#' @param map a character item naming the map to search in
#' @param recursive a logical indicating if subdirectories should be searched
#'  in for r-files (default FALSE)
#' @param only.cross.file a logical indicating (if TRUE) to select only
#'  references in other files than the definition
#'
#' @return list of function.defs, with in each item a list with functionname,
#'  file, line in file where the function is defined and a dataframe dfrefs
#'  with the places where the function is called (file, line,
#'  function or empty)
#'
#' @examples
#' \dontrun{
#'  temp <- get_func_xref(".")
#'  for (fi in seq_along(temp)) {
#'   temp1 <- temp[[fi]]
#'   cat(temp1$functionname, temp1$file, temp1$line, "\n")
#'   dfrefs <- temp1$dfrefs
#'   if (nrows(dfrefs)>0) {
#'     for (fr in seq.int(1, nrow(dfrefs))) {
#'      cat("     ", dfrefs$refFileName[fr], dfrefs$refLocation[fr],
#'       dfrefs$refFunction[fr], "\n")
#'     }
#'   }
#'  }
#' }
#'
#' @author Luc De Wilde
#' @name get_func_xref
#' @rdname get_func_xref
#' @export
get_func_xref <- function(map = ".",
                          recursive = FALSE,
                          only.cross.file = FALSE) {
  stopifnot(is.character(map), length(map) == 1)
  stopifnot(is.logical(recursive), length(recursive) == 1)
  files <- dir(map, pattern = "\\.[rR]$", recursive = recursive)
  if (length(files) == 0) return(list())
  if (map != ".") files <- paste0(map, "/", files)
  function.defs <- lapply(files, function(filename) {
    temp <- get_parsed(filename)
    temp$filename <- filename
    welke_zo <- which(temp$token == "FUNCTION")
    if (length(welke_zo) == 0) return(FALSE)
    welke <- vapply(welke_zo, function(x) {
        leftassign <- 0
        if (temp$token[x - 1] == "LEFT_ASSIGN") leftassign <- x - 1
        if (temp$token[x - 2] == "LEFT_ASSIGN") leftassign <- x - 2
        if (leftassign == 0) return(0)
        x <- 0
        if (temp$token[leftassign - 1] == "SYMBOL") x <- leftassign - 1
        if (temp$token[leftassign - 2] == "SYMBOL") x <- leftassign - 2
        return(x)
    }, 0)
    welke <- welke[welke != 0]
    if (length(welke) == 0) return(FALSE)
    temp[welke, ]
  })
  w <- sapply(seq_along(function.defs),
              function(x) !identical(function.defs[[x]], FALSE))
  if (length(w) == 0) stop("no function definitions found.")
  function.defs <- do.call(rbind, function.defs[w])
  function.defs <- function.defs[order(function.defs$text), ]
  references <- lapply(files, function(filename) {
    filelines <- suppressWarnings(readLines(filename))
    temp <- try(getParseData(parse(text = gsub("^*\\$", "#", filelines),
                                   keep.source = TRUE)), silent = TRUE)
    if (inherits(temp, "try-error")) return(FALSE)
    temp$filename <- filename
    welke <- which(temp$token == "SYMBOL_FUNCTION_CALL")
    if (length(welke) == 0) return(FALSE)
    temp[welke, ]
  })
  w <- sapply(seq_along(references),
              function(x) !identical(references[[x]], FALSE))
  references <- do.call(rbind, references[w])

  get_function_on_locatie <- function(locatie.file, locatie.lijn) {
    locatie.functies <- function.defs[function.defs$filename == locatie.file, ]
    if (nrow(locatie.functies) == 0) return("")
    locatie.functies <- locatie.functies[order(locatie.functies$line1,
                                               decreasing = TRUE), ]
    locatie.j <- which(locatie.functies$line1 <= locatie.lijn)
    if (length(locatie.j) == 0) return("")
    locatie.functies$text[locatie.j[1]]
  }
  references$functionname <- vapply(seq_len(nrow(references)), function(i) {
    get_function_on_locatie(references$filename[i], references$line1[i])
  }, "_")

  tmp.xref <- lapply(seq_len(nrow(function.defs)), function(i) {
    fname <- function.defs$text[i]
    ffile <- function.defs$filename[i]
    fline <- function.defs$line1[i]
    if (only.cross.file) {
      ind <- which(references$text == fname &
                     (references$filename != function.defs$filename[i]))
    } else {
      ind <- which(references$text == fname)
    }
    fdf <- references[ind, ]
    fdf <- data.frame(
      refFileName = fdf$filename,
      refLocation = fdf$line1,
      refFunction = fdf$functionname
    )
    list(functionname = fname, file = ffile, line = fline, dfrefs = fdf)
  })
  tmp.xref
}
