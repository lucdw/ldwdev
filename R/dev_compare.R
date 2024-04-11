#' @importFrom methods slot
#' @importFrom methods slotNames
ldw_lines_to_text <- function(welke) {
  if (welke[1] < welke[2]) {
    welke12 <- paste0(welke[1], ",", welke[2])
  } else {
    welke12 <- as.character(welke[2])
  }
  if (welke[3] < welke[4]) {
    welke34 <- paste0(welke[3], ",", welke[4])
  } else {
    if (welke[3] == welke[4]) {
      welke34 <- as.character(welke[4])
    } else {
      welke34 <- ""
    }
  }
  if (welke[1] <= welke[2] && welke[3] <= welke[4]) {
    return(c(welke12, "c", welke34))
  } else if (welke[1] > welke[2]) {
    return(c(welke12, "a", welke34))
  } else {
    return(c(welke12, "d", welke34))
  }
}
#' Compare two text files
#'
#' This function compares two text files and stores the differences in a
#' typical .diff file.
#'
#' @param infile1 a connection or a character vector of length 1
#'                with the path to the first file
#' @param infile2 a connection or a character vector of length 1
#'                with the path to the second file
#' @param outfile a connection or a character vector of length 1
#'                with the path to the output file
#' @param remove.lines NULL or a character vector with strings
#'                identifying lines which should be excluded from comparison
#' @param equal.lines integer value > 1L, for number of
#'                    lines to be a match, default is 2L
#'
#' @return number of differences found
#'
#' @examples
#' filenames <- tempfile(fileext = c(".txt", ".txt"))
#' outfil <- file()
#' writeLines(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
#'             filenames[1])
#' writeLines(c("0", "1", "a", "b", "c", "e", "f", "grrr", "h", "i", "j"),
#'             filenames[2])
#' compare_files(filenames[1], filenames[2], outfil, "rr")
#' cat(readLines(outfil), sep="\n")
#'
#' compare_files(filenames[1], filenames[2], outfil, equal.lines = 3L)
#' cat(readLines(outfil), sep="\n")
#' close(outfil)
#' unlink(filenames)
#'
#' @author Luc De Wilde
#' @name compare_files
#' @rdname compare_files
#' @export
compare_files <- function(infile1, infile2,                       # nolint
                          outfile = paste0(dirname(infile1), "/",
                                           basename(infile1),
                                           basename(infile2), ".diff"),
                          remove.lines = NULL,
                          equal.lines = 2L) {
  stopifnot(is.character(infile1) || inherits(infile1, "connection"))
  stopifnot(is.character(infile2) || inherits(infile2, "connection"))
  stopifnot(is.character(outfile) || inherits(outfile, "connection"))
  stopifnot(is.null(remove.lines) || is.character(remove.lines))
  equal.lines <- as.integer(equal.lines)
  stopifnot(equal.lines > 1L)
  suppressWarnings(lines1 <- readLines(infile1))
  if (length(lines1) == 0L) dev_stop(gettext("infile1 empty"))
  if (!is.character(infile1) && isOpen(infile1, "r")) close(infile1)
  if (!is.null(remove.lines)) {
    for (rml in remove.lines) {
      toremove <- grepl(rml, lines1, fixed = TRUE)
      lines1 <- lines1[!toremove]
    }
  }
  suppressWarnings(lines2 <- readLines(infile2))
  if (length(lines2) == 0L) dev_stop(gettext("infile2 empty"))
  if (!is.character(infile2) && isOpen(infile2, "r")) close(infile2)
  if (!is.null(remove.lines)) {
    for (rml in remove.lines) {
      toremove <- grepl(rml, lines2, fixed = TRUE)
      lines2 <- lines2[!toremove]
    }
  }
  if (is.character(outfile)) {
    oc <- file(outfile, open = "wt")
  } else {
    oc <- outfile
    if (!isOpen(oc, "w")) open(oc, "wt")
  }
  i1 <- 1L
  i2 <- 1L
  aantal <- 0L
  while (i1 <= length(lines1) || i2 <= length(lines2)) {
    window <- 8L
    found <- FALSE
    while (!found) {
      welke <- c(i1, i1 - 1L, i2, i2 - 1L)
      welke1 <- welke2 <- integer(0)
      if (i1 <= length(lines1)) {
        welke[2L] <- min(i1 + window, length(lines1))
        welke1 <- seq.int(welke[1L], welke[2L])
      }
      if (i2 <= length(lines2)) {
        welke[4L] <- min(i2 + window, length(lines2))
        welke2 <- seq.int(welke[3L], welke[4L])
      }
      m <- match(lines1[welke1], lines2[welke2], nomatch = 0L)
      for (j in which(m > 0L)) {
        pos1 <- i1 + j - 1L
        pos2 <- i2 + m[j] - 1L
        tot1 <- min(pos1 + equal.lines - 2L, length(lines1))
        tot2 <- min(pos2 + equal.lines - 2L, length(lines2))
        if (all(lines1[seq.int(pos1, tot1)] == lines2[seq.int(pos2, tot2)])) {
          if (j > 1L || m[j] > 1L) {
            welke[2L] <- i1 + j - 2L
            welke[4L] <- i2 + m[j] - 2L
            txtje <- ldw_lines_to_text(welke)
            cat(paste(txtje, collapse = ""), "\n", sep = "", file = oc)
            if (j > 1L)
              cat(paste0("< ", lines1[seq.int(i1, i1 + j - 2L)], "\n"),
                            sep = "", file = oc)
            if (txtje[2L] == "c") cat("---\n", file = oc)
            if (m[j] > 1L)
              cat(paste0("> ", lines2[seq.int(i2, i2 + m[j] - 2L)], "\n"),
                               sep = "", file = oc)
            aantal <- aantal + 1L
          }
          jj1 <- j + 1L
          jj2 <- m[j] + 1L
          while (i1 + jj1 <= length(lines1) && i2 + jj2 <= length(lines2) &&
                 lines1[i1 + jj1] == lines2[i2 + jj2]) {
            jj1 <- jj1 + 1L
            jj2 <- jj2 + 1L
          }
          i1 <- i1 + jj1
          i2 <- i2 + jj2
          found <- TRUE
          break
        }
      }
      if (!found) {
        if (i1 + window >= length(lines1) && i2 + window >= length(lines2)) {
          txtje <- ldw_lines_to_text(welke)
          cat(paste(txtje, collapse = ""), "\n", sep = "", file = oc)
          if (length(welke1) > 0L) cat(paste0("< ", lines1[welke1], "\n"),
                                       sep = "", file = oc)
          if (txtje[2L] == "c") cat("---\n", file = oc)
          if (length(welke2) > 0L) cat(paste0("> ", lines2[welke2], "\n"),
                                       sep = "", file = oc)
          i1 <- length(lines1) + 1L
          i2 <- length(lines2) + 1L
          aantal <- aantal + 1L
          break
        }
        window <- 2L * window
      }
    }
  }
  if (is.character(outfile)) close(oc)
  return(aantal)
}
#' Compare two R objects
#'
#' This function compares two R objects and writes the differences to a
#' connection or the console.
#'
#' @param object1 the first R object
#' @param object2 the second R object
#' @param outfile a connection or a character vector of length 1
#'                with the path to the output file, if "" (default) write to
#'                console, if FALSE output is returned, see Value.
#' @param ignore regular expression(s) for name of slots or elements (of a
#'               list) to ignore in comparing
#' @param max.diff maximum number of differences to show, default 30L
#'
#' @return number of differences reported or, if outfile FALSE,
#'  character vector with differences and attribute 'diff' the number of
#'  differences found.
#'
#' @examples
#' lst1 <- list(a = 2, b = 2:4, c = c("a", "b"))
#' lst2 <- list(a = 2, b = 2:4, c = c(c1 = "a", c2 = "b"))
#' lst3 <- list(a = 7, b = 2:5, c = c(c1 = "a", c2 = NA_character_))
#' compare_objects(lst1, lst2)
#' compare_objects(lst2, lst3)
#' compare_objects(lst2, lst4 <- lst2)
#' compare_objects(letters, LETTERS, max.diff = 10L)
#' str(compare_objects(lst2, lst3, outfile = FALSE))
#' compare_objects(lst2, lst3, ignore = "c")
#'
#' @author Luc De Wilde
#' @name compare_objects
#' @rdname compare_objects
#' @export
compare_objects <- function(
    object1,
    object2,
    outfile = "",
    ignore = NULL,
    max.diff = 30L) {
  stopifnot(is.character(outfile) || inherits(outfile, "connection") ||
              identical(outfile, FALSE))
  stopifnot(is.numeric(max.diff) && as.integer(max.diff) > 0L)
  string1 <- deparse1(substitute(object1))
  string2 <- deparse1(substitute(object2))
  reportlines <- character(0)
  aantal <- 0
  my.env <- environment()
  where <- ""
  compare_values(object1, object2, where, my.env, ignore)
  if (identical(outfile, FALSE)) {
    attr(reportlines, "diff") <- aantal
    return(reportlines)
  } else {
    cat(c(reportheader(my.env), reportlines), file = outfile, sep = "\n")
    return(aantal)
  }
}
reportheader <- function(env) {
  string1 <- get("string1", env)
  string2 <- get("string2", env)
  outfile <- get("outfile", env)
  hdr <- gettextf("Differences between %s and %s", string1, string2)
  c(hdr, strrep("-", nchar(hdr)), "")
}
compare_values <- function(val1, val2, where, env, ignore) {
  if (identical(val1, val2)) return()
  differences <- get("aantal", env)
  lines <- get("reportlines", env)
  max.differences <- get("max.diff", env)
  outfile <- get("outfile", env)
  class1 <- class(val1)
  class2 <- class(val2)
  if (typeof(val1) != typeof(val2)) {
    lines <- c(lines,
               gettextf("%1$s types are different: %2$s <-> %3$s",
               where, typeof(val1), typeof(val2)))
    differences <- differences + 1L
    assign("aantal", differences, env)
    assign("reportlines", lines, env)
    return() # no further examination if types are different
  }
  if (!setequal(class1, class2)) {
    if (differences == 0) reportheader(env)
    lines <- c(lines,
               gettextf("%1$s have different classes: %2$s <-> %3$s",
               where, tostring(class1), tostring(class2)))
    assign("aantal", differences + 1L, env)
    assign("reportlines", lines, env)
    return() # no further examination if classes different
  }
  if (isS4(val1)) { # handle slots of S4 classes
    for (slot.name in slotNames(val1)) {
      if (any(sapply(ignore, function(re) grepl(re, slot.name)))) next
      compare_values(slot(val1, slot.name), slot(val2, slot.name),
                     paste0(where, "@", slot.name), env, ignore)
      aantalnu <- get("aantal", env)
      if (aantalnu >= max.differences) {
        lines <- c(lines,
                   gettext("Maximum number of differences reached!"))
        assign("reportlines", lines, env)
        break
      }
    }
    return()
  }
  if (is.list(val1)) { # handle elements of lists
    for (elem.name in names(val1)) {
      if (any(sapply(ignore, function(re) grepl(re, elem.name)))) next
      compare_values(val1[[elem.name]], val2[[elem.name]],
                     paste0(where, "$", elem.name), env, ignore)
    }
    return()
  }
  if (!identical(names(val1),names(val2))) {
    lines <- c(lines,
               gettextf("%1$s have different names: %2$s <-> %3$s",
                     where, tostring(names(val1)), tostring(names(val2))))
    differences <- differences + 1L
    assign("aantal", differences, env)
    assign("reportlines", lines, env)
    return() # no further examination if names different
  }
  if (length(val1) != length(val2)) {
    lines <- c(lines, gettextf("%1$s have different length: %2$d <-> %3$d",
                     where, length(val1), length(val2)))
    differences <- differences + 1L
    assign("aantal", differences, env)
    assign("reportlines", lines, env)
    return() # no further examination if lengths different
  }
  elem.names <- names(val1)
  if (is.null(elem.names)) {
    elem.names <- as.character(seq_along(val1))
  } else {
    elem.names <- dQuote(elem.names)
  }
  for (i in seq_along(val1)) {
      elem.name <- elem.names[i]
      if (is.na(val1[i]) && is.na(val2[i])) next
      if (is.na(val1[i]) || is.na(val2[i]) || val1[i] != val2[i]) {
        lines <- c(lines, gettextf("%1$s[%2$s] differ: %3$s <-> %4$s",
                     where, elem.name,
                     tostring(as.vector(val1[i])),
                     tostring(as.vector(val2[i]))))
        assign("aantal", differences + 1L, env)
        assign("reportlines", lines, env)
        differences <- differences + 1L
        if (differences >= max.differences) {
          lines <- c(lines, gettext("Maximum number of differences reached!"))
          assign("reportlines", lines, env)
          break
        }
      }
  }
  return(invisible(NULL))
}
