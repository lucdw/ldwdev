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
#' The compare_files function
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
