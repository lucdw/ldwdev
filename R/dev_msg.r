# Displays a message (... concatenated with spaces in between) with header
# "ldwdev(function):" and formatted to have a maximum line length of 'txt.width'
# while all but the first line start with 'indent' spaces
dev_cat <- function(...) {
  wat <- unlist(list(...), use.names = FALSE)
  cat(dev_msg(wat))
}

# Displays a message formatted as
# above via R function 'message'
dev_note <- function(...) {
  wat <- unlist(list(...), use.names = FALSE)
  message(dev_msg(wat), domain = NA)
}

# Displays a message  formatted as
# above via R function 'warning'
dev_warn <- function(...) {
  wat <- unlist(list(...), use.names = FALSE)
  warning(dev_msg(wat), call. = FALSE, domain = NA)
}

# Displays a message formatted as
# above via R function 'stop'
dev_stop <- function(...) {
  wat <- unlist(list(...), use.names = FALSE)
  stop(dev_msg(wat), call. = FALSE, domain = NA)
}

# subroutine for above functions
dev_msg <- function(wat, txt.width = 80L, indent = 2L) {
  x <- sub("[() ].*$", "", as.character(sys.calls()))
  ignore.in.stack <- c(
    "^eval$", "^try", "^doTryCatch", "^dev_msg", "^stop$", "^warn$",
    "^which$", "^unique$", "^as\\.", "^unlist$", "^message$",
    "^source$", "^withVisible$", "^tryCatch.W.E$", "^withCallingHandlers$",
    "^do.call$"
  )
  ignores <- rep(FALSE, length(x))
  for (ptrn in ignore.in.stack) {
    ignores <- ignores | grepl(ptrn, x)
  }
  x <- x[!ignores]
  if (length(x) == 0) {
    header <- "ldwdev:"
  } else {
    header <- paste0("ldwdev(", x[length(x)], "):")
  }
  # make sure we only have a single string
  txt <- paste(wat, collapse = " ")
  # split the txt in little chunks
  chunks <- strsplit(paste(header, txt), "\\s+", fixed = FALSE)[[1]]

  # chunk size (number of characters)
  chunk.size <- nchar(chunks)

  # remove empty chunk in position 1 (if txt starts with whitespace)
  if (chunk.size[1L] == 0L) {
    chunks <- chunks[-1L]
    chunk.size <- chunk.size[-1]
  }

  nstart <- 1L
  nstop <- 1L
  while (nstart <= length(chunks)) {
    while (sum(chunk.size[seq.int(nstart, nstop)]) + nstop - nstart + indent
          < txt.width && nstop < length(chunks)) {
      nstop <- nstop + 1
    }
    if (nstop < length(chunks)) {
      chunks[nstop + 1L] <- paste0("\n", strrep(" ", indent),
                                   chunks[nstop + 1L])
    }
    nstart <- nstop + 1L
    nstop <- nstart
  }
  paste(chunks, collapse = " ")
}
