# Displays a message (... concatenated with spaces in between) without header
# and formatted to have a maximum line length of 'txt.width'
# while all but the first line start with 'indent' spaces
dev_cat <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...), use.names = FALSE)
  cat(dev__msg(wat, "", txt.width, indent))
}

# Displays a message with header 'ldwdev NOTE :' and formatted as
# above via R function 'message'
dev_note <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...), use.names = FALSE)
  message(dev__msg(wat, gettext("NOTE"), txt.width, indent), domain = NA)
}

# Displays a message with header 'ldwdev WARNING :' and formatted as
# above via R function 'warning'
dev_warn <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...), use.names = FALSE)
  warning(dev__msg(wat, gettext("WARNING"), txt.width, indent),
          call. = FALSE, domain = NA)
}

# Displays a message with header 'ldwdev ERROR :' and formatted as
# above via R function 'stop'
dev_stop <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...), use.names = FALSE)
  stop(dev__msg(wat, gettext("ERROR"), txt.width, indent),
       call. = FALSE, domain = NA)
}

# subroutine for above functions
dev__msg <- function(wat, header = "", txt.width = 80L, indent = 2L) {
  if (header != "") header <- paste("ldwdev", header, ":")
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
