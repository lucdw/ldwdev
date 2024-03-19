dev_cat <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...))
  dev__msg(wat, 0L, txt.width, indent)
}
dev_note <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...))
  dev__msg(wat, 1L, txt.width, indent)
}
dev_warn <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...))
  dev__msg(wat, 2L, txt.width, indent)
}
dev_stop <- function(..., txt.width = 80L, indent = 4L) {
  wat <- unlist(list(...))
  dev__msg(wat, 3L, txt.width, indent)
}
dev__msg <- function(txt,
                     severity = 2L,
                     txt.width = 80L,
                     indent = 2L) {
  stopifnot(any(severity == 0:3))
  if (severity == 0L) {
    header = ""
  } else {
    header <- paste("ldwdev",
                    switch(severity,
                          gettext("NOTE"),
                          gettext("WARNING"),
                          gettext("ERROR")),
                    ":")
  }
  # make sure we only have a single string
  txt <- paste(txt, collapse = " ")

  # split the txt in little chunks
  chunks <- strsplit(paste(header, txt), "\\s+", fixed = FALSE)[[1]]

  # chunk size (number of characters)
  chunk.size <- nchar(chunks)

  # remove empty chunk in position 1 (if txt starts with whitespace)
  empty.idx <- which(chunk.size == 0)
  if (chunk.size[1L] == 0L) {
    chunks <- chunks[-1L]
    chunk.size <- chunk.size[-1]
  }

  nstart <- 1L
  nstop <- 1L
  while(nstart <= length(chunks)) {
    while(sum(chunk.size[seq.int(nstart, nstop)]) + nstop - nstart + indent
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
  msg <- paste(chunks, collapse = " ")
  if (severity == 0L) cat(msg)
  if (severity == 1L) message(msg)
  if (severity == 2L) warning(msg, call. = FALSE)
  if (severity == 3L) stop(msg, call. = FALSE)
}