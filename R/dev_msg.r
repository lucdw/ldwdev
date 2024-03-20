# Displays a message (... concatenated with spaces in between) with header
#   ''                  if called via dev_cat
#   'ldwdev NOTE :'     if called via dev_note
#   'ldwdev WARNING :'  if called via dev_warn
#   'ldwdev ERROR :'    if called via dev_stop
# and formatted to have a maximum line length of 'txt.width'
# while all but the first line start with 'indent' spaces
# The message is displayed via cat, message, warning of stop depending on
# the call.
dev_cat <- dev_note <- dev_warn <- dev_stop <-
  function(..., txt.width = 80L, indent = 2L) {
  funcname <- as.character(sys.call()[1L])
  header <- switch(funcname,
                   dev_cat = "",
                   dev_note = gettext("NOTE"),
                   dev_warn = gettext("WARNING"),
                   dev_stop = gettext("ERROR"))
  if (funcname != "dev_cat") header <- paste("ldwdev", header, ":")
  # make sure we only have a single string
  txt <- unlist(list(...), use.names = FALSE)
  txt <- paste(txt, collapse = " ")
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
  msg <- paste(chunks, collapse = " ")
  if (funcname == "dev_cat") cat(msg)
  if (funcname == "dev_note") message(msg, domain = NA)
  if (funcname == "dev_warn") warning(msg, call. = FALSE, domain = NA)
  if (funcname == "dev_stop") stop(msg, call. = FALSE, domain = NA)
}
