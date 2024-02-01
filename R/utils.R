#' @importFrom utils getParseData
get_parsed <- function(filename) {
  stopifnot(is.character(filename), length(filename) == 1)
  stopifnot(file.exists(filename))
  filelines <- suppressWarnings(readLines(filename))
  temp <- try(getParseData(parse(text = filelines, keep.source = TRUE)))
  if (inherits(temp, "try-error")) {
    stop("file ", filename, " cannot be parsed")
  }
  return(invisible(temp))
}
get_toplevel_assigns <- function(parseddata, parentid = 0L) {
  exprs <- parseddata$id[parseddata$parent == parentid]
  retval <- list()
  for (i in seq_along(exprs)) {
    id <- exprs[i]
    rij <- which(parseddata$id == id)
    tmp <- switch(parseddata$token[rij],
     SYMBOL = paste0("SYMBOL:", parseddata$text[rij]),
     LEFT_ASSIGN = "LEFT_ASSIGN",
     FUNCTION = paste("FUNCTION",
                      parseddata$line1[parseddata$id == parseddata$parent[rij]],
                      parseddata$col1[parseddata$id == parseddata$parent[rij]],
                      parseddata$line2[parseddata$id == parseddata$parent[rij]],
                      parseddata$col2[parseddata$id == parseddata$parent[rij]],
                      sep=";"),
     expr = get_toplevel_assigns(parseddata, id),
     paste0("U:", parseddata$token[rij])
    )
    retval[[i]] <- tmp
    if (grepl("^FUNCTION;", tmp[1])) break
  }
  unlist(retval)
}
get_toplevel <- function(parseddata) {
  tla <- get_toplevel_assigns(parseddata)
  asg <- which(tla == "LEFT_ASSIGN")
  functies <- list()
  synoniemen <- list()
  for (i in asg) {
    if (grepl("^SYMBOL:", tla[i - 1L])) {
      smb <- sub("^SYMBOL:", "", tla[i - 1L])
      if (grepl("^SYMBOL:", tla[i + 1])) {
        synoniemen[[smb]] <- sub("^SYMBOL:", "", tla[i + 1L])
      } else if (grepl("^FUNCTION;", tla[i + 1])) {
        functies[[smb]] <- as.integer(
          strsplit(sub("^FUNCTION;", "", tla[i + 1L]), ";", fixed = TRUE)[[1L]])
      }
    }
  }
  # correction for 'a <-> b' and 'b <-> c' => 'a <-> c' and 'b <-> c'
  assocs <- names(synoniemen) %in% unlist(synoniemen, recursive =  FALSE, use.names = FALSE)
  while (any(assocs)) {
    asso <- names(synoniemen)[assocs]
    for (j in asso) synoniemen[[j]] <- synoniemen[[synoniemen[[j]]]]
    # opgelet: als a <-> b én b <-> a (is waarschijnlijk fout, maar ontloop toch loop indien voorkomt)
    removesyns <- names(synoniemen) == unlist(synoniemen, recursive =  FALSE, use.names = FALSE)
    synoniemen <- synoniemen[!removesyns]
    assocs <- names(synoniemen) %in% unlist(synoniemen, recursive =  FALSE, use.names = FALSE)
  }
  #
  list(functies = functies, synoniemen = synoniemen)
}
get_function_calls <- function(parseddata, range) {
  stopifnot(!missing(range), length(range) == 4, is.integer(range))
  frompos <- 10000L * range[1] + range[2]
  topos <- 10000L * range[3] + range[4]
  fcrows <- which(parseddata$token == "SYMBOL_FUNCTION_CALL" &
                    10000L * parseddata$line1 + parseddata$col1 >= frompos &
                    10000L * parseddata$line2 + parseddata$col2 <= topos)
  sort(unique(parseddata$text[fcrows]))
}
