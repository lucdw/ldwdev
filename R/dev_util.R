#' @importFrom utils browseURL
#' @importFrom utils getParseData
dev_parsed <- function(filename) {
  stopifnot(is.character(filename), length(filename) == 1)
  stopifnot(file.exists(filename))
  filelines <- suppressWarnings(readLines(filename))
  temp <- try(getParseData(parse(text = filelines, keep.source = TRUE)))
  if (inherits(temp, "try-error")) {
    dev_stop(gettextf("file %s cannot be parsed", filename), 3L)
  }
  attr(temp, "basename") <- basename(filename)
  invisible(temp)
}
dev_toplevel_assigns <- function(parseddata, parentid = 0L) {
  checkalias <- function(sfcid) {
    if (parseddata$text[parseddata$id == sfcid] == "lav_alias") {
      parentsfc <- parseddata$parent[parseddata$id == sfcid]
      grandparentsfc <- parseddata$parent[parseddata$id == parentsfc]
      tmp1 <- parseddata[parseddata$parent == grandparentsfc, ]
      if (nrow(tmp1) == 4L) {
        tmp2 <- parseddata[parseddata$parent == tmp1$id[3L], ]
        if (nrow(tmp2) == 1L && tmp2$token == "STR_CONST") {
          return(paste(
            "SYMBOL",
            gsub('"', '', tmp2$text, fixed = TRUE),
            tmp2$line1,
            tmp2$col1 + 1L,
            sep = ";"
        ))
        }
      }
    }
    paste0("U:", parseddata$text[sfcid])
  }
  exprs <- parseddata$id[parseddata$parent == parentid]
  retval <- list()
  for (i in seq_along(exprs)) {
    id <- exprs[i]
    rij <- which(parseddata$id == id)
    tmp <- switch(
      parseddata$token[rij],
      SYMBOL = paste(
        "SYMBOL",
        parseddata$text[rij],
        parseddata$line1[rij],
        parseddata$col1[rij],
        sep = ";"
      ),
      LEFT_ASSIGN = "LEFT_ASSIGN",
      FUNCTION = paste(
        "FUNCTION",
        parseddata$line1[parseddata$id == parseddata$parent[rij]],
        parseddata$col1[parseddata$id == parseddata$parent[rij]],
        parseddata$line2[parseddata$id == parseddata$parent[rij]],
        parseddata$col2[parseddata$id == parseddata$parent[rij]],
        sep = ";"
      ),
      expr = dev_toplevel_assigns(parseddata, id),
      SYMBOL_FUNCTION_CALL = checkalias(id),
      paste0("U:", parseddata$token[rij])
    )
    retval[[i]] <- tmp
    if (grepl("^FUNCTION;", tmp[1])) break
  }
  unlist(retval)
}
dev_toplevel <- function(parseddata) {
  tla <- dev_toplevel_assigns(parseddata)
  asg <- which(tla == "LEFT_ASSIGN")
  functies <- list()
  synoniemen <- list()
  for (i in asg) {
    if (grepl("^SYMBOL;", tla[i - 1L])) {
      smb <- sub("^SYMBOL;", "", tla[i - 1L])
      smbname <- sub(";.*$", "", smb)
      smbloc <- as.integer(strsplit(sub("^.*?;", "", smb), ";", fixed = TRUE)[[
        1L
      ]])
      if (grepl("^SYMBOL;", tla[i + 1])) {
        smb_r <- sub("^SYMBOL;", "", tla[i + 1L])
        smbname_r <- sub(";.*$", "", smb_r)
        smbloc_r <- as.integer(strsplit(
          sub("^.*?;", "", smb_r),
          ";",
          fixed = TRUE
        )[[1L]])
        synoniemen[[smbname]] <- list(
          name = smbname_r,
          def1 = smbloc,
          def2 = smbloc_r,
          src = attr(parseddata, "basename")
        )
      } else if (grepl("^FUNCTION;", tla[i + 1])) {
        funcloc <- as.integer(strsplit(
          sub("^FUNCTION;", "", tla[i + 1L]),
          ";",
          fixed = TRUE
        )[[1L]])
        functies[[smbname]] <- list(
          def1 = smbloc,
          def2 = funcloc,
          src = attr(parseddata, "basename")
        )
      }
    }
  }
  # correction for 'a <-> b' and 'b <-> c' => 'a <-> c' and 'b <-> c'
  linkse <- names(synoniemen)
  rechtse <- sapply(synoniemen, \(x) x$name)
  ab_index <- which(rechtse %in% linkse)
  while (length(ab_index) > 0L) {
    nr1 <- ab_index[1]
    # opgelet: als a <-> b én b <-> a (is waarschijnlijk fout, maar ontloop
    # toch oneindige lus indien het voorkomt)
    if (linkse[nr1] == rechtse[nr1]) {
      synoniemen <- synoniemen[-nr1]
    } else {
      synoniemen[[nr1]] <- synoniemen[[rechtse[[nr1]]]]
    }
    linkse <- names(synoniemen)
    rechtse <- sapply(synoniemen, \(x) x$name)
    ab_index <- which(rechtse %in% linkse)
  }
  list(functies = functies, synoniemen = synoniemen)
}
open_pdf <- function(f) {
  os <- .Platform$OS.type
  if (os == "windows") {
    shell.exec(normalizePath(f)) # nolint
  } else {
    pdf <- getOption("pdfviewer", default = '')
    if (nchar(pdf) == 0)
      stop("The 'pdfviewer' option is not set. Use options(pdfviewer=...)")
    system2(pdf, args = c(f))
  }
}
