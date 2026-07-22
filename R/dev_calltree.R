#' @importFrom cyclocomp cyclocomp
#' @importFrom utils packageVersion capture.output
#' @importFrom methods new
#' @importFrom utils setTxtProgressBar txtProgressBar
dev_hash <- function(x) {
  p <- 257L
  m <- 1000033L
  hash_value <- 551017L
  p_pow <- 1L
  tmp <- as.integer(charToRaw(x))
  for (j in tmp) {
    hash_value <- (hash_value + (j + 1L) * p_pow) %% m
    p_pow <- (p_pow * p) %% m
  }
  hash_value
}
dev_htmlnaam <- function(x) {
  paste0("f", dev_hash(x), ".html")
}
dev_call_info <- function(map, ns) {
  stopifnot(is.character(map), length(map) == 1L)
  showprogress <- interactive() && !getOption("quiet", FALSE)
  files <- dir(map, pattern = "\\.[rR]$")
  if (length(files) == 0 && dir.exists(paste0(map, "/R"))) {
    map <- paste0(map, "/R")
    files <- dir(map, pattern = "\\.[rR]$")
  }
  if (length(files) == 0) {
    dev_stop(gettext("No R files found!"))
  }
  if (map != ".") {
    files <- paste0(map, "/", files)
  }
  parsedfiles <- lapply(files, function(f) dev_parsed(f))
  synoniemen <- list()
  functies <- list()
  functieparsed <- integer(0)
  cat("get R source info\n")
  if (showprogress) pb <- txtProgressBar(style = 3)
  for (i in seq_along(parsedfiles)) {
    if (showprogress) setTxtProgressBar(pb, i / length(parsedfiles))
    parsedfile <- parsedfiles[[i]]
    tmp <- dev_toplevel(parsedfile)
    synoniemen <- c(synoniemen, tmp$synoniemen)
    functies <- c(functies, tmp$functies)
    pfiles <- rep.int(i, length(tmp$functies))
    names(pfiles) <- names(tmp$functies)
    functieparsed <- c(functieparsed, pfiles)
  }
  if (showprogress) close(pb)
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  functienamen <- functienamen[functienamen %in% ls(ns)]
  synoniemnamen <- synoniemnamen[synoniemnamen %in% ls(ns)]
  functies <- functies[functienamen]
  synoniemen <- synoniemen[synoniemnamen]
  alles <- sort(c(functienamen, synoniemnamen))
  xref <- list()
  listcalls <- list()
  cat("get function calls\n")
  if (showprogress) pb <- txtProgressBar(style = 3)
  for (i in seq_along(functienamen)) {
    if (showprogress) setTxtProgressBar(pb, i / length(functienamen))
    f <- functienamen[i]
    calls <- dev_function_calls(
      parsedfiles[[functieparsed[f]]],
      functies[[f]]$def2
    )
    funcind <- calls %in% alles
    if (any(funcind)) {
      funccall <- calls[funcind]
      listcalls[[f]] <- funccall
      for (calledone in funccall) {
        xref[[calledone]] <- c(xref[[calledone]], f)
        if (any(calledone == synoniemnamen)) {
          # add also to synoniem
          calledfunc <- synoniemen[[calledone]]$name
          xref[[calledfunc]] <- c(xref[[calledfunc]], paste0(f, "|", calledone))
        }
      }
    }
  }
  if (showprogress) close(pb)
  for (i in seq_along(alles)) {
    f <- alles[i]
    if (is.null(listcalls[[f]])) {
      listcalls[[f]] <- character(0L)
    }
    if (is.null(xref[[f]])) xref[[f]] <- character(0L)
  }
  list(
    functies = functies,
    synoniemen = synoniemen,
    calls = listcalls,
    xrefs = xref
  )
}
#' Create a call tree for a function in the r files of a package
#'
#' This function creates a call tree for a function
#' in the r files of a package.
#'
#' @details
#' The function determines the functions in the package by
#' looking at the top-level assignments in the R files. Only
#' LEFT_ASSIGN \code{'<-'} symbols are looked at. Function calls
#' via \code{do.call} are recognized if the first parameter of the call
#' is positional and a symbol or a string constant (the function name).
#' Functions called via \code{eval} or function definitions
#' via \code{setMethod} are ignored!
#'
#' @param map a character item naming the directory of the package
#' @param func name(s) of the function for which
#' a calltree is demanded
#'
#' @return recursive list of functions with called functions,
#' when the called function also appears higher in the
#'  branch followed by ":RECURSIVE" and if the called function
#'   has no further (package) functions
#'  called followed by ":TERMINAL"
#'
#' @examples
#' \dontrun{
#' temp <- get_func_calltree("R", "get_xref")
#' str(temp)
#' }
#'
#' @author Luc De Wilde
#' @name get_func_calltree
#' @rdname get_func_calltree
#' @export
get_func_calltree <- function(map, func) {
  stopifnot(is.character(map), length(map) == 1)
  stopifnot(is.character(func), length(func) > 0)
  pkgname <- basename(normalizePath(map))
  ns <- getNamespace(pkgname)
  tmp <- dev_call_info(map, ns)
  functies <- tmp$functies
  synoniemen <- tmp$synoniemen
  listcalls <- tmp$calls
  calltree <- function(fname, parents) {
    fname2 <- fname
    if (any(fname == names(synoniemen))) {
      fname2 <- synoniemen[[f]]$name
    }
    if (any(parents == fname) || any(parents == fname2)) {
      return("RECURSIVE")
    }
    calls <- listcalls[[fname]]
    lijst <- list()
    for (i in seq_along(calls)) {
      calledf <- calls[i]
      lijst[[calledf]] <- calltree(calledf, c(parents, fname))
    }
    if (length(lijst) == 0L) {
      return("TERMINAL")
    }
    lijst
  }
  lijst <- list()
  for (i in seq_along(func)) {
    f <- func[i]
    funcind <- which(c(names(functies), names(synoniemen)) == f)
    if (length(funcind) == 0) {
      dev_warn(gettextf("function %s not found in R files!", f))
    } else {
      lijst[[f]] <- calltree(f, parents = character(0))
    }
  }
  class(lijst) <- c("calltree", class(lijst))
  lijst
}
#' Prints a calltree made with get_func_calltree()
#'
#' This function prints a call tree in a readable format.
#'
#' @param x a calltree created by get_func_calltree
#' @param ... other parameters, ignored here
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' temp <- get_func_calltree("R", "get_xref")
#' print(temp)
#' }
#'
#' @author Luc De Wilde
#' @name print.calltree
#' @rdname print.calltree
#' @export
print.calltree <- summary.calltree <- function(x, ...) {
  ddd <- list(...)
  if (is.null(ddd$envpc)) {
    ddd$envpc <- new.env(parent = emptyenv())
    assign("on_new_line", FALSE, envir = ddd$envpc)
  }
  if (is.null(ddd$pos)) {
    ddd$pos <- 0L
  }
  namen <- names(x)
  if (inherits(x, "character")) {
    cat(x, "\n", sep = "")
    assign("on_new_line", TRUE, envir = ddd$envpc)
    return()
  }
  i <- 1
  while (i <= length(namen)) {
    if (get("on_new_line", ddd$envpc)) {
      cat(strrep(" ", ddd$pos))
      assign("on_new_line", FALSE, envir = ddd$envpc)
    }
    cat(namen[i], ":", sep = "")
    print.calltree(
      x[[i]],
      envpc = ddd$envpc,
      pos = nchar(namen[i]) + 1L + ddd$pos
    )
    i <- i + 1L
  }
}

#' Create a list and html website with functions and calls between them
#' of a package
#'
#' This function creates a list and optionally an html website with all
#'  R functions of a package
#'
#' @details
#' The function determines the functions in the package by
#' looking at the top-level assignments in the R files. Only
#'  LEFT_ASSIGN \code{'<-'} symbols are looked at. Function calls
#' via \code{do.call} are recognized if the first parameter of the call
#' is positional and a symbol or a string constant (the function name).
#' Functions called via \code{eval} or function definitions
#' via \code{setMethod} are ignored!
#'
#' @param map the directory of the package
#' @param dest the directory where the html files should be written, optional
#'
#' @return a list with objects of class \code{\linkS4class{dev_func}} for
#' all top-level functions
#'
#' @examples
#' \dontrun{
#' temp <- calltree_html(".", "c:/temp/wwwcalltree")
#' }
#'
#' @author Luc De Wilde
#' @name calltree_html
#' @rdname calltree_html
#' @export
calltree_html <- function(map, dest = NULL) {
  if (missing(dest)) {
    html <- FALSE
  } else {
    html <- TRUE
    stopifnot(is.character(dest), length(dest) == 1L)
  }
  showprogress <- interactive() && !getOption("quiet", FALSE)
  return_value <- list()
  pkgname <- basename(normalizePath(map))
  exports <- getNamespaceExports(pkgname)
  ns <- getNamespace(pkgname)
  namestar <- function(a) {
    if (any(exports == a)) {
      return(paste(a, "*"))
    }
    a
  }
  xxx <- readLines(paste0(map, "/NAMESPACE"))
  s3 <- grep("^ *S3method\\(.*, .*\\) *$", xxx, value = TRUE)
  if (length(s3) > 0) {
    s3 <- gsub(
      "^ *S3m.*\\(.*,.*, *(.*)\\)",
      "exports <- c(exports, \"\\1\")",
      s3
    )
    s3 <- gsub(
      "^ *S3m.*\\(([^,]*), *([^)]*)\\).*$",
      "exports <- c(exports, \"\\1.\\2\")",
      s3
    )
    eval(str2expression(s3))
  }
  tmp <- dev_call_info(map, ns)
  functies <- tmp$functies
  synoniemen <- tmp$synoniemen
  listcalls <- tmp$calls
  xref <- tmp$xrefs
  if (html && !dir.exists(dest)) {
    dir.create(dest)
  }
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  alles <- sort(c(functienamen, synoniemnamen))
  functiecount <- length(functienamen)
  allescount <- length(alles)
  cat("get functions complexity\n")
  if (showprogress) pb <- txtProgressBar(style = 3)
  for (i in seq_along(functienamen)) {
    if (showprogress) setTxtProgressBar(pb, i / allescount)
    f <- functienamen[i]
    cycval <- cyclocomp(ns[[f]])
    devfunc <- new("dev_func",
      name             = f,
      defined_in       = functies[[f]]$src,
      defined_at_lines = c(functies[[f]]$def1[1], functies[[f]]$def2[3] +
        1L - functies[[f]]$def1[1]),
      exported         = any(exports == f),
      calls            = listcalls[[f]],
      called_by        = xref[[f]],
      synonym_of       = character(0L),
      complexity       = cycval
    )
    rval1 <- list(devfunc)
    names(rval1) <- f
    return_value <- c(return_value, rval1)
  }
  cycval <- 0L
  for (i in seq_along(synoniemnamen)) {
    if (showprogress) setTxtProgressBar(pb, (i + functiecount) / allescount)
    f <- synoniemnamen[i]
    funccall <- listcalls[[synoniemen[[f]]$name]]
    if (is.null(funccall)) funccall <- character(0)
    devfunc <- new("dev_func",
      name             = f,
      defined_in       = synoniemen[[f]]$src,
      defined_at_lines = c(synoniemen[[f]]$def1[1], 1L),
      exported         = any(exports == f),
      calls            = funccall,
      called_by        = xref[[f]],
      synonym_of       = synoniemen[[f]]$name,
      complexity       = cycval
    )
    rval1 <- list(devfunc)
    names(rval1) <- f
    return_value <- c(return_value, rval1)
  }
  if (showprogress) close(pb)
  if (html) {
    writeLines(index_html, paste0(dest, "/index.html"))
    writeLines(onefunc_html, paste0(dest, "/onefunc.html"))
    writeLines(ldwdev0_js, paste0(dest, "/ldwdev0.js"))
    capture.output({
      cat("const Functies = new Map([\n")
      for (i in seq_along(alles)) {
        synoniem <- 0
        devfunc <- return_value[[alles[i]]]
        if (length(devfunc@synonym_of)) synoniem <- which(alles == devfunc@synonym_of)
        cat(
          '[', i, ', new Functie(', i, ', "', alles[i], '", ',
          synoniem, ', "', devfunc@defined_in, '", ', 
          devfunc@defined_at_lines[1L], ', ', devfunc@defined_at_lines[2L], ', ',
          if (devfunc@exported) 'true' else 'false', ', [',
          paste(match(devfunc@calls, alles), collapse = ","),
          '], [',
          paste(match(sub("\\|.*$", "", devfunc@called_by), alles), collapse = ","),
          '], ', devfunc@complexity, ')]', sep = "")
        if (i == length(alles)) cat('\n') else cat(',\n')
      }
      cat("]);\n")
      cat('const PkgInfo = {name: "', pkgname, '", version: "',
          as.character(packageVersion(pkgname)),
          '", date: "',
          as.character(Sys.Date()),
          '"}\n',
          sep = ""
      )
    }, file = paste0(dest, "/ldwdev.js"))
  }
  browseURL(paste0(dest, "/index.html"))
  return_value
  }
