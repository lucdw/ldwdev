#' @importFrom cyclocomp cyclocomp
#' @importFrom utils packageVersion capture.output
#' @importFrom methods new
#' @importFrom utils setTxtProgressBar txtProgressBar
dev_function_calls <- function(parseddata, range) {
  stopifnot(!missing(range), length(range) == 4, is.integer(range))
  frompos <- 10000L * range[1] + range[2]
  topos <- 10000L * range[3] + range[4]
  fcrows <- which(
    parseddata$token == "SYMBOL_FUNCTION_CALL" &
    10000L * parseddata$line1 + parseddata$col1 >= frompos &
    10000L * parseddata$line2 + parseddata$col2 <= topos
  )
  for (i in seq_len(nrow(parseddata))) {
    # symbols which are no slot or list element
    if (parseddata$token[i] == "SYMBOL" && parseddata$text[i - 1L] != "$" &&
      parseddata$text[i - 1L] != "@" && parseddata$text[i + 1] != "=" &&
      parseddata$token[i + 1L] != "LEFT_ASSIGN" &&
      10000L * parseddata$line1[i] + parseddata$col1[i] >= frompos &&
      10000L * parseddata$line2[i] + parseddata$col2[i] <= topos
    ) fcrows <- c(fcrows, i)
  }
  docalls <- which(
    parseddata$token == "SYMBOL_FUNCTION_CALL" &
      10000L * parseddata$line1 + parseddata$col1 >= frompos &
      10000L * parseddata$line2 + parseddata$col2 <= topos &
      parseddata$text == "do.call"
  )
  docallfuncs <- character(0)
  for (j in docalls) {
    if (
      parseddata$token[j + 3] == "STR_CONST"
    ) {
      docallfuncs <- c(docallfuncs, parseddata$text[j + 3])
    }
  }
  sort(unique(c(docallfuncs, parseddata$text[fcrows])))
}

dev_call_info <- function(map, ns) {
  stopifnot(is.character(map), length(map) == 1L)
  showprogress <- interactive() && !getOption("quiet", FALSE)
  nsfunc <- sapply(ls(ns), \(x) is.function(ns[[x]]))
  nsfunctions <- ls(ns)[nsfunc]
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
  synoniemen <- list()
  functies <- list()
  calls <- list()
  complexities <- list()
  cat("get R source info\n")
  if (showprogress) pb <- txtProgressBar(style = 3)
  for (i in seq_along(files)) {
    if (showprogress) setTxtProgressBar(pb, i / length(files))
    parsedfile <- dev_parsed(files[i])
    tmp <- dev_toplevel(parsedfile)
    tmp$functies <- tmp$functies[names(tmp$functies) %in% nsfunctions]
    tmp$synoniemen <- tmp$synoniemen[names(tmp$synoniemen) %in% nsfunctions]
    for (i in seq_along(tmp$functies)) {
      f <- tmp$functies[[i]]
      fname <- names(tmp$functies)[i]
      calls[[fname]] <- dev_function_calls(
        parsedfile, f$def2
      )
      complexities[[fname]] <- cyclocomp(ns[[fname]])
    }
    for (fname in names(tmp$synoniemen)) {
      complexities[[fname]] <- cyclocomp(ns[[fname]])
    }
    synoniemen <- c(synoniemen, tmp$synoniemen)
    functies <- c(functies, tmp$functies)
  }
  if (showprogress) close(pb)
  synoniemnamen <- names(synoniemen)
  functienamen <- names(functies)
  alles <- sort(c(functienamen, synoniemnamen))
  xref <- list()
  listcalls <- list()
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    callsf <- calls[[f]]
    funcind <- callsf %in% alles
    if (any(funcind)) {
      funccall <- callsf[funcind]
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
    xrefs = xref,
    complexities = complexities
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
  return_value <- list()
  pkgname <- basename(normalizePath(map))
  exports <- getNamespaceExports(pkgname)
  ns <- getNamespace(pkgname)
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
  if (html && !dir.exists(dest)) {
    dir.create(dest)
  }
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  alles <- sort(c(functienamen, synoniemnamen))
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    cycval <- tmp$complexities[[f]]
    devfunc <- new("dev_func",
      name             = f,
      defined_in       = functies[[f]]$src,
      defined_at_lines = c(functies[[f]]$def1[1], functies[[f]]$def2[3] +
        1L - functies[[f]]$def1[1]),
      exported         = any(exports == f),
      calls            = tmp$calls[[f]],
      called_by        = tmp$xrefs[[f]],
      synonym_of       = character(0L),
      complexity       = cycval
    )
    rval1 <- list(devfunc)
    names(rval1) <- f
    return_value <- c(return_value, rval1)
  }
  for (i in seq_along(synoniemnamen)) {
    f <- synoniemnamen[i]
    cycval <- tmp$complexities[[f]]
    funccall <- tmp$calls[[synoniemen[[f]]$name]]
    if (is.null(funccall)) funccall <- character(0)
    devfunc <- new("dev_func",
      name             = f,
      defined_in       = synoniemen[[f]]$src,
      defined_at_lines = c(synoniemen[[f]]$def1[1], 1L),
      exported         = any(exports == f),
      calls            = funccall,
      called_by        = tmp$xrefs[[f]],
      synonym_of       = synoniemen[[f]]$name,
      complexity       = cycval
    )
    rval1 <- list(devfunc)
    names(rval1) <- f
    return_value <- c(return_value, rval1)
  }
  if (html) {
    writeLines(index_html, paste0(dest, "/index.html"))
    writeLines(onefunc_html, paste0(dest, "/onefunc.html"))
    writeLines(ldwdev0_js, paste0(dest, "/ldwdev0.js"))
    capture.output({
      cat("const Functies = new Map([\n")
      for (i in seq_along(alles)) {
        synoniem <- 0
        devfunc <- return_value[[alles[i]]]
        if (length(devfunc@synonym_of)) {
          synoniem <- which(alles == devfunc@synonym_of)
        }
        cat(
          '[', i, ', new Functie(', i, ', "', alles[i], '", ',
          synoniem, ', "', devfunc@defined_in, '", ',
          devfunc@defined_at_lines[1L], ', ',
          devfunc@defined_at_lines[2L], ', ',
          if (devfunc@exported) 'true' else 'false', ', [',
          paste(match(devfunc@calls, alles), collapse = ","),
          '], [',
          paste(match(sub("\\|.*$", "", devfunc@called_by), alles),
                                                    collapse = ","),
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
