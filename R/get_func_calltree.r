#' The get_func_calltree function
#'
#' This function makes creates a call tree for a function in the r files of a package.
#'
#' @param map a character item naming the R map of the package
#' @param func name(s) of the function for which a calltree is demanded
#'
#' @return recursive list of functions with called functions, when the called function also appears higher in the
#'  branch followed by ":RECURSIVE" and if the called function has no further (package) functions
#'  called followed by ":TERMINAL"
#'
#' @examples
#' \dontrun{
#'  temp <- get_func_calltree("R", "get_xref")
#'  str(temp)
#' }
#'
#' @author Luc De Wilde
#' @name get_func_calltree
#' @rdname get_func_calltree
#' @export
get_func_calltree <- function(map, func) {
  stopifnot(is.character(map), length(map) == 1)
  stopifnot(is.character(func), length(func) > 0)
  files <- dir(map, pattern = "\\.[rR]$")
  if (length(files) == 0) return(list())
  if (map != ".") files <- paste0(map, "/", files)
  parsedfiles <- lapply(files, function(f) get_parsed(f))
  synoniemen <- list()
  functies <- list()
  functieparsed <- integer(0)
  for (i in seq_along(parsedfiles)) {
    parsedfile <- parsedfiles[[i]]
    tmp <- get_toplevel(parsedfile)
    synoniemen <- c(synoniemen, tmp$synoniemen)
    functies <- c(functies, tmp$functies)
    pfiles <- rep.int(i, length(tmp$functies))
    names(pfiles) <- names(tmp$functies)
    functieparsed <- c(functieparsed, pfiles)
  }
  calltree <- function(fname, parents) {
    while (any(fname == names(synoniemen))) fname <- synoniemen[[f]]
    if (any(parents == fname)) return("RECURSIVE")
    calls <- get_function_calls(parsedfiles[[functieparsed[fname]]], functies[[fname]])
    lijst <- list()
    for (i in seq_along(calls)) {
      calledf <- calls[i]
      while (any(calledf == names(synoniemen))) calledf <- synoniemen[[calledf]]
      if (any(calledf == names(functies))) {
        lijst[[calledf]] <- calltree(calledf, c(parents, fname))
      }
    }
    if (length(lijst) == 0L) return("TERMINAL")
    return(lijst)
  }
  lijst <- list()
  for (i in seq_along(func)) {
    f <- func[i]
    while (any(f == names(synoniemen))) f <- synoniemen[[f]]
    funcind <- which(names(functies) == f)
    if (length(funcind) == 0) {
      warning("functie ", func[i], " niet gevonden in de R-files")
    } else {
      lijst[[func[i]]] <- calltree(f, parents = character(0))
    }
  }
  class(lijst) <- c("calltree", class(lijst))
  lijst
}
#' The print.calltree function
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
#'  temp <- get_func_calltree("R", "get_xref")
#'  print(temp)
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
  if (is.null(ddd$pos)) ddd$pos <- 0L
  namen <- names(x)
  if (inherits(x, "character")) {
    cat(x, "\n", sep="")
    assign("on_new_line", TRUE, envir = ddd$envpc)
    return()
  }
  i <- 1
  while (i <= length(namen)) {
    if (get("on_new_line", ddd$envpc)) {
      cat(strrep(" ", ddd$pos))
      assign("on_new_line", FALSE, envir = ddd$envpc)
    }
    cat(namen[i],":",sep="")
    print.calltree(x[[i]], envpc = ddd$envpc, pos = nchar(namen[i]) + 1L + ddd$pos)
    i <- i + 1L
  }
}
#' The calltree_hmtl function
#'
#' This function creates a html website with all R functions of a package
#'
#' @param map the R directory of the package
#' @param dest the directory where the html files should be written
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'  temp <- calltree_html("R", "c:\temp\wwwcalltree")
#'  print(temp)
#' }
#'
#' @author Luc De Wilde
#' @name calltree_hmtl
#' @rdname calltree_hmtl
#' @export
calltree_html <- function(map, dest) {
  stopifnot(is.character(map), length(map) == 1L)
  stopifnot(is.character(dest), length(dest) == 1L)
  if (!dir.exists(dest)) dir.create(dest)
  files <- dir(map, pattern = "\\.[rR]$")
  if (length(files) == 0) return(list())
  if (map != ".") files <- paste0(map, "/", files)
  parsedfiles <- lapply(files, function(f) get_parsed(f))
  synoniemen <- list()
  functies <- list()
  functieparsed <- integer(0)
  for (i in seq_along(parsedfiles)) {
    parsedfile <- parsedfiles[[i]]
    tmp <- get_toplevel(parsedfile)
    synoniemen <- c(synoniemen, tmp$synoniemen)
    functies <- c(functies, tmp$functies)
    pfiles <- rep.int(i, length(tmp$functies))
    names(pfiles) <- names(tmp$functies)
    functieparsed <- c(functieparsed, pfiles)
  }
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  alles <- sort(c(functienamen, synoniemnamen))
  xref <- list()
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    while (any(f == names(synoniemen))) f <- synoniemen[[f]]
    calls <- get_function_calls(parsedfiles[[functieparsed[f]]], functies[[f]])
    funcind <- calls %in% functienamen
    if (any(funcind)) {
      funccall <- calls[funcind]
      for (calledone in funccall) {
        xref[[calledone]] <- c(xref[[calledone]], f)
      }
    }
  }
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    while (any(f == names(synoniemen))) f <- synoniemen[[f]]
    calls <- get_function_calls(parsedfiles[[functieparsed[f]]], functies[[f]])
    sink(paste0(dest, "/", f, ".html"))
    cat("<!DOCTYPE html>
      <html>
      <head>
      <title>",
        f,
      "</title>
      </head>
      <style>
div.mycontainer {
  width:95%;
  overflow:auto;
}
div.mycontainer div {
  width:45%;
  float:left;
}
</style>
      <body>
      <h1>",
      f,
      "</h1>
<div class=\"mycontainer\">
  <div style=\"background-color:#FFF4A3;\">
  <h2>Functions called</h2>
  ")
    funcind <- calls %in% functienamen
    if (any(funcind)) {
      funccall <- calls[funcind]
      for (j in seq_along(funccall)) {
        cat("<a href=\"", funccall[j], ".html\">", funccall[j], "</a><br />\n", sep = "")
      }
    } else {
      cat("none")
    }
    cat("</div>
  <div style=\"background-color:#D9EEE1;\">
  <h2>Called by functions</h2>
  ")
    if (is.null(xref[[f]])) {
      cat("none")
    } else {
      for (j in seq_along(xref[[f]])) {
        caller <- xref[[f]][j]
        cat("<a href=\"", caller, ".html\">", caller, "</a><br />\n", sep = "")
      }
    }
    cat("</div>
        </div>
        </body>\n</html>\n")
    sink()
  }
  for (i in seq_along(synoniemnamen)) {
    f <- synoniemnamen[i]
    sink(paste0(dest, "/", f, ".html"))
    cat("<!DOCTYPE html>
      <html>
      <head>
      <title>",
        f,
        "</title>
      </head>
      <body>
      <h1>",
        f,
        "</h1>\n")
    cat("This function is the same as <a href=\"",
          synoniemen[[f]], ".html\">", synoniemen[[f]], "</a><br />\n", sep = "")
    cat("</body>\n</html>\n")
    sink()
  }
  alles <- sort(c(functienamen, synoniemnamen))
  sink(paste0(dest, "/index.html"))
  cat("<!DOCTYPE html>
      <html>
      <head>
      <title>all functions</title>
      </head>
      <body>
      <h1>All functions in ",
      map,
      ".</h1>\n")
  for (i in seq_along(alles)) {
    cat("<a href=\"", alles[i], ".html\", target=\"_blank\">", alles[i], "</a><br />\n", sep = "")
  }
  cat("</body>\n</html>\n")
  sink()
}
