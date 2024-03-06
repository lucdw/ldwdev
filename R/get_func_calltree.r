get_call_info <- function(map) {
  stopifnot(is.character(map), length(map) == 1L)
  files <- dir(map, pattern = "\\.[rR]$")
  if (length(files) == 0 && dir.exists(paste0(map, "/R"))) {
    map <- paste0(map, "/R")
    files <- dir(map, pattern = "\\.[rR]$")
  }
  if (length(files) == 0) stop("No R files found!")
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
  listcalls <- list()
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    calls <- get_function_calls(parsedfiles[[functieparsed[f]]],
                                functies[[f]]$def2)
    funcind <- calls %in% alles
    if (any(funcind)) {
      funccall <- calls[funcind]
      listcalls[[f]] <- funccall
      for (calledone in funccall) {
        xref[[calledone]] <- c(xref[[calledone]], f)
        if (any(calledone == synoniemnamen)) { # add also to synoniem
          calledfunc <- synoniemen[[calledone]]$name
          xref[[calledfunc]] <- c(xref[[calledfunc]],
                                  paste0(f, "|", calledone))
        }
      }
    }
  }
  for (i in seq_along(alles)) {
    f <- alles[i]
    if (is.null(listcalls[[f]])) listcalls[[f]] <- character(0L)
    if (is.null(xref[[f]])) xref[[f]] <- character(0L)
  }
  return(list(functies = functies, synoniemen = synoniemen,
              calls = listcalls, xrefs = xref))
}
#' The get_func_calltree function
#'
#' This function makes creates a call tree for a function
#' in the r files of a package.
#'
#' @details
#' The function determines the functions in the package by
#' looking at the top-level assignments in the R files. Only
#' LEFT_ASSIGN \code{'<-'} symbols are looked at. Function calls
#' via \code{do.call} or function definitions
#' via \code{setMethod} are ignored!
#'
#' @param map a character item naming the directory
#' or R-directory of the package
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
  tmp <- get_call_info(map)
  functies <- tmp$functies
  synoniemen <- tmp$synoniemen
  listcalls <- tmp$calls
  calltree <- function(fname, parents) {
    fname2 <- fname
    if (any(fname == names(synoniemen))) fname2 <- synoniemen[[f]]$name
    if (any(parents == fname) || any(parents == fname2)) return("RECURSIVE")
    calls <- listcalls[[fname]]
    lijst <- list()
    for (i in seq_along(calls)) {
      calledf <- calls[i]
      lijst[[calledf]] <- calltree(calledf, c(parents, fname))
    }
    if (length(lijst) == 0L) return("TERMINAL")
    return(lijst)
  }
  lijst <- list()
  for (i in seq_along(func)) {
    f <- func[i]
    funcind <- which(c(names(functies), names(synoniemen)) == f)
    if (length(funcind) == 0) {
      warning("functie ", f, " niet gevonden in de R-files")
    } else {
      lijst[[f]] <- calltree(f, parents = character(0))
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
    print.calltree(x[[i]], envpc = ddd$envpc,
                   pos = nchar(namen[i]) + 1L + ddd$pos)
    i <- i + 1L
  }
}
#' The calltree_html function
#'
#' This function creates a html website with all R functions of a package
#'
#' @details
#' The function determines the functions in the package by
#' looking at the top-level assignments in the R files. Only
#'  LEFT_ASSIGN \code{'<-'} symbols are looked at. Function calls
#' via \code{do.call} or function definitions via \code{setMethod}
#' are ignored!
#'
#' @param map the directory or R-directory of the package
#' @param dest the directory where the html files should be written
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'  temp <- calltree_html("R", "c:/temp/wwwcalltree")
#' }
#'
#' @author Luc De Wilde
#' @name calltree_html
#' @rdname calltree_html
#' @export
calltree_html <- function(map, dest) {
  stopifnot(is.character(dest), length(dest) == 1L)
  tmp <- get_call_info(map)
  functies <- tmp$functies
  synoniemen <- tmp$synoniemen
  listcalls <- tmp$calls
  xref <- tmp$xrefs
  if (!dir.exists(dest)) dir.create(dest)
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  htmlnamen <- list()
  for (i in seq_along(functienamen))
    htmlnamen[[functienamen[i]]] <- paste0("f", i)
  for (i in seq_along(synoniemnamen))
    htmlnamen[[synoniemnamen[i]]] <- paste0("s", i)
  alles <- sort(c(functienamen, synoniemnamen))
  for (i in seq_along(functienamen)) {
    f <- functienamen[i]
    funccall <- listcalls[[f]]
    sink(paste0(dest, "/", htmlnamen[[f]], ".html"))
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
"</h1><p>defined on line ",
functies[[f]]$def1[1],
" of ",
functies[[f]]$src,
"</p><div class=\"mycontainer\">
  <div style=\"background-color:#FFF4A3;\">
  <h2>Functions called</h2>
  ")
    if (length(funccall) > 0L) {
      for (j in seq_along(funccall)) {
        cat("<a href=\"", htmlnamen[[funccall[j]]], ".html\">",
            funccall[j], "</a><br />\n", sep = "")
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
        callers <- strsplit(caller, "|", fixed = TRUE)[[1]]
        if (length(callers) == 2L) {
          cat("<a href=\"", htmlnamen[[callers[1]]], ".html\">",
              callers[1], "</a> via ",
              "<a href=\"", htmlnamen[[callers[2]]], ".html\">",
              callers[2], "</a><br />\n",
              sep = "")
        } else {
          cat("<a href=\"", htmlnamen[[caller]], ".html\">",
              caller, "</a><br />\n", sep = "")
        }
      }
    }
    cat("</div>
        </div>
        </body>\n</html>\n")
    sink()
  }
for (i in seq_along(synoniemnamen)) {
  f <- synoniemnamen[i]
  funccall <- listcalls[[synoniemen[[f]]$name]]
  sink(paste0(dest, "/", htmlnamen[[f]], ".html"))
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
      "</h1>\n<p>defined on line ",
      synoniemen[[f]]$def1[1],
      " of ",
      synoniemen[[f]]$src,
      "</p>\n")
  cat("<p>This function is the same as <a href=\"",
      htmlnamen[[synoniemen[[f]]$name]], ".html\">",
      synoniemen[[f]]$name, "</a></p>\n", sep = "")
  cat("</p><div class=\"mycontainer\">
  <div style=\"background-color:#FFF4A3;\">
  <h2>Functions called (copied from ", synoniemen[[f]]$name, ")</h2>
  ")
  if (length(funccall) > 0L) {
    for (j in seq_along(funccall)) {
      cat("<a href=\"", htmlnamen[[funccall[j]]], ".html\">",
          funccall[j], "</a><br />\n", sep = "")
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
      cat("<a href=\"", htmlnamen[[caller]], ".html\">", caller,
          "</a><br />\n", sep = "")
    }
  }
  cat("</div>
        </div>
        </body>\n</html>\n")
  sink()
}
sink(paste0(dest, "/index.html"))
cat("<!DOCTYPE html>
      <html>
      <head>
      <title>all functions</title>
      </head>
      <body>
      <h1>All functions in ",
    map,
    ".</h1>\n<table>\n<tr><th>Name</th><th># calls</th><th># called by</th>",
    "<th>synonym of</th><th>defined in</th><th>at line</th></tr>\n")
for (i in seq_along(alles)) {
  cat("<tr><td><a href=\"", htmlnamen[[alles[i]]],
      ".html\", target=\"_blank\">",
      alles[i], "</a></td><td>", length(listcalls[[alles[i]]]),
      "</td><td>",
      length(xref[[alles[i]]]), "</td><td>", sep = "")
  if (any(alles[i] == synoniemnamen)) {
    cat(synoniemen[[alles[i]]]$name, "</td><td>",
        synoniemen[[alles[i]]]$src, "</td><td>",
        synoniemen[[alles[i]]]$def1[1])
  } else {
    cat("</td><td>", functies[[alles[i]]]$src, "</td><td>",
        functies[[alles[i]]]$def1[1])
  }
  cat("</td></tr>\n")
}
cat("</table></body>\n</html>\n")
sink()
browseURL(paste0(dest, "/index.html"))
}
