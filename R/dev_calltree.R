#' @importFrom cyclocomp cyclocomp
#' @importFrom utils packageVersion
#' @importFrom methods new
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
dev_call_info <- function(map) {
  stopifnot(is.character(map), length(map) == 1L)
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
  for (i in seq_along(parsedfiles)) {
    parsedfile <- parsedfiles[[i]]
    tmp <- dev_toplevel(parsedfile)
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
dev_indir_xrefs <- function(xrefs) {
  dir_xrefs <- lapply(xrefs, function(f) {
    a <- unlist(strsplit(f, "|", TRUE))
    unique(a)
  })
  add_xref <- function(x, curlist) {
    dxx <- dir_xrefs[[x]]
    if (length(dxx) == 0L || all(dxx == x)) {
      return(curlist)
    }
    cl <- unique(c(curlist, dxx))
    for (xx in setdiff(dxx, curlist)) {
      cl <- add_xref(xx, cl)
    }
    unique(cl)
  }
  all.xrefs <- lapply(names(dir_xrefs), function(x) add_xref(x, x))
  indir_xrefs <- lapply(seq_along(xrefs), function(j) {
    setdiff(all.xrefs[[j]], c(names(xrefs)[j], dir_xrefs[[j]]))
  })
  names(indir_xrefs) <- names(xrefs)
  indir_xrefs
}
dev_indir_calls <- function(calls) {
  add_call <- function(x, curlist) {
    dxx <- calls[[x]]
    if (length(dxx) == 0L || all(dxx == x)) {
      return(curlist)
    }
    cl <- unique(c(curlist, dxx))
    for (xx in setdiff(dxx, curlist)) {
      cl <- add_call(xx, cl)
    }
    unique(cl)
  }
  all.calls <- lapply(names(calls), function(x) add_call(x, x))
  indir_calls <- lapply(seq_along(calls), function(j) {
    setdiff(all.calls[[j]], c(names(calls)[j], calls[[j]]))
  })
  names(indir_calls) <- names(calls)
  indir_calls
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
  tmp <- dev_call_info(map)
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

dev_write_function_html <- function(pkgname, dev_func_obj,
                                    indir_xrefs, indir_calls, namestar, dest) {
sink(paste0(dest, "/", dev_htmlnaam(dev_func_obj@name)))
cat(
"<!DOCTYPE html>
<html>
<head>
<title>",
      pkgname,
      " ",
      dev_func_obj@name,
      "</title>
<style>
h1 {
 color: #1e64c8;
}
p.lijst {
 margin-top: 0.5em;
 margin-bottom: 0.5em;
}
p.lijst:nth-child(even) {
  background-color: #dddddd;
}
div.mycontainer {
  width:99%;
  overflow:auto;
}
div.mycontainer div {
  width:23%;
  float:left;
  border-style: solid;
  border-width: 5px;
  margin: 4px;
}
</style>
</head>
<body>
<h1>",
    dev_func_obj@name,
    "</h1><p>defined on line ", dev_func_obj@defined_at_lines[1L],
    " of ", dev_func_obj@defined_in,
    ", ", dev_func_obj@defined_at_lines[2],
    " lines",
    "</p>\n"
)
  if (length(dev_func_obj@synonym_of) > 0L) {
    cat(
    "<p>This function is the same as <a href=\"",
    dev_htmlnaam(dev_func_obj@synonym_of),
    "\">",
    dev_func_obj@synonym_of,
    "</a></p>\n",
    sep = ""
  )
  }
cat("<div class=\"mycontainer\">
<div style=\"border-color:#dddd00;\">
<h2>Calls functions</h2>
"
)
if (length(dev_func_obj@calls) > 0L) {
  for (j in seq_along(dev_func_obj@calls)) {
    cat(
      "<p class='lijst'><a href='",
      dev_htmlnaam(dev_func_obj@calls[j]),
      "'>",
      namestar(dev_func_obj@calls[j]),
      "</a></p>\n",
      sep = ""
    )
  }
} else {
  cat("none")
}
cat(
  "</div>
<div style=\"border-color:#ffff00;\">
<h2>Calls indirectly functions</h2>
"
)
if (is.null(indir_calls[[dev_func_obj@name]]) ||
  length(indir_calls[[dev_func_obj@name]]) == 0L) {
  cat("none")
} else {
  for (j in seq_along(indir_calls[[dev_func_obj@name]])) {
    calledone <- indir_calls[[dev_func_obj@name]][j]
    cat(
      "<p class='lijst'><a href='",
      dev_htmlnaam(calledone),
      "'>",
      namestar(calledone),
      "</a></p>\n",
      sep = ""
    )
  }
}
cat(
  "</div>
<div style=\"border-color:#00dddd;\">
<h2>Called by</h2>
"
)
if (is.null(dev_func_obj@called_by)) {
  cat("none")
} else {
  for (j in seq_along(dev_func_obj@called_by)) {
    caller <- dev_func_obj@called_by[j]
    callers <- strsplit(caller, "|", fixed = TRUE)[[1]]
    if (length(callers) == 2L) {
      cat(
        "<p class='lijst'><a href='",
        dev_htmlnaam(callers[1]),
        "'>",
        callers[1],
        "</a> via ",
        "<a href='",
        dev_htmlnaam(callers[2]),
        "'>",
        callers[2],
        "</a></p>\n",
        sep = ""
      )
    } else {
      cat(
        "<p class='lijst'><a href='",
        dev_htmlnaam(caller),
        "'>",
        namestar(caller),
        "</a></p>\n",
        sep = ""
      )
    }
  }
}
cat(
  "</div>
<div style=\"border-color:#00ffff;\">
<h2>Called indirectly by</h2>
"
)
if (is.null(indir_xrefs[[dev_func_obj@name]]) ||
  length(indir_xrefs[[dev_func_obj@name]]) == 0L) {
  cat("none")
} else {
  for (j in seq_along(indir_xrefs[[dev_func_obj@name]])) {
    caller <- indir_xrefs[[dev_func_obj@name]][j]
    cat(
      "<p class='lijst'><a href='",
      dev_htmlnaam(caller),
      "'>",
      namestar(caller),
      "</a></p>\n",
      sep = ""
    )
  }
}
cat(
  "</div>
    </div>
    </body>\n</html>\n"
)
sink()
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
  tmp <- dev_call_info(map)
  functies <- tmp$functies
  synoniemen <- tmp$synoniemen
  listcalls <- tmp$calls
  xref <- tmp$xrefs
  indir_xrefs <- dev_indir_xrefs(xref)
  indir_calls <- dev_indir_calls(listcalls)
  if (html && !dir.exists(dest)) {
    dir.create(dest)
  }
  functienamen <- names(functies)
  synoniemnamen <- names(synoniemen)
  alles <- sort(c(functienamen, synoniemnamen))
  functiecount <- length(functienamen)
  allescount <- length(alles)
  if (interactive()) cat("\n")
  for (i in seq_along(functienamen)) {
    if (interactive()) cat("\rget functions info",
      format((100L * i) %/% allescount, width = 4L), "%")
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
    if (html)
      dev_write_function_html(pkgname, devfunc, indir_xrefs, indir_calls, namestar, dest)
  }
  cycval <- 0L
  for (i in seq_along(synoniemnamen)) {
    if (interactive()) cat("\rget functions info",
      format((100L * (i + functiecount)) %/% allescount, width = 4L), "%")
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
    if (html)
      dev_write_function_html(pkgname, devfunc, indir_xrefs, indir_calls, namestar, dest)
  }
  if (interactive()) cat("\n")
  if (!html) return(return_value)
  ################## write index.html #######################
  sink(paste0(dest, "/index.html"))
  cat(
    "<!DOCTYPE html>
<html>
<head>
<title>",
    pkgname,
    " functions calltree</title>
<style>
h1 {
 color: #1e64C8;
}
table {
  font-family: arial, sans-serif;
  border-collapse: collapse;
  width: 100%;
}

td, th {
  border: 1px solid #dddddd;
  text-align: left;
  padding: 6px;
}

tr:nth-child(even) {
  background-color: #dddddd;
}
</style>
      </head>
      <body>
      <h1>Functions in ",
    pkgname,
    " version ",
    as.character(packageVersion(pkgname)),
    " - ",
    as.character(Sys.Date()),
    ".</h1><p>Click on a column header to sort on that column.
    <span id='sorting' style='background-color: green; visibility: hidden;'>
    s o r t i n g</span></p>
\n<table id='myTable'>\n<tr>
<th onclick='sortTable(0)'>Name (*=exported)</th>
<th onclick='sortTable(1)'># calls</th>,
<th onclick='sortTable(2)'># called by</th>
<th onclick='sortTable(3)'>synonym of</th>
<th onclick='sortTable(4)'>defined in</th>
<th onclick='sortTable(5)'>at line</th>
<th onclick='sortTable(6)'># lines</th>
<th onclick='sortTable(7)'>complexity</th>
</tr>\n",
    sep = ""
  )
  for (i in seq_along(alles)) {
    cat(
      "<tr><td><a href=\"",
      dev_htmlnaam(alles[i]),
      "\", target=\"_blank\">",
      namestar(alles[i]),
      sep = ""
    )
    cat(
      "</a></td><td>",
      length(listcalls[[alles[i]]]),
      "</td><td>",
      length(xref[[alles[i]]]),
      "</td><td>",
      sep = ""
    )
    if (any(alles[i] == synoniemnamen)) {
      cat(
        synoniemen[[alles[i]]]$name,
        "</td><td>",
        synoniemen[[alles[i]]]$src,
        "</td><td>",
        synoniemen[[alles[i]]]$def1[1],
        "</td><td></td><td>"
      )
    } else {
      cat(
        "</td><td>",
        functies[[alles[i]]]$src,
        "</td><td>",
        functies[[alles[i]]]$def1[1],
        "</td><td>",
        functies[[alles[i]]]$def2[3] + 1 - functies[[alles[i]]]$def1[1],
        "</td><td>"
      )
      cat(cyclocomp(ns[[alles[i]]]))
    }
    cat("</td></tr>\n")
  }
  cat("</table>\n")
  cat(
    "
<script>
columnIndex = 0;
function sortTable(column = 0) {
  document.getElementById('sorting').style.visibility='visible';
  columnIndex = column;
  window.setTimeout(sortTable2, 250);
}
lastsort = -1;
lastasc = false;
function sortTable2() {
   const table = document.getElementById('myTable');
   const rows = Array.from(table.rows).slice(1); // Exclude header row
   ascending = true;
   if (lastsort == columnIndex) ascending = !lastasc;
   rows.sort((a, b) => {
       const cellA = a.cells[columnIndex].innerText.toLowerCase();
       const cellB = b.cells[columnIndex].innerText.toLowerCase();
       cellAnum = Number(cellA)
       cellBnum = Number(cellB)
       if (Number.isNaN(cellAnum) || Number.isNaN(cellBnum)) {
          if (cellA < cellB) return ascending ? -1 : 1;
          if (cellA > cellB) return ascending ? 1 : -1;
          return 0;
       } else {
          if (cellAnum < cellBnum) return ascending ? -1 : 1;
          if (cellAnum > cellBnum) return ascending ? 1 : -1;
          return 0;
       }
   });
   rows.forEach(row => table.appendChild(row)); // Reorder rows
   lastsort = columnIndex;
   lastasc = ascending;
  document.getElementById('sorting').style.visibility='hidden';
}
</script>
    "
  )
  cat("</body>\n</html>\n")
  sink()
  browseURL(paste0(dest, "/index.html"))
  return_value
}
