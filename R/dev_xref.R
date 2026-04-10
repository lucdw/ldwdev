#' Gets a cross-reference of variables used in top-level defined functions
#'
#' This function creates a cross-reference of the variables for
#' all functions defined on the top-level in .R-files.
#'
#' @param file a character vector naming the files to search in
#'
#' @return named list with for each file a named list with for each found top-level
#'  function in the file a named list with for each variable in the function a dev_var object
#'
#' @examples
#' filename <- tempfile(fileext = ".R")
#' writeLines(c(
#'  "f1 <- function(a, s3list, s4obj, ...) {",
#'  " b <- paste(\"sum:\", s3list$teller + s4obj@teller)",
#'  " return(paste0(a, b))",
#'  "}"
#' ), filename)
#' tmp <- get_xref(filename)
#' for (ifile in seq_along(tmp)) {
#'   cat(names(tmp)[ifile], "\n")
#'   thefile <- tmp[[ifile]]
#'   for (ifunc in seq_along(thefile)) {
#'     cat(" ", names(thefile)[ifunc], "\n")
#'     thefunc <- thefile[[ifunc]]
#'     for (v in thefunc) {
#'       cat("   ", v@name)
#'       if (v@argument) cat(" is an argument of the function.")
#'       cat("\n    References: ")
#'       for (ref in v@xrefs) {
#'         cat(ref@line, ":", ref@position, if (ref@modified) "*", " | ", sep = "")
#'       }
#'       cat("\n")
#'     }
#'   }
#' }
#' unlink(filename)
#'
#' @author Luc De Wilde
#' @name get_xref
#' @rdname get_xref
#' @export
get_xref <- function(file = "") {
  stopifnot(is.character(file), length(file) > 0L)
  retval <- list()
  for (f in file) {
    retval[[f]] <- list()
    parseddata <- dev_parsed(f)
    exprs <- parseddata$id[parseddata$parent == 0 & parseddata$token == "expr"]
    for (i in seq_along(exprs)) {
      subs <- parseddata[parseddata$parent == exprs[i], ]
      asg <- which(subs$token == "LEFT_ASSIGN")
      for (j in asg) {
        if (
          j > 1L &&
            j < length(subs) &&
            subs$token[j - 1L] == "expr" &&
            subs$token[j + 1L] == "expr"
        ) {
          subs1 <- parseddata[parseddata$parent == subs$id[j - 1L], ]
          subs2 <- parseddata[parseddata$parent == subs$id[j + 1L], ]
          if (
            nrow(subs1) == 1L &&
              subs1$token[1L] == "SYMBOL" &&
              subs2$token[1L] == "FUNCTION"
          ) {
            # found a function on top-level
            funcname <- subs1$text
            env <- new.env(parent = emptyenv())
            dev_get_vars(subs2, parseddata, env, TRUE)
            retval[[f]][[funcname]] <- as.list(env, all.names = TRUE, sorted = TRUE)
          }
        }
      }
    }
  }
  retval
}
dev_get_vars <- function(subs, parseddata, env, first, ismodified) {
  if (first) {
    symbolformals <- which(subs$token == "SYMBOL_FORMALS")
    for (k in symbolformals) {
      loc <- new("dev_ref", line = subs$line1[k], position = subs$col1[k], modified = FALSE)
      var <- new("dev_var", name = subs$text[k], argument = TRUE,
        xrefs = list(loc))
      assign(var@name, var, env)
    }
  }
  for (jj in seq_along(subs$token)) {
    skipthis <- FALSE
    breakafter <- FALSE
    LA <- which(subs$token == "LEFT_ASSIGN")
    if (length(LA) == 0L) LA <- 0L
    if (subs$token[jj] == "SYMBOL") {
      vname <- subs$text[jj]
      loc <- new("dev_ref", line = subs$line1[jj], position = subs$col1[jj], modified = ismodified)
    } else if (subs$terminal[jj] == FALSE && any(subs$text %in% c("$", "@"))) {
      found <- FALSE
      curj <- subs$id[jj]
      while (!found) {
        sss <- parseddata[parseddata$parent == curj, ]
        curj <- sss$id[1L]
        found <- sss$terminal[1L] == TRUE
      }
      if (sss$token == "SYMBOL") {
        vname <- sss$text[1L]
        loc <- new("dev_ref", line = sss$line1[1L], position = sss$col1[1L], modified = ismodified)
        breakafter <- TRUE
      } else {
        skipthis <- TRUE
      }
    } else {
      if (subs$token[jj] %in% c("expr", "forcond")) {
        subsubs <- parseddata[parseddata$parent == subs$id[jj], ]
        dev_get_vars(subsubs, parseddata, env, FALSE, jj < LA[1L])
      }
      skipthis <- TRUE
      }
    if (!skipthis) {
      var <- get0(vname, env)
      if (is.null(var)) {
        var <- new("dev_var", name = vname, argument = FALSE, xrefs = list(loc))
      } else {
        var@xrefs <- c(var@xrefs, loc)
      }
      assign(var@name, var, env)
      if (breakafter) break
    }
  }
  invisible(NULL)
}

#' Create html pages with functions and xrefs of variables in R files
#'
#' This function creates a html page with all R functions in an R file
#' and the variables used therein. The resulting htmlfiles will be in the
#' dest directory with name `R file name`.html.
#'
#' @details
#' The function determines the functions in the R files by
#' looking at the top-level assignments. Only
#'  LEFT_ASSIGN \code{'<-'} symbols are looked at!
#'
#' @param file vector of filenames
#' @param dest the directory where the html files should be written
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'  temp <- xref_html(paste0("R/", c("dev_calltree", "dev_util"), ".R"),
#'                         "c:/temp/examplexref")
#' }
#'
#' @author Luc De Wilde
#' @name xref_html
#' @rdname xref_html
#' @export
xref_html <- function(file, dest) {
  stopifnot(is.character(dest), length(dest) == 1L)
  tmp <- get_xref(file)
  tmpnamen <- names(tmp)
  for (i in seq_along(tmp)) {
    xref_html1(tmp[[i]], tmpnamen[i], dest)
  }
  invisible(NULL)
}
xref_html1 <- function(gotxref, sourcemember, dest) {
  functienamen <- names(gotxref)
  if (length(functienamen) == 0L) return()
  if (!dir.exists(dest)) {
    dir.create(dest)
  }
  htmlnaam <- paste0(dest, "/", basename(sourcemember) , ".html")
  sink(htmlnaam)
  title <- paste("Xref", sourcemember)
  cat(
    "<!DOCTYPE html>
<html>
<head>
<title>",
    title,
    "</title>
<style>
h1 {
 color: #1e64C8;
}
p.lijst {
 margin-top: 0.5em;
 margin-bottom: 0.5em;
}
p.lijst:nth-child(even) {
  background-color: #dddddd;
}
div.mycontainer {
  width:95%;
  overflow:auto;
  border-style: solid;
  border-width: 5px;
  margin: 4px;
}
</style>
<script>
const funcids = [\n")
for (j in seq_along(functienamen)) {
  cat('["', j, '", "', functienamen[j], '"],\n', sep = "")
}
cat("];\nconst funcvar = [\n")
for (j in seq_along(functienamen)) {
  func <- gotxref[[j]]
  for (i in seq_along(func)) {
    vartje <- func[[i]]
    cat('["', j, '", "', vartje@name, '", "', if(vartje@argument) "x", '", "',
    paste(sapply(vartje@xrefs, \(xr)
          paste0(xr@line, ":", xr@position, if(xr@modified) "*" else "")),
          collapse = " ")
     ,'"],\n', sep = "")
  }
}
cat("];
let fvLen = funcvar.length;
function ShowFunc(funcname){
  for (let j = 0; j < funcids.length; j++) {
    if (funcids[j][0] == funcname) funcnaam = funcids[j][1]
  }
  text = \"<H1>Function \" + funcnaam + \"</H1><table>\";
  text += \"<tr><th>Variable</th><th>Argument?</th><th>Positions where used, * means item is modified</th></tr>\";
  for (let i = 0; i < fvLen; i++) {
    if (funcvar[i][0] == funcname)
      text += \"<tr><td>\" + funcvar[i][1] +
        \"</td><td>\" + funcvar[i][2] + \"</td><td>\" + funcvar[i][3] + \"</td></tr>\\n\";
  }
  text += \"</table>\";
  document.getElementById(\"cont\").innerHTML = text;
}
function FromSelect() {
  Selector = document.getElementById('selector');
  ShowFunc(Selector.value);
}
window.onload = function() {
  FromSelect();
}
</script>
</head>
<body>
  <h1>Xref functions in ",
    sourcemember,
    ".</h1>\n",
    "  <select id='selector' onchange='FromSelect()' title='Choose function'>",
    sep = ""
  )
  for (j in seq_along(functienamen)) {
    cat(
      "<option value='",
      j,
      "'>",
      functienamen[j],
      "</option>\n",
      sep = ""
    )
  }
  cat(
    "</select>\n<div id=\"cont\" name=\"cont\" style=\"border-color:#ffd200;\" class=\"mycontainer\"></div>
     </body>
     </html>\n"
  )
  sink()
  browseURL(htmlnaam)
}
