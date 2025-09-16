#' Gets a cross-reference of variables used in top-level defined functions
#'
#' This function creates a cross-reference of the variables for
#' all functions defined on the top-level in a .R-file.
#'
#' @param file a character vector naming the files to search in
#'
#' @return list with for each top.level function a named list with:
#' * FUNC__FILE_ the name of the file where function is found;
#' * FUNC__OFFSET_ number of lines before the function definition;
#' * the variables used and the relative line where they are used.
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
#' for (j in seq_along(tmp)) {
#'   cat("function ", names(tmp)[j])
#'   tmp1 <- tmp[[j]]
#'   cat(" found in", tmp1$FUNC__FILE_, "at offset", tmp1$FUNC__OFFSET_, ".\n")
#'   tmp1$FUNC__FILE_ <- NULL
#'   tmp1$FUNC__OFFSET_ <- NULL
#'   for (k in seq_along(tmp1)) {
#'     cat("  ", names(tmp1)[k], ":",
#'     tostring(tmp1[[k]], "none"), "\n")
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
    parseddata <- get_parsed(f)
    exprs <- parseddata$id[parseddata$parent == 0 & parseddata$token == "expr"]
    for (i in seq_along(exprs)) {
      subs <- parseddata[parseddata$parent == exprs[i], ]
      asg <- which(subs$token == "LEFT_ASSIGN")
      for (j in asg) {
        if (j > 1L && j < length(subs) && subs$token[j - 1L] == "expr" &&
            subs$token[j + 1L] == "expr") {
          subs1 <- parseddata[parseddata$parent == subs$id[j - 1L], ]
          subs2 <- parseddata[parseddata$parent == subs$id[j + 1L], ]
          if (nrow(subs1) == 1L && subs1$token[1] == "SYMBOL" &&
              subs2$token[1] == "FUNCTION") {
            # found a function on top-level
            funcname <- subs1$text
            env <- new.env(parent = emptyenv())
            assign("FUNC__FILE_", basename(f), env)
            assign("FUNC__OFFSET_", subs1$line1[1] - 1L, env)
            get_vars(subs2, parseddata, env, TRUE)
            retval[[funcname]] <- as.list(env, all.names = TRUE, sorted = TRUE)
          }
        }
      }
    }
  }
  retval
}
get_vars <- function(subs, parseddata, env, first) {
  offset <- env$FUNC__OFFSET_
  if (first) {
    symbolformals <- which(subs$token == "SYMBOL_FORMALS")
    for (k in symbolformals) assign(subs$text[k], subs$line1[k] - offset, env)
  }
  for (j in seq_along(subs$token)) {
    if (subs$token[j] == "SYMBOL") {
      vname <- subs$text[j]
      lines <- get0(vname, env)
      if (is.null(lines)) {
        lines <- subs$line1[j] - offset
      } else {
        lines <- c(lines, subs$line1[j] - offset)
      }
      assign(vname, lines, env)
    }
    if (subs$terminal[j] == FALSE) {
      subsubs <- parseddata[parseddata$parent == subs$id[j], ]
      if (nrow(subsubs) == 3L && any(subsubs$token[2L] == c("'@'", "'$'"))) {
        vname <- get_composite_name(subsubs, parseddata)
        lines <- get0(vname, env)
        if (is.null(lines)) {
          lines <- subs$line1[j] - offset
        } else {
          lines <- c(lines, subs$line1[j] - offset)
        }
        assign(vname, lines, env)
      } else {
        get_vars(subsubs, parseddata, env, FALSE)
      }
    }
  }
  invisible(NULL)
}
get_composite_name <- function(subsubs, parseddata) {
  if (nrow(subsubs) == 1L) return(subsubs$text[1])
  subsubsubs <- parseddata[parseddata$parent == subsubs$id[1], ]
  coll <- "@"
  if (subsubs$token[2] == "'$'") coll <- "$"
  paste0(get_composite_name(subsubsubs, parseddata),
         coll,
         subsubs$text[3L])
}


#' Create a html page with functions and xrefs of variables
#'
#' This function creates a html page with all R functions in a list
#' of R-files and the variables used therein.
#'
#' @details
#' The function determines the functions in the R files by
#' looking at the top-level assignments. Only
#'  LEFT_ASSIGN \code{'<-'} symbols are looked at!
#'
#' @param file vector of filenames to look in
#' @param dest the directory where the html files should be written
#' @param title the title for the html file
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
xref_html <- function(file, dest, title = paste("Xref", dirname(file[1L]))) {
  stopifnot(is.character(dest), length(dest) == 1L)
  tmp <- get_xref(file)
  tmpnamen <- names(tmp)
  aantal <- 0L
  for (i in seq_along(tmp)) {
    aantal <- aantal + length(tmp[[i]]) - 2
  }
  functienamen <- varnamen <- lijnen <- rep("",aantal)
  k <- 0L
  for (i in seq_along(tmp)) {
    tmpinamen <- names(tmp[[i]])
    for (j in seq_along(tmp[[i]])) {
      if (tmpinamen[j] == "FUNC__FILE_") next
      if (tmpinamen[j] == "FUNC__OFFSET_") next
      k <- k + 1L
      functienamen[k] <- tmpnamen[i]
      varnamen[k] <- tmpinamen[j]
      lijnen[k] <- tostring(tmp[[i]][[j]], "none")
    }
  }
  volgorde <- order(functienamen, varnamen)
  functienamen <- functienamen[volgorde]
  varnamen <- varnamen[volgorde]
  lijnen <- lijnen[volgorde]
  if (!dir.exists(dest)) dir.create(dest)
  sink(paste0(dest, "/index.html"))
  cat("<!DOCTYPE html>
<html>
<head>
<title>", title, "</title>
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
}
div.mycontainer div {
  width:45%;
  float:left;
  border-style: solid;
  border-width: 5px;
  margin: 4px;
}
</style>
<script>
const funcvar = [",
paste("[\"", functienamen, "\", \"", varnamen, "\", \"",
      lijnen , "\"],\n", sep = ""),
"];
let fvLen = funcvar.length;
function ShowFunc(funcname){
  text = \"<H1>Function \" + funcname + \"</H1><table>\";
  text += \"<tr><th>Variable</th><th>Lines where used</th></tr>\";
  for (let i = 0; i < fvLen; i++) {
    if (funcvar[i][0] == funcname)
      text += \"<tr><td><button type='button' onclick='ShowVar(\\\"\" +
        funcvar[i][1] + \"\\\");'>\" + funcvar[i][1] +
        \"</button></td><td>\" + funcvar[i][2] + \"</td></tr>\\n\";
  }
  text += \"</table>\";
  document.getElementById(\"links\").innerHTML = text;
}
function ShowVar(varname){
  text = \"<H1>Variable \" + varname + \"</H1><table>\";
  text += \"<tr><th>Function</th><th>Lines where used</th></tr>\";
  for (let i = 0; i < fvLen; i++) {
    if (funcvar[i][1] == varname)
      text += \"<tr><td><button type='button' onclick='ShowFunc(\\\"\" +
        funcvar[i][0] + \"\\\");'>\" + funcvar[i][0] +
        \"</button></td><td>\" + funcvar[i][2] + \"</td></tr>\\n\";
  }
  text += \"</table>\";
  document.getElementById(\"rechts\").innerHTML = text;
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
  <h1>Xref functions in ", h1xref(file), ".</h1>\n",
"  <select id='selector' onchange='FromSelect()' title='Choose function'>",
        sep = "")
  for (functienaam in tmpnamen) {
    cat("<option value='", functienaam, "'>", functienaam, "</option>\n",
        sep = "")
  }
  cat("</select>\n<div class='mycontainer'>
  <div id='links' name='links' style=\"border-color:#ffd200;\"></div>
  <div id='rechts' name='rechts' style=\"border-color:#1e64c8;\"></div>
  </div></body>\n</html>\n")
  sink()
  browseURL(paste0(dest, "/index.html"))
}
h1xref <- function(file) {
  if (length(file) == 1L) return(file)
  file <- sort(file)
  dirnames <- unique(sapply(file, dirname))
  if (length(dirnames) == 1L) file <- substring(file, nchar(dirnames) + 2L)
  tostring(file, "and", "single")
}
