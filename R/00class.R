#' An S4 class to represent a top-level function
#'
#' @slot name A length-one character vector
#' @slot defined_in A length-one character vector
#' @slot defined_at_lines A length-two integer vector giving the first line
#' and the number of lines of the function definition
#' @slot exported A length-one logical specifying if the function is exported
#' @slot calls A character vector with names of top-level functions called by
#' this function
#' @slot called_by A character vector with names of top-level functions which
#' call this function
#' @slot synonym_of A length zero or one character vector with the name of the
#' synonym function which really defines the body
#' @slot complexity A length-one integer specifying the function complexity
setClass("dev_func",
  slots = c(
    name             = "character",
    defined_in       = "character",
    defined_at_lines = "integer",
    exported         = "logical",
    calls            = "character",
    called_by        = "character",
    synonym_of       = "character",
    complexity       = "integer"
  )
)
