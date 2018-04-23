#' Get code or level
#'
#' These functions extract the code (numeric) or the level (character) from a character variable of the form "[code]. [level]"
#' @name lvl
#' @aliases lvlval
#' @param a character variable of form "[code]. [level]"

#' @rdname lvl
#' @export
# extracts number at start of text
lvl <- function(a) as.numeric(gsub("[^0-9].+", "", a))


#' @rdname lvl
#' @export
# extracts text after number
lvlval <- function(a) gsub("^[0-9]+. ", "", a)




#' @examples
#'
#'
#' lvl(c("1. one", "2. two"))
#' lvlval(c("1. one", "2. two"))
