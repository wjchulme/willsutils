
#' Get code or level
#'
#' These functions extract the code (numeric) or the level (character) from a character variable of the form "[code]. [level]"
#' @param a character variable of form "[code]. [level]"
#' @keywords BCIS
#' @export
#' @examples
#'
#'
#' lvl(c("1. one", "2. two"))
#' lvlval(c("1. one", "2. two"))


# extracts number at start of text
lvl <- function(a) as.numeric(gsub("[^0-9].+", "", a))

# extracts text after number
lvlval <- function(a) gsub("^[0-9]+. ", "", a)




