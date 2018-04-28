#' Printing statistics nicely
#'
#' @param x Statistic
#' @param k Number of decminal places to round to
#' @param base rounds to the nearest 'base'
#' @param pval p-value (or any probability)
#' @param b,b1,b2 bracketed values
#' @name printing
#' @examples
#'
NULL





# rounds to nearest base
#' @rdname printing
#' @export
mround <- function(x, base) { base * round(x / base) }

# rounds to k decimal places, includes zeros and returns character vector
#' @rdname printing
#' @export
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

# prints p-value, censoring at kth decimal place
#' @rdname printing
#' @export
print_pval <- function(pval, k=3){
  ifelse(pval < 1/(10^k), paste0("p<", 1/(10^k)), paste0("p=", specify_decimal(pval, k = 3)))
}


# print estimate and confidence limits in the from 'x (b)'
#' @rdname printing
#' @export
print_est1bracket <- function(x, b, round=1){
  paste0(specify_decimal(x, round), " (", specify_decimal(b, round), ")")
}

# print estimate and confidence limits in the from 'x (b1, b2)'
#' @rdname printing
#' @export
print_est2bracket <- function(x, b1, b2, round=1){
  paste0(specify_decimal(x, round), " (", specify_decimal(b1, round), ", ", specify_decimal(b2, round), ")")
}

# prints confidence limits in the from '(b1, b2)'
#' @rdname printing
#' @export
print_2bracket <- function(b1, b2, round=1){
  paste0("(", specify_decimal(b1, round), ", ", specify_decimal(b2, round), ")")
}


# adds pading so that decimals for printed numbers are aligned - nicked from skimr package (HOW DO I ATTRIBUTE?)
#' @rdname printing
#' @export
align_decimal <- function(x){
  split <- stringr::str_split(x, "\\.", simplify = TRUE)
  if (ncol(split) < 2) return(x)
  max_whole <- max(nchar(split[,1]))
  max_decimal <- max(nchar(split[,2]))
  left <- stringr::str_pad(split[,1], max_whole, side = "left")
  right <- stringr::str_pad(split[,2], max_decimal, side = "right")
  dec <- ifelse(split[, 2] == "", " ", ".")
  sprintf("%s%s%s", left, dec, right)
}





