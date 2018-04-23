#' Printing statistics nicely
#'
#' @param x Statistic
#' @param k Number of decminal places to round to
#' @param base rounds to the nearest 'base'
#' @param pval p-value (or any probability)
#' @param xLL,xUL lower and upper confidence limits
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
  ifelse(pval<1/(10^k), paste0("p<", 1/(10^k)), paste0("p=", specify_decimal(pval, k = 3)))
}

# print estimate and confidence limits in the from 'est (estLL, estUL)'
#' @rdname printing
#' @export
print_estCIs <- function(x, xLL, xUL, round=1){
  paste0(specify_decimal(x, round), " (", specify_decimal(xLL, round), ", ", specify_decimal(xUL, round), ")")
}

# prints confidence limits in the from '(estLL, estUL)'
#' @rdname printing
#' @export
print_CIs <- function(xLL, xUL, round=1){
  paste0("(", specify_decimal(xLL, round), ", ", specify_decimal(xUL, round), ")")
}





