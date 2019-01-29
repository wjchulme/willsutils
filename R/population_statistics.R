#' Population estimates for var and sd
#'
#' @param x a numeric vector.
#' @param y NULL (default) or a vector.
#' @param na.rm logical. Should missing values be removed?
#' @name population_estimates
#' @examples
#'
NULL


# Calculates population variance
#' @rdname population_estimates
#' @export
var.p = function(x, na.rm = FALSE){

  if(na.rm){
    len <- sum(!is.na(x))
    if(len > 1) var(x, na.rm = na.rm)*(len-1)/len
    else if(len==1) 0
    else NA_real_
  }
  else {
    len <- length(x)
    if(len > 1) var(x, na.rm = na.rm)*(len-1)/len
    else if(len==1) 0
    else NA_real_
  }
}


# Calculates population standard deviation
#' @rdname population_estimates
#' @export
sd.p = function(x, na.rm = FALSE){
  if(na.rm){
    len <- sum(!is.na(x))
    if(len > 1) sd(x, na.rm = na.rm)*sqrt((len-1)/len)
    else if(len==1) 0
    else NA_real_
  }
  else {
    len <- length(x)
    if(len > 1) sd(x, na.rm = na.rm)*sqrt((len-1)/len)
    else if(len==1) 0
    else NA_real_
  }
}
