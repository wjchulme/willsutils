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
var.p = function(x, y = NULL, na.rm = FALSE){
  len<-sum(!is.na(x))
  var(x, y, na.rm = na.rm)*(len-1)/len
}

# Calculates population standard deviation
#' @rdname population_estimates
#' @export
sd.p = function(x, na.rm = FALSE){
  len<-sum(!is.na(x))
  sd(x, na.rm = na.rm)*sqrt((len-1)/len)
}
