#' Not in
#'
#' This negates the `%in%` function. Used as a binary operator.


#' @examples
#' c(2,4,6,8,10) %ni% c(3,4,5)
#'
#'
#' @export
`%ni%` <- Negate(`%in%`)
