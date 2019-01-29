#' Mass, distribution function, quantile function and random generation for an ordinal distribution.
#'
#' @param x,q vector of integers or character values that must exist in `names(prob)`
#' @param p vector of probabilities
#' @param n number of random values to return
#' @param prob vector of probabilities for observing each value
#' @param as.numeric logical; return value as numeric or character?
#' @param log logical; if TRUE, probabilities p are given as log(p)
#' @name ordinal
#' @examples
#'
NULL



# probability mass function (or density)
#' @rdname ordinal
#' @export
dordinal <- function(x, prob, log = FALSE){

  if (!is.numeric(x) | !all(x>=0)) stop("x must be non-negative numeric")
  if (!all(x%%1==0)) warning("non-integer x: density given as zero")

  prob <- c(0, prob/sum(prob), 0)  #normalise

  if (log) log( prob[pmin(pmax(x+1,1),length(prob))]*((x%%1)==0)*1 )
  else prob[pmin(pmax(x+1,1),length(prob))]*((x%%1)==0)*1
}



# distribution function
#' @rdname ordinal
#' @export
pordinal <- function(q, prob, log = FALSE){

  if (!is.numeric(q) | !all(q>=0)) stop("q must be non-negative numeric")

  q <- floor(q)
  prob <- prob/sum(prob)
  cumulprob <- c(0,cumsum(prob))

  if (log) log(cumulprob[pmin(pmax(q+1,1),length(cumulprob))])
  else cumulprob[pmin(pmax(q+1,1),length(cumulprob))]
}



# quantile function
#' @rdname ordinal
#' @export
qordinal <- function(p, prob, as.numeric = TRUE){

  if (!all(p>=0 & p<=1)) stop("p must be between 0 and 1")

  prob <- prob/sum(prob) #normalise
  cumulprob <- cumsum(prob)

  if(as.numeric) as.numeric(unlist(lapply(p, function(p, cumulprob){ seq_along(cumulprob)[min(which(p<=cumulprob))]}, cumulprob=cumulprob)))
  else if (!is.null(names(prob))) unlist(lapply(p, function(p, cumulprob){ names(cumulprob)[min(which(p<=cumulprob))]}, cumulprob=cumulprob))
  else unlist(lapply(p, function(p, cumulprob){ as.character(seq_along(cumulprob))[min(which(p<=cumulprob))]}, cumulprob=cumulprob))
}



# random value from ordinal distribution
#' @rdname ordinal
#' @export
rordinal <- function(n, prob, as.numeric = TRUE){

  prob <- prob/sum(prob)  #normalise
  counts <- apply(rmultinom(n, 1, prob = prob)==1, 1, sum)

  if(as.numeric) sample(rep(seq_along(counts), counts))
  else if (!is.null(names(prob))) sample(rep(names(counts), counts))
  else sample(rep(as.character(seq_along(counts)), counts))
}


