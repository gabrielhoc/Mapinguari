#' List of unevaluated expressions
#'
#' \code{qlist} is similar to \code{quote}, but takes multiple arguments and returns them, (unevaluated) as a list.
#'
#' @param ... expressions, possibly named.
#'
#' @return Returns the expressions inputed (unevaluated) as a list.
#'
#' @examples
#' quote_list <- qlist(sum(prec[1:6]), mean(tmax[1:6]), mean(tmax[7:12]), mean(tmin[1:6]))
#'
#' tmax <- 32:44
#' tmin <- 15:27
#' prec <- 50:62
#'
#' lapply(quote_list, eval)
#'
#' @export

qlist <- function(...) as.list(substitute(list(...))[-1])


