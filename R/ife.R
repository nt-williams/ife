#' @rdname ife_constructor
#' @export
ife <- function(x, eif, weights = rep(1, length(eif)), id = as.character(1:length(eif))) {
  influence_func_estimate(x, eif, weights, id)
}
