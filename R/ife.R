#' @rdname ife_constructor
#' @export
ife <- function(x, eif,
                weights = rep(1, length(eif)),
                id = as.character(1:length(eif)),
                critical_value = qnorm(0.975)) {
  influence_func_estimate(x, eif, weights, id, critical_value)
}
