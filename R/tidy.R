#' @importFrom generics tidy
#' @export
generics::tidy

tidy <- new_generic("tidy", "x")

method(tidy, influence_func_estimate) <- function(x) {
  out <- data.frame(
    estimate = x@x,
    std.error = x@std_error,
    conf.low = x@conf_int[1],
    conf.high = x@conf_int[2]
  )
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
