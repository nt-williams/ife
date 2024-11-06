#' @importFrom cli cli_div cli_text cli_end
#' @importFrom S7 new_class new_generic new_property new_object S7_object class_double class_character `method<-` class_numeric
NULL

influence_func_estimate <- new_class("influence_func_estimate",
  package = "ife",
  properties = list(
    x = class_double,
    eif = class_double,
    weights = class_double,
    id = class_character,
    std_error = new_property(
      getter = function(self) {
        n <- length(self@eif)
        id <- 1:n
        clusters <- split(self@eif*self@weights, id)
        j <- length(clusters)
        sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
      }
    ),
    conf_int = new_property(
      getter = function(self) {
        self@x + c(-1, 1)*self@std_error*qnorm(0.975)
      }
    )
  ),
  #' Create a new `influence_func_estimate` object
  #'
  #' @name ife_constructor
  #'
  #' @param x [\code{numeric(1)}]\cr
  #'  The estimate.
  #' @param eif [\code{numeric(n)}]\cr
  #'  The influence function.
  #' @param weights [\code{numeric(n)}]\cr
  #'  Optional sampling weights.
  #' @param id [\code{character(n)}]\cr
  #'  Optional cluster identifiers.
  #'
  #' @return An 'S7' object of class \code{influence_func_estimate}.
  #' @export
  #'
  #' @examples
  #' x <- influence_func_estimate(5, runif(10))
  #' y <- ife(5, runif(10))
  #' x + y
  #' x + 1
  #' 1 - y
  #' x / y
  #' x * y
  #' tidy(x)
  constructor = function(x, eif, weights = rep(1, length(eif)), id = as.character(1:length(eif))) {
    new_object(S7_object(), x = x, eif = eif, weights = weights, id = id)
  },
  validator = function(self) {
    if (length(self@x) != 1) {
      return("@x must be length 1")
    }

    if (!(length(self@eif) > 1)) {
      return("@eif must be greather than length 1")
    }

    if (length(self@weights) != length(self@eif)) {
      return("@weights must be same length as @eif")
    }

    if (length(self@id) != length(self@eif)) {
      return("@id must be same length as @eif")
    }

    if (any(is.na(self@eif))) {
      return("@eif must not contain `NA`")
    }

    if (any(is.na(self@weights))) {
      return("@weights must not contain `NA`")
    }

    if (any(is.na(self@id))) {
      return("@id must not contain `NA`")
    }
  }
)

# print
method(print, influence_func_estimate) <- function(x, ...) {
  div <- cli_div(theme = list(.val = list(digits = 2)))
  cli_text(cat("      "), "Estimate: {.val {x@x}}")
  cli_text(cat("    "), "Std. error: {.val {x@std_error}}")
  cli_text("95% Conf. int.: {.val {x@conf_int[1]}}, {.val {x@conf_int[2]}}")
  cli_end(div)
}

# x + y
method(`+`, list(influence_func_estimate, influence_func_estimate)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimate(e1@x + e2@x, e1@eif + e2@eif, e1@weights, e1@id)
}

method(`+`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) {
  influence_func_estimate(e1@x + e2, e1@eif, e1@weights, e1@id)
}

method(`+`, list(class_numeric, influence_func_estimate)) <- function(e1, e2) e2 + e1

# x - y
method(`-`, list(influence_func_estimate, influence_func_estimate)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimate(e1@x - e2@x, e1@eif - e2@eif, e1@weights, e1@id)
}

method(`-`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) {
  influence_func_estimate(e1@x - e2, e1@eif, e1@weights, e1@id)
}

method(`-`, list(class_numeric, influence_func_estimate)) <- function(e1, e2) {
  influence_func_estimate(e1 - e2@x, e2@eif, e2@weights, e2@id)
}

# x / y
method(`/`, list(influence_func_estimate, influence_func_estimate)) <- function(e1, e2) {
  check_same(e1, e2)
  eif <- (e1@eif / e2@x) - ((e2@eif / e2@x^2) * e1@x)
  influence_func_estimate(e1@x / e2@x, eif, e1@weights, e1@id)
}

method(`/`, list(class_numeric, influence_func_estimate)) <- function(e1, e2) {
  influence_func_estimate(e1 / e2@x, -e1 / e2@x^2 * e2@eif, e2@weights, e2@id)
}

method(`/`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) {
  influence_func_estimate(e1@x / e2, 1 / e2 * e1@eif, e1@weights, e1@id)
}

# x * y
method(`*`, list(influence_func_estimate, influence_func_estimate)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimate(e1@x * e2@x, e2@x * e1@eif + e1@x * e2@eif, e1@weights, e1@id)
}

method(`*`, list(class_numeric, influence_func_estimate)) <- function(e1, e2) {
  influence_func_estimate(e1 * e2@x, e1 * e2@eif, e2@weights, e2@id)
}

method(`*`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) e2 * e1
