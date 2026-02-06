#' @importFrom cli cli cli_div format_inline cli_text cli_end
#' @importFrom S7 new_class new_generic new_property new_object S7_object class_double class_character `method<-` class_numeric
#' @importFrom stats qnorm
NULL

influence_func_estimate <- new_class("influence_func_estimate",
  package = "ife",
  properties = list(
    x = new_property(
      class_double,
      setter = make_immutable("x")
    ),
    eif = new_property(
      class_double,
      setter = make_immutable("eif")
    ),
    weights = new_property(
      class_double,
      setter = make_immutable("weights")
    ),
    id = new_property(
      class_character,
      setter = make_immutable("id")
    ),
    critical_value = class_double,
    std_error = new_property(
      class_double,
      default = NA_real_,
      setter = function(self, value) {
        if (!is.null(self@std_error) && !is.na(self@std_error)) {
          stop("@std_error is read-only", call. = FALSE)
        }
        self@std_error <- value
        self
      }
    ),
    conf_int = new_property(
      getter = function(self) {
        self@x + c(-1, 1)*self@std_error*self@critical_value
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
  #' @param critical_value [\code{numeric(1)}]\cr
  #'  Optional critical value for constructing confidence interval.
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
  #' # Example: Confidence interval for a variance estimate
  #' x <- rnorm(100, 0, 2)
  #' ife(mean(x^2), x^2 - mean(x^2)) - ife(mean(x), x - mean(x))^2
  constructor = function(x, eif,
                         weights = rep(1, length(eif)),
                         id = as.character(1:length(eif)),
                         critical_value = qnorm(0.975)) {
    self <- new_object(
      S7_object(),
      x = x,
      eif = eif,
      weights = weights,
      id = id,
      critical_value = critical_value,
      std_error = NA_real_
    )

    S7::validate(self)

    self@std_error <- std_error(self)
    self
  },
  validator = function(self) {
    if (length(self@x) != 1) {
      return("@x must be length 1")
    }

    if (length(self@eif) <= 1) {
      return("@eif must be greather than length 1")
    }

    if (length(self@weights) != length(self@eif)) {
      return("@weights must be same length as @eif")
    }

    if (length(self@id) != length(self@eif)) {
      return("@id must be same length as @eif")
    }

    if (length(self@critical_value) != 1) {
      return("@critical_value must be length 1")
    }

    if (anyNA(self@eif)) {
      return("@eif must not contain `NA`")
    }

    if (anyNA(self@weights)) {
      return("@weights must not contain `NA`")
    }

    if (anyNA(self@id)) {
      return("@id must not contain `NA`")
    }
  }
)

# print
method(print, influence_func_estimate) <- function(x, ...) {
  ci <- x@conf_int
  div <- cli_div(theme = list(.val = list(digits = 3)))
  cli({
    cat(format_inline("      Estimate: {.val {x@x}}\n"))
    cat(format_inline("    Std. error: {.val {x@std_error}}\n"))
    cli_text("95% Conf. int.: {.val {ci[1]}}, {.val {ci[2]}}")
  })
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

# Delegate to ife + scalar since addition is commutative
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
  influence_func_estimate(e1 - e2@x, -e2@eif, e2@weights, e2@id)
}

# x / y
method(`/`, list(influence_func_estimate, influence_func_estimate)) <- function(e1, e2) {
  check_same(e1, e2)
  if (abs(e2@x) < .Machine$double.eps) {
    stop("Division by zero: denominator estimate is zero", call. = FALSE)
  }
  eif <- (e1@eif / e2@x) - ((e2@eif / e2@x^2) * e1@x)
  influence_func_estimate(e1@x / e2@x, eif, e1@weights, e1@id)
}

method(`/`, list(class_numeric, influence_func_estimate)) <- function(e1, e2) {
  if (abs(e2@x) < .Machine$double.eps) {
    stop("Division by zero: denominator estimate is zero", call. = FALSE)
  }
  influence_func_estimate(e1 / e2@x, -e1 / e2@x^2 * e2@eif, e2@weights, e2@id)
}

method(`/`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) {
  if (abs(e2) < .Machine$double.eps) {
    stop("Division by zero: denominator is zero", call. = FALSE)
  }
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

# x^n
method(`^`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) {
  influence_func_estimate(e1@x^e2, e2 * e1@x^(e2 - 1) * e1@eif, e1@weights, e1@id)
}

# Delegate to scalar * ife since multiplication is commutative
method(`*`, list(influence_func_estimate, class_numeric)) <- function(e1, e2) e2 * e1

# log(x)
method(log, influence_func_estimate) <- function(x, base) {
  if (x@x <= 0) {
    stop("log() requires positive values: estimate is ", x@x, call. = FALSE)
  }
  influence_func_estimate(log(x@x), x@eif / x@x, x@weights, x@id)
}

# exp(x)
method(exp, influence_func_estimate) <- function(x) {
  influence_func_estimate(exp(x@x), exp(x@x) * x@eif, x@weights, x@id)
}
