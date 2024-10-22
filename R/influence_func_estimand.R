influence_func_estimand <- S7::new_class("influence_func_estimand",
  package = "ife",
  properties = list(
    x = S7::class_double,
    eif = S7::class_double,
    weights = S7::class_double,
    id = S7::class_character,
    std_error = S7::new_property(
      getter = function(self) {
        n <- length(self@eif)
        id <- 1:n
        clusters <- split(self@eif*self@weights, id)
        j <- length(clusters)
        sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
      }
    ),
    conf_int = S7::new_property(
      getter = function(self) {
        self@x + c(-1, 1)*self@std_error*qnorm(0.975)
      }
    )
  ),
  #' Create a new `influence_func_estimand` object
  #'
  #' @param x [\code{numeric(1)}]\cr
  #'  The estimand.
  #' @param eif [\code{numeric(n)}]\cr
  #'  The influence function.
  #' @param weights [\code{numeric(n)}]\cr
  #'  Optional sampling weights.
  #' @param id [\code{character(n)}]\cr
  #'  Optional cluster identifiers.
  #'
  #' @return An 'S7' object of class \code{influence_func_estimand}.
  #' @export
  #'
  #' @examples
  #' x <- influence_func_estimand(5, runif(10))
  #' y <- influence_func_estimand(5, runif(10))
  #' x + y
  #' x + 1
  #' 1 - y
  #' x / y
  #' x * y
  constructor = function(x, eif, weights = rep(1, length(eif)), id = as.character(1:length(eif))) {
    S7::new_object(S7::S7_object(), x = x, eif = eif, weights = weights, id = id)
  },
  validator = function(self) {
    if (length(self@x) != 1) {
      "@x must be length 1"
    }

    if (!(length(self@eif > 1))) {
      "@eif must be greather than length 1"
    }

    if (length(self@weights) != length(self@eif)) {
      "@weights must be same length as @eif"
    }

    if (length(self@id) != length(self@eif)) {
      "@id must be same length as @eif"
    }

    if (any(is.na(self@eif))) {
      "@eif must not contain `NA`"
    }

    if (any(is.na(self@weights))) {
      "@weights must not contain `NA`"
    }

    if (any(is.na(self@id))) {
      "@id must not contain `NA`"
    }
  }
)

# print
S7::method(print, influence_func_estimand) <- function(x) {
  div <- cli::cli_div(theme = list(.val = list(digits = 2)))
  cli::cli_li("     Estimand: {.val {x@x}}")
  cli::cli_li("   Std. error: {.val {x@std_error}}")
  cli::cli_li("95% Conf. int.: {.val {x@conf_int[1]}}, {.val {x@conf_int[2]}}")
  cli::cli_end(div)
}

# x + y
S7::method(`+`, list(influence_func_estimand, influence_func_estimand)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimand(e1@x + e2@x, e1@eif + e2@eif, e1@weights, e1@id)
}

S7::method(`+`, list(influence_func_estimand, S7::class_numeric)) <- function(e1, e2) {
  influence_func_estimand(e1@x + e2, e1@eif, e1@weights, e1@id)
}

S7::method(`+`, list(S7::class_numeric, influence_func_estimand)) <- function(e1, e2) e2 + e1

# x - y
S7::method(`-`, list(influence_func_estimand, influence_func_estimand)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimand(e1@x - e2@x, e1@eif - e2@eif, e1@weights, e1@id)
}

S7::method(`-`, list(influence_func_estimand, S7::class_numeric)) <- function(e1, e2) {
  influence_func_estimand(e1@x - e2, e1@eif, e1@weights, e1@id)
}

S7::method(`-`, list(S7::class_numeric, influence_func_estimand)) <- function(e1, e2) {
  influence_func_estimand(e1 - e2@x, e2@eif, e2@weights, e2@id)
}

# x / y
S7::method(`/`, list(influence_func_estimand, influence_func_estimand)) <- function(e1, e2) {
  check_same(e1, e2)
  eif <- (e1@eif / e2@x) - ((e2@eif / e2@x^2) * e1@x)
  influence_func_estimand(e1@x / e2@x, eif, e1@weights, e1@id)
}

S7::method(`/`, list(S7::class_numeric, influence_func_estimand)) <- function(e1, e2) {
  influence_func_estimand(e1 / e2@x, -e1 / e2@x^2 * e2@eif, e2@weights, e2@id)
}

S7::method(`/`, list(influence_func_estimand, S7::class_numeric)) <- function(e1, e2) {
  influence_func_estimand(e1@x / e2, 1 / e2 * e1@eif, e1@weights, e1@id)
}

# x * y
S7::method(`*`, list(influence_func_estimand, influence_func_estimand)) <- function(e1, e2) {
  check_same(e1, e2)
  influence_func_estimand(e1@x * e2@x, e2@x * e1@eif + e1@x * e2@eif, e1@weights, e1@id)
}

S7::method(`*`, list(S7::class_numeric, influence_func_estimand)) <- function(e1, e2) {
  influence_func_estimand(e1 * e2@x, e1 * e2@eif, e2@weights, e2@id)
}

S7::method(`*`, list(influence_func_estimand, S7::class_numeric)) <- function(e1, e2) e2 * e1
