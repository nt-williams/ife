check_same <- function(e1, e2) {
  if (length(e1@eif) != length(e2@eif)) {
    stop("Length of @eif must be the same")
  }

  if (!identical(e1@weights, e2@weights)) {
    stop("@weights must be the same")
  }

  if (!identical(e1@id, e2@id)) {
    stop("@id must be the same")
  }

  invisible()
}

make_immutable <- function(property) {
  function(self, value) {
    if (!is.null(S7::prop(self, property))) {
      stop(paste0("@", property, " is read-only"), call. = FALSE)
    }
    S7::prop(self, property) <- value
    self
  }
}
