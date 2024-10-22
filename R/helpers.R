check_same <- function(e1, e2) {
  if (length(e1@eif) != length(e2@eif)) {
    stop("Length of @eif must be the same")
  }

  if (all(e1@weights != e2@weights)) {
    stop("@weights must be the same")
  }

  if (all(e1@id != e2@id)) {
    stop("@id must be the same")
  }

  invisible()
}
