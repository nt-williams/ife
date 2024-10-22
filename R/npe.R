npe <- S7::new_class("npe",
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
