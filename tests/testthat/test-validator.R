describe("validator", {
  it("only allows one estimate", {
    expect_error(ife(c(1, 2), runif(5)), "@x must be length 1")
  })

  it("eif must be more than length one", {
    expect_error(ife(1, runif(1)), "@eif must be greather than length 1")
  })

  it("weights and eif must be same length", {
    expect_error(ife(1, runif(5), weights = runif(4)), "@weights must be same length as @eif")
  })

  it("id and eif must be same length", {
    expect_error(ife(1, runif(5), id = c("a", "b")), "@id must be same length as @eif")
  })

  it("eif can't contain NA", {
    expect_error(ife(1, c(1, 2, NA)), "@eif must not contain `NA`")
  })

  it("weights can't contain NA", {
    expect_error(ife(1, c(1, 2, 3), weights = c(1, 2, NA)), "@weights must not contain `NA`")
  })

  it("id can't contain NA", {
    expect_error(ife(1, c(1, 2, 3), id = c("a", "b", NA)), "@id must not contain `NA`")
  })
})

describe("validate contrasts", {
  it("eifs are the same length", {
    x <- ife(1, runif(5))
    y <- ife(1, runif(4))
    expect_error(x + y, "Length of @eif must be the same")
  })

  it("weights must be the same", {
    x <- ife(1, runif(5))
    y <- ife(1, runif(5), weights = runif(5))
    expect_error(x + y, "@weights must be the same")
  })

  it("ids must be the same", {
    x <- ife(1, runif(5))
    y <- ife(1, runif(5), id = c("a", "b", "c", "d", "e"))
    expect_error(x + y, "@id must be the same")
  })
})
