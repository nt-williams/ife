describe("operators", {
  tol <- 1e-4
  set.seed(123)
  x <- ife(1, runif(5))
  y <- ife(2, runif(5))
  a <- 5

  describe("+", {
    it("ife + ife", {
      z <- x + y
      expect_equal(z@x, 3)
      expect_equal(z@std_error, 0.20732041, tolerance = tol)
    })

    it("ife + scalar", {
      z <- x + a
      expect_equal(z@x, 6)
      expect_equal(z@std_error, 0.1316355, tolerance = tol)
    })

    it("scalar + ife", {
      z <- a + x
      expect_equal(z@x, 6)
      expect_equal(z@std_error, 0.1316355, tolerance = tol)
    })
  })

  describe("-", {
    it("ife - ife", {
      z <- x - y
      expect_equal(z@x, -1)
      expect_equal(z@std_error, 0.1680583, tolerance = tol)
    })

    it("ife - scalar", {
      z <- x - a
      expect_equal(z@x, -4)
      expect_equal(z@std_error, 0.1316355, tolerance = tol)
    })

    it("scalar - ife", {
      z <- a - x
      expect_equal(z@x, 4)
      expect_equal(z@std_error, 0.1316355, tolerance = tol)
    })
  })

  describe("/", {
    it("ife / ife", {
      z <- x / y
      expect_equal(z@x, 0.5)
      expect_equal(z@std_error, 0.06748066, tolerance = tol)
    })

    it("ife / scalar", {
      z <- x / a
      expect_equal(z@x, 0.2)
      expect_equal(z@std_error, 0.0263271, tolerance = tol)
    })

    it("scalar / ife", {
      z <- a / x
      expect_equal(z@x, 5)
      expect_equal(z@std_error, 0.6581774, tolerance = tol)
    })
  })

  describe("*", {
    it("ife * ife", {
      z <- x * y
      expect_equal(z@x, 2)
      expect_equal(z@std_error, 0.3198977, tolerance = tol)
    })

    it("ife * scalar", {
      z <- x * a
      expect_equal(z@x, 5)
      expect_equal(z@std_error, 0.6581774, tolerance = tol)
    })

    it("scalar * ife", {
      z <- a * x
      expect_equal(z@x, 5)
      expect_equal(z@std_error, 0.6581774, tolerance = tol)
    })
  })
})
