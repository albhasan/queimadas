test_that("get_cdata works", {
  s1 <- sample(c(LETTERS, letters), 3, replace = TRUE)
  s2 <- sample(c(LETTERS, letters), 3, replace = TRUE)
  s3 <- sample(c(LETTERS, letters), 3, replace = TRUE)
  cnames <- paste0(s1, s2, s3)
  d <- data.frame(x = 11:19, y = 101:109, z = 1001:1009)
  colnames(d) <- cnames

  for (i in seq_along(cnames)) {
    expect_equal(
      object = sum(get_cdata(x = d, cname = cnames[i])),
      expected = sum(1:9 + 10^i)
    )
  }
})
