test_that("Check typed wrappers", {
  expect_equal(v_integer(1), 1)
  expect_equal(v_integer(NULL), NA_integer_)
  expect_type(v_integer(1), "integer")
  expect_type(v_integer(NULL), "integer")

  expect_equal(v_character(letters[1:3]), c("a", "b", "c"))
  expect_equal(v_character(NULL), NA_character_)
  expect_type(v_character(letters[1:3]), "character")
  expect_type(v_character(NULL), "character")

  expect_equal(v_real(pi), pi)
  expect_equal(v_real(NA), NA_real_)
  expect_type(v_real(pi), "double")
  expect_type(v_real(NA), "double")

  expect_equal(v_logical(T), T)
  expect_equal(v_logical(NA), NA)
  expect_type(v_logical(T), "logical")
  expect_type(v_logical(NA), "logical")
})

