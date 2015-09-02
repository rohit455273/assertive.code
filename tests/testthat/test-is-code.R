# TODO: tests for is_error_free, is_if_condition, 
# is_loaded

test_that(
  "test is_binding_locked with a nonexistent variable returns false", 
  {
     e <- new.env()
     expect_false(is_binding_locked(x, e))
  }
)

test_that(
  "test is_binding_locked with an unlocked variable returns false", 
  {
     e <- new.env()
     e$x <- 1:10
     expect_false(is_binding_locked(x, e))
  }
)

test_that(
  "test is_binding_locked with a locked variable returns true", 
  {
     e <- new.env()
     e$x <- 1:10
     lockBinding("x", e)
     expect_true(is_binding_locked(x, e))
  }
)

test_that("test.is_debugged.a_function.returns_true_when_debugged", {
  x <- function() {
  }
  expect_false(is_debugged(x))
  debug(x)
  expect_true(is_debugged(x))
  undebug(x)
  expect_false(is_debugged(x))
})

test_that("test.is_existing.some_variables.returns_true_when_they_exist", {
  e <- new.env()
  e$a_variable <- 1
  x <- c("a_variable", "not_a_variable")
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  # actual needs to be calculated in its own line, not inside
  # expect_equal, or the parent frame is wrong
  actual <- is_existing(x, envir = e, inherits = FALSE)
  expect_equal(actual, expected, label = paste("actual = ", toString(deparse(actual))))
})

test_that("test.is_valid_r_code.invalid_r_code.returns_false", {
  x <- "x <- 1 + sqrt(pi) y <- sin(x)"
  expect_false(strip_attributes(actual <- is_valid_r_code(x)))
  expect_equal(
    cause(actual),
    noquote('"x <- 1 + sqrt(pi) y <- sin(x)" is not valid R code. <text>:1:19: unexpected symbol\n1: x <- 1 + sqrt(pi) y\n                      ^.')
  )
})

test_that("test.is_valid_r_code.valid_r_code.returns_true", {
  x <- "x <- 1 + sqrt(pi); y <- sin(x)"
  expect_true(is_valid_r_code(x))
})

