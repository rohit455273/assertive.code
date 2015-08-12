library(testthat)
library(devtools)
library(assertive.code)

with_envvar(
  c(LANG = "en_US"),
  test_check("assertive.code")
)
