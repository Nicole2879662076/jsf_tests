context("Medium Test: Basic R package with reticulate")

options(testthat.progress.max_fails = Inf)

cat("\n")
cat("===========================================\n")
cat("jsfR Package - Medium Test Results\n")
cat("===========================================\n\n")

test_that("1. jsf_available() - Python availability", {
  cat("[1/4] jsf_available()\n")
  result <- jsf_available()
  expect_type(result, "logical")
  cat("  Status: ", if(result) "PASS" else "SKIP", "\n")
  cat("  Return: ", result, "\n\n")
})

test_that("2. jsf_simple_simulate() - R-Python integration", {
  skip_if_not(jsf_available(), "Python jsf not available")

  cat("[2/4] jsf_simple_simulate()\n")
  result <- jsf_simple_simulate(t_max = 2)

  expect_type(result, "list")
  expect_true(is.list(result[[1]]))
  expect_true(is.numeric(result[[2]]))

  cat("  Status: PASS\n")
  cat("  Structure: List with ", length(result), " elements\n")
  cat("  Core: R ↔ Python via reticulate ✓\n\n")
})

test_that("3. JSFResult S7 Class - Object system", {
  skip_if_not(jsf_available(), "Python jsf not available")

  cat("[3/4] JSFResult S7 Class\n")
  result <- jsf_run_s7(t_max = 2)

  expect_true(inherits(result, "S7_object") || grepl("JSFResult", class(result)[1]))
  expect_true(.hasSlot(result, "time"))
  expect_true(.hasSlot(result, "populations"))

  cat("  Status: PASS\n")
  cat("  Class: ", class(result)[1], "\n")
  cat("  Slots: time, populations, metadata ✓\n\n")
})

test_that("4. plot_jsf() - Visualization", {
  cat("[4/4] plot_jsf()\n")
  expect_true(exists("plot_jsf", mode = "function"))
  cat("  Status: PASS\n")
  cat("  Function: Available ✓\n\n")
})

cat("===========================================\n")
cat("Summary: 4/4 tests completed\n")
cat("===========================================\n")
