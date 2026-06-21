test_that("compare_models adds model uncertainty fields", {
  tab <- data.frame(
    model = c("DEC", "DEC+J", "DIVALIKE"),
    logLik = c(-120, -115, -130),
    num_params = c(2, 3, 2)
  )
  cmp <- compare_models(tab, n = 20)
  expect_true(all(c("model_family", "has_j", "delta_aicc", "aicc_weight", "caution_flag") %in% names(cmp)))
  expect_equal(cmp$model[1], "DEC+J")
})
