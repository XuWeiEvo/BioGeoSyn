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

test_that("model sensitivity summary is readable as a table", {
  tab <- data.frame(
    model = c("DEC", "DEC+J", "DIVALIKE"),
    logLik = c(-120, -115, -130),
    num_params = c(2, 3, 2)
  )
  cmp <- compare_models(tab, n = 20)
  sensitivity_table <- model_sensitivity_summary_table(cmp)

  expect_true(all(c(
    "summary_item",
    "section",
    "display_label",
    "answer",
    "models",
    "model_count",
    "evidence",
    "interpretation_note"
  ) %in% names(sensitivity_table)))
  expect_equal(
    sensitivity_table$display_label[match("best_overall_is_plus_j", sensitivity_table$summary_item)],
    "Best model includes +J"
  )
  expect_equal(
    sensitivity_table$answer[match("best_overall_is_plus_j", sensitivity_table$summary_item)],
    "yes"
  )
  expect_equal(
    sensitivity_table$answer[match("auto_declare_best_model", sensitivity_table$summary_item)],
    "no"
  )
})
