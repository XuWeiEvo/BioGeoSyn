compat_bundle <- function(name, model = "DEC", max_range = 3L, codes = c(A = "Area A", B = "Area B"),
                          age = 10, constraints = list(times_file = NULL), n_maps = 50L) {
  root <- tempfile(paste0("bgs-compat-", name, "-"))
  dir.create(file.path(root, "tables"), recursive = TRUE)
  dir.create(file.path(root, "logs"))
  cc <- names(codes)
  ev <- data.frame(
    event_index = 1:2, model = model, replicate = 1:2,
    event_class = c("cladogenetic", "anagenetic"), event_type = c("vicariance", "d"),
    event_time_before_present = c(age, age / 2), node_index = c(3L, 1L),
    parent_state = c(paste(cc, collapse = ""), cc[[1L]]), child_state = c(paste(cc, collapse = ","), paste(cc, collapse = "")),
    source_region_code = c(NA, cc[[1L]]), target_region_code = c(NA, cc[[2L]]),
    source_region = c(NA, codes[[1L]]), target_region = c(NA, codes[[2L]]),
    stringsAsFactors = FALSE
  )
  utils::write.csv(ev, file.path(root, "tables", "bsm_events.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = model, event_type = "d", replicate_count = n_maps, mean_count = 1),
                   file.path(root, "tables", "bsm_event_summary.csv"), row.names = FALSE)
  utils::write.csv(data.frame(node_index = 1:3, node_type = c("tip", "tip", "internal"), parent_node_index = c(3L, 3L, NA),
                              time_before_present = c(0, 0, age)), file.path(root, "tables", "tree_nodes.csv"), row.names = FALSE)
  yaml::write_yaml(list(inputs = list(max_range_size = max_range), advanced = list(constraints = constraints)),
                   file.path(root, "config_used.yml"))
  writeLines(c("other attached packages:", " [1] BioGeoBEARS_1.1.3 BioGeoSyn_1.2.0"), file.path(root, "logs", "session_info.txt"))
  z <- file.path(tempdir(), paste0(name, ".zip"))
  unlink(z)
  zip_relative_files(root, z, c("tables/bsm_events.csv", "tables/bsm_event_summary.csv", "tables/tree_nodes.csv",
                                "config_used.yml", "logs/session_info.txt"))
}

test_that("processes_possible_under_model follows the standard model definitions", {
  expect_setequal(processes_possible_under_model("DEC"),
                  c("in_situ_speciation", "subset_sympatry", "vicariance", "range_expansion", "local_extinction"))
  expect_false("subset_sympatry" %in% processes_possible_under_model("DIVALIKE"))
  expect_setequal(setdiff(processes_possible_under_model("BAYAREALIKE"), c("range_expansion", "local_extinction")),
                  "in_situ_speciation")
  expect_true("founder_event_speciation" %in% processes_possible_under_model("BAYAREALIKE+J"))
  expect_false("range_switching" %in% processes_possible_under_model("DEC+J"))
})

test_that("compatible bundles pass and record their settings", {
  z <- c(compat_bundle("okA"), compat_bundle("okB", age = 20))
  cc <- check_bundle_compatibility(z)
  expect_s3_class(cc, "bgs_compatibility")
  expect_equal(nrow(cc$clades), 2L)
  expect_equal(cc$clades$max_range_size, c(3L, 3L))
  expect_equal(cc$clades$biogeobears_version, c("1.1.3", "1.1.3"))
  expect_false(any(cc$checks$level == "warning"))
  expect_match(cc$checks$message[cc$checks$check == "model"], "Founder-event")
  expect_output(print(cc), "compatibility")
})

test_that("mixed models, range limits, constraints and codings are flagged", {
  z <- c(
    compat_bundle("m1", model = "DEC"),
    compat_bundle("m2", model = "BAYAREALIKE+J", max_range = 4L, constraints = list(times_file = "times.txt")),
    compat_bundle("m3", codes = c(A = "Area A", B = "Area Z"), age = 500)
  )
  cc <- check_bundle_compatibility(z)
  warn <- cc$checks$check[cc$checks$level == "warning"]
  expect_true(all(c("model", "max_range_size", "constraints", "areas", "time_units") %in% warn))
  msg <- cc$checks$message[cc$checks$check == "model"]
  expect_match(msg, "Vicariance")
  expect_match(msg, "Founder-event")
})

test_that("trees of one clade mapped under different models are flagged", {
  z <- c(compat_bundle("T__tree001", model = "DEC"), compat_bundle("T__tree002", model = "DEC+J"))
  cc <- check_bundle_compatibility(z, c("T__tree001", "T__tree002"))
  expect_true("model_within_clade" %in% cc$checks$check)
  expect_equal(unique(cc$clades$clade), "T")
})

test_that("inherited assumptions are stated", {
  a <- inherited_assumptions()
  expect_true(length(a) >= 5L)
  expect_true(any(grepl("concordance", a)))
})
