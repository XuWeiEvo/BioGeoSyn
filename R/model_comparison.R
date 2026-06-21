#' Compare BioGeoBEARS models with methodological annotations
#'
#' @param model_table Data frame with at least `model`, `logLik`, and `num_params`.
#'   If `AICc` is absent it will be computed when `n` is provided.
#' @param n Optional sample size used for AICc.
#' @return A model comparison data frame.
#' @export
compare_models <- function(model_table, n = NULL) {
  required <- c("model", "logLik", "num_params")
  missing <- setdiff(required, names(model_table))
  if (length(missing) > 0L) {
    stop("model_table is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- model_table
  out$model_family <- model_family(out$model)
  out$has_j <- is_j_model(out$model)
  out$AIC <- -2 * out$logLik + 2 * out$num_params

  if (!"AICc" %in% names(out)) {
    if (!is.null(n)) {
      out$AICc <- out$AIC + (2 * out$num_params * (out$num_params + 1)) / pmax(n - out$num_params - 1, 1)
    } else {
      out$AICc <- out$AIC
    }
  }

  out <- out[order(out$AICc), , drop = FALSE]
  out$delta_aicc <- out$AICc - min(out$AICc, na.rm = TRUE)
  rel_lik <- exp(-0.5 * out$delta_aicc)
  out$aicc_weight <- rel_lik / sum(rel_lik, na.rm = TRUE)
  out$caution_flag <- flag_methodological_cautions(out)
  out$interpretation_note <- interpretation_notes(out)
  row.names(out) <- NULL
  out
}

#' Flag methodological cautions in model comparison output
#'
#' @param comparison A model comparison table.
#' @return Character vector of caution labels.
#' @export
flag_methodological_cautions <- function(comparison) {
  flags <- rep("none", nrow(comparison))
  if (!"has_j" %in% names(comparison)) {
    comparison$has_j <- is_j_model(comparison$model)
  }
  if (!"delta_aicc" %in% names(comparison)) {
    comparison$delta_aicc <- comparison$AICc - min(comparison$AICc, na.rm = TRUE)
  }

  best_is_j <- isTRUE(comparison$has_j[which.min(comparison$AICc)])
  if (best_is_j) {
    flags[comparison$has_j & comparison$delta_aicc <= 2] <- "plus_j_supported_check_sensitivity"
    flags[!comparison$has_j & comparison$delta_aicc <= 2] <- "non_j_near_best_include_in_discussion"
  }
  flags
}

interpretation_notes <- function(comparison) {
  ifelse(
    comparison$has_j,
    "Treat +J support as a statistical result requiring biological interpretation and sensitivity checks.",
    "Compare against paired +J models and report uncertainty before drawing biological conclusions."
  )
}

#' Assess model sensitivity across +J and non-+J families
#'
#' @param comparison A model comparison table returned by [compare_models()].
#' @return A list with best model, best non-J model, best +J model, and notes.
#' @export
assess_model_sensitivity <- function(comparison) {
  best <- comparison[which.min(comparison$AICc), , drop = FALSE]
  non_j <- comparison[!comparison$has_j, , drop = FALSE]
  plus_j <- comparison[comparison$has_j, , drop = FALSE]

  list(
    best_overall = best,
    best_non_j = if (nrow(non_j) > 0L) non_j[which.min(non_j$AICc), , drop = FALSE] else NULL,
    best_plus_j = if (nrow(plus_j) > 0L) plus_j[which.min(plus_j$AICc), , drop = FALSE] else NULL,
    note = paste(
      "Model comparison is a guide to statistical fit, not an automatic",
      "biological conclusion. Report +J and non-+J comparisons, uncertainty,",
      "and sensitivity of inferred events."
    )
  )
}
