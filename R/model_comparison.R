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

model_sensitivity_summary_table <- function(comparison, sensitivity = assess_model_sensitivity(comparison), delta_threshold = 2) {
  if (is.null(comparison) || nrow(comparison) == 0L) {
    return(data.frame())
  }
  if (!"has_j" %in% names(comparison)) {
    comparison$has_j <- is_j_model(comparison$model)
  }
  if (!"delta_aicc" %in% names(comparison)) {
    comparison$delta_aicc <- comparison$AICc - min(comparison$AICc, na.rm = TRUE)
  }

  best <- sensitivity$best_overall
  best_non_j <- sensitivity$best_non_j
  best_plus_j <- sensitivity$best_plus_j
  plus_j_near_best <- comparison[comparison$has_j & comparison$delta_aicc <= delta_threshold, , drop = FALSE]
  non_j_near_best <- comparison[!comparison$has_j & comparison$delta_aicc <= delta_threshold, , drop = FALSE]

  rows <- list(
    sensitivity_summary_row(
      summary_item = "best_overall_model",
      section = "Fit summary",
      display_label = "Best statistical model",
      answer = best$model[1L],
      models = best$model,
      model_count = nrow(best),
      evidence = format_model_evidence(best),
      interpretation_note = best$interpretation_note[1L] %||% sensitivity$note
    ),
    sensitivity_summary_row(
      summary_item = "best_overall_is_plus_j",
      section = "Sensitivity",
      display_label = "Best model includes +J",
      answer = if (isTRUE(best$has_j[1L])) "yes" else "no",
      models = best$model,
      model_count = nrow(best),
      evidence = paste0("Best statistical model: ", best$model[1L]),
      interpretation_note = if (isTRUE(best$has_j[1L])) {
        "The best-fitting statistical model includes +J; report sensitivity instead of declaring a simple biological conclusion."
      } else {
        "The best-fitting statistical model does not include +J; still compare against +J alternatives and report uncertainty."
      }
    ),
    sensitivity_summary_row(
      summary_item = "plus_j_models_within_delta_aicc_2",
      section = "Sensitivity",
      display_label = "+J model within delta AICc <= 2",
      answer = if (nrow(plus_j_near_best) > 0L) "yes" else "no",
      models = plus_j_near_best$model,
      model_count = nrow(plus_j_near_best),
      evidence = format_model_evidence(plus_j_near_best),
      interpretation_note = if (nrow(plus_j_near_best) > 0L) {
        "At least one +J model is within delta AICc <= 2; include explicit +J sensitivity cautions."
      } else {
        "No +J model is within delta AICc <= 2 in this run."
      }
    ),
    sensitivity_summary_row(
      summary_item = "best_plus_j_model",
      section = "Paired comparison",
      display_label = "Best +J model",
      answer = if (!is.null(best_plus_j)) best_plus_j$model[1L] else "not available",
      models = if (!is.null(best_plus_j)) best_plus_j$model else character(),
      model_count = if (!is.null(best_plus_j)) nrow(best_plus_j) else 0L,
      evidence = format_model_evidence(best_plus_j),
      interpretation_note = "Best +J model shown for comparison with non-+J alternatives."
    ),
    sensitivity_summary_row(
      summary_item = "best_non_j_model",
      section = "Paired comparison",
      display_label = "Best non-+J model",
      answer = if (!is.null(best_non_j)) best_non_j$model[1L] else "not available",
      models = if (!is.null(best_non_j)) best_non_j$model else character(),
      model_count = if (!is.null(best_non_j)) nrow(best_non_j) else 0L,
      evidence = format_model_evidence(best_non_j),
      interpretation_note = "Best non-+J model shown as a baseline for sensitivity interpretation."
    ),
    sensitivity_summary_row(
      summary_item = "non_j_models_within_delta_aicc_2",
      section = "Sensitivity",
      display_label = "Non-+J model within delta AICc <= 2",
      answer = if (nrow(non_j_near_best) > 0L) "yes" else "no",
      models = non_j_near_best$model,
      model_count = nrow(non_j_near_best),
      evidence = format_model_evidence(non_j_near_best),
      interpretation_note = if (nrow(non_j_near_best) > 0L) {
        "At least one non-+J model is near-best; include it in biological interpretation."
      } else {
        "No non-+J model is within delta AICc <= 2 in this run."
      }
    ),
    sensitivity_summary_row(
      summary_item = "auto_declare_best_model",
      section = "Guardrail",
      display_label = "Automatically declare best biological model",
      answer = "no",
      models = character(),
      model_count = NA_integer_,
      evidence = "Configured methodological guardrail: auto_declare_best_model = false",
      interpretation_note = sensitivity$note
    )
  )

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

sensitivity_summary_row <- function(summary_item, section, display_label, answer, models, model_count, evidence, interpretation_note) {
  data.frame(
    summary_item = summary_item,
    section = section,
    display_label = display_label,
    answer = answer %||% NA_character_,
    models = if (length(models) > 0L) paste(models, collapse = "; ") else NA_character_,
    model_count = model_count,
    evidence = evidence %||% NA_character_,
    interpretation_note = interpretation_note %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

format_model_evidence <- function(x) {
  if (is.null(x) || nrow(x) == 0L) {
    return(NA_character_)
  }
  pieces <- vapply(seq_len(nrow(x)), function(i) {
    row <- x[i, , drop = FALSE]
    evidence <- row$model
    if ("AICc" %in% names(row)) {
      evidence <- paste0(evidence, " AICc=", round(row$AICc, 4))
    }
    if ("delta_aicc" %in% names(row)) {
      evidence <- paste0(evidence, " delta_aicc=", round(row$delta_aicc, 4))
    }
    if ("aicc_weight" %in% names(row)) {
      evidence <- paste0(evidence, " weight=", round(row$aicc_weight, 4))
    }
    evidence
  }, character(1))
  paste(pieces, collapse = " | ")
}
