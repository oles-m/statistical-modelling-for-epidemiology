# =========================
# Model Utilities
# =========================

library(broom)
library(dplyr)

# ---- Format model output ----
format_model_table <- function(tidy_model,
                               digits = 3,
                               measure_name = "Estimate",
                               include_cols = c("estimate", "std.error", "statistic", "CI", "p.value")) {
  tidy_model <- tidy_model %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ clean_labels_edinburgh(term)
      ),
      across(where(is.numeric), ~ signif(.x, digits)),
      CI = paste0("[", conf.low, ", ", conf.high, "]"),
      p.value = ifelse(
        p.value < 0.001,
        "<0.001",
        as.character(signif(p.value, digits))
      )
    )
  # Select columns dynamically
  col_map <- list(
    estimate = measure_name,
    std.error = "Std. Error",
    statistic = "t-Statistic",
    CI = "95% CI",
    p.value = "p-value"
  )
  cols_to_keep <- c("term", include_cols)
  tidy_model <- dplyr::select(tidy_model, dplyr::all_of(cols_to_keep))
  # Rename dynamically
  rename_vec <- c(Term = "term")
  for (col in include_cols) {
    rename_vec[[col_map[[col]]]] <- col
  }
  tidy_model <- tidy_model %>%
    rename(!!!rename_vec)
  return(tidy_model)
}

# ---- Full Edinburgh model summary ----
model_summary_edinburgh <- function(model,
                                    filename = NULL,
                                    title = NULL,
                                    folder = "outputs",
                                    conf_level = 0.95,
                                    scale = c("link", "response", "both"),
                                    include_cols = c("estimate", "CI", "p.value"),
                                    digits = 3) {
  scale <- match.arg(scale)
  is_glm <- inherits(model, "glm")
  fam <- if (is_glm) model$family$family else NULL
  # ---- Tidy model ----
  tidy_model <- broom::tidy(
    model,
    conf.int = TRUE,
    conf.level = conf_level
  )
  # ---- Handle GLM transformations ----
  if (is_glm && fam %in% c("binomial", "poisson")) {
    link_name <- ifelse(fam == "binomial", "Log-Odds", "Log-Count")
    resp_name <- ifelse(fam == "binomial", "Odds Ratio", "Rate Ratio")
    if (scale == "response") {
      tidy_model <- tidy_model %>%
        mutate(
          estimate = exp(estimate),
          conf.low = exp(conf.low),
          conf.high = exp(conf.high)
        )
      measure_name <- resp_name
    } else if (scale == "both") {
      tidy_model <- tidy_model %>%
        mutate(
          estimate_link = estimate,
          estimate_resp = exp(estimate),
          conf.low_resp = exp(conf.low),
          conf.high_resp = exp(conf.high)
        )
      tidy_model <- tidy_model %>%
        mutate(
          CI_resp = paste0("[",
                           signif(conf.low_resp, digits),
                           ", ",
                           signif(conf.high_resp, digits),
                           "]")
        )
      tidy_model <- tidy_model %>%
        select(term,
               estimate_link,
               estimate_resp,
               CI_resp,
               p.value)
      tidy_model <- tidy_model %>%
        mutate(
          term = case_when(
            term == "(Intercept)" ~ "Intercept",
            TRUE ~ clean_labels_edinburgh(term)
          ),
          across(where(is.numeric), ~ signif(.x, digits)),
          p.value = ifelse(
            p.value < 0.001,
            "<0.001",
            as.character(signif(p.value, digits))
          )
        ) %>%
        rename(
          Term = term,
          !!link_name := estimate_link,
          !!resp_name := estimate_resp,
          "95% CI (Response)" = CI_resp,
          "p-value" = p.value
        )
      model_table <- table_edinburgh(
        tidy_model,
        title = ifelse(is.null(title),
                       paste0(link_name, " and ", resp_name, " Comparison"),
                       title)
      )
      if (!is.null(filename)) {
        save_table(model_table, filename, folder)
      }
      return(model_table)
    } else {
      measure_name <- link_name
    }
  } else {
    measure_name <- "Estimate"
  }
  # ---- Standard formatting ----
  tidy_model <- format_model_table(
    tidy_model,
    digits = digits,
    measure_name = measure_name,
    include_cols = include_cols
  )
  # ---- Title ----
  if (is.null(title)) {
    title <- paste0(measure_name, " Model Results")
  }
  model_table <- table_edinburgh(
    tidy_model,
    title = title
  )
  if (!is.null(filename)) {
    save_table(model_table, filename, folder)
  }
  return(model_table)
}