# =========================
# Edinburgh Plotting Style
# =========================

library(ggplot2)
library(gt)
library(dplyr)
library(rlang)

source("scripts/utils/name_cleaning.R")
source("scripts/utils/model_utils.R")

# Keep the plotting utility file self-contained so the label helpers are visible here.
.variable_dictionary <- c(
  "abscore" = "Cognitive Score",
  "bloodpb" = "Blood Lead Level",
  "ageint"  = "Age",
  "sex"     = "Sex",
  "mqualif"  = "Mother's Qualifications",
  "fqualif"  = "Father's Qualifications",
  "fitted"    = "Fitted Values",
  "residuals" = "Residuals",
  "predicted_prob" = "Predicted Probability"
)

clean_basic_names <- function(names_vec) {
  names_vec <- tolower(names_vec)
  names_vec <- gsub("_", " ", names_vec)
  names_vec <- gsub("\\bmean\\b", "Mean", names_vec, ignore.case = TRUE)
  names_vec <- gsub("\\bsd\\b", "SD", names_vec, ignore.case = TRUE)
  names_vec <- gsub("\\bpredicted_prob\\b", "Predicted Probability", names_vec, ignore.case = TRUE)
  tools::toTitleCase(names_vec)
}

apply_variable_dictionary <- function(names_vec) {
  raw_names <- tolower(names_vec)
  cleaned <- ifelse(
    raw_names %in% names(.variable_dictionary),
    .variable_dictionary[raw_names],
    names_vec
  )
  cleaned
}

clean_names_edinburgh <- function(names_vec) {
  basic <- clean_basic_names(names_vec)
  final <- apply_variable_dictionary(names_vec)
  ifelse(
    tolower(names_vec) %in% names(.variable_dictionary),
    final,
    basic
  )
}

clean_labels_edinburgh <- function(x) {
  clean_names_edinburgh(x)
}

# ---- Colour palette ----
EDINBURGH_DARK   <- "#041E42"
EDINBURGH_BLUE   <- "#005EB8"
EDINBURGH_LIGHT  <- "#A7C6ED"
EDINBURGH_ACCENT <- "#E94B3C"

# =========================
# Colour scales
# =========================

scale_colour_edinburgh <- function() {
  scale_colour_manual(values = c(
    EDINBURGH_BLUE,
    EDINBURGH_ACCENT
  ))
}

scale_fill_edinburgh <- function() {
  scale_fill_manual(values = c(
    EDINBURGH_BLUE,
    EDINBURGH_ACCENT
  ))
}

# =========================
# Label helpers
# =========================

extract_var_name <- function(mapping) {
  rlang::as_name(rlang::get_expr(mapping))
}

apply_edinburgh_labels <- function(p) {
  # Always apply if mapping exists (don't rely on label checks)
  if (!is.null(p$mapping$x)) {
    x_var <- extract_var_name(p$mapping$x)
    p <- p + labs(x = clean_labels_edinburgh(x_var))
  }
  if (!is.null(p$mapping$y)) {
    y_var <- extract_var_name(p$mapping$y)
    p <- p + labs(y = clean_labels_edinburgh(y_var))
  } else {
    # Handle default stats like histogram count
    p <- p + labs(y = "Count")
  }
  return(p)
}

# =========================
# Theme
# =========================

theme_edinburgh <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        colour = EDINBURGH_DARK,
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = "grey30"
      ),
      axis.title = element_text(
        face = "bold",
        colour = EDINBURGH_DARK
      ),
      axis.text = element_text(colour = "grey20"),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.position = "top",
      legend.title = element_blank(),
      plot.caption = element_text(
        size = 9,
        colour = "grey40",
        hjust = 0
      )
      ,
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}


set_edinburgh_theme <- function() {
  theme_set(theme_edinburgh())
}

# =========================
# Custom geoms
# =========================

geom_histogram_edinburgh <- function(...) {
  geom_histogram(
    fill = EDINBURGH_BLUE,
    colour = "white",
    ...
  )
}

geom_point_edinburgh <- function(...) {
  geom_point(
    colour = EDINBURGH_BLUE,
    alpha = 0.4,
    ...
  )
}

geom_smooth_edinburgh <- function(se = TRUE, ...) {
  geom_smooth(
    method = "lm",
    colour = EDINBURGH_ACCENT,
    se = se,
    ...
  )
}

# =========================
# Survival plots
# =========================

survival_palette_edinburgh <- function(n = 2) {
  grDevices::colorRampPalette(c(
    EDINBURGH_BLUE,
    EDINBURGH_ACCENT,
    EDINBURGH_LIGHT
  ))(max(n, 1))
}

km_curve_data_edinburgh <- function(fit) {
  if (!inherits(fit, "survfit")) {
    stop("km_curve_data_edinburgh() expects a survfit object.")
  }

  fit_summary <- summary(fit)
  strata_levels <- if (!is.null(fit$strata)) names(fit$strata) else "All"
  strata_values <- if (is.null(fit_summary$strata)) {
    rep("All", length(fit_summary$time))
  } else {
    as.character(fit_summary$strata)
  }

  tibble(
    time = fit_summary$time,
    surv = fit_summary$surv,
    lower = fit_summary$lower,
    upper = fit_summary$upper,
    n_risk = fit_summary$n.risk,
    n_event = fit_summary$n.event,
    n_censor = fit_summary$n.censor,
    strata = factor(strata_values, levels = strata_levels)
  )
}

km_risk_table_data_edinburgh <- function(fit, times = NULL, n_breaks = 5) {
  if (!inherits(fit, "survfit")) {
    stop("km_risk_table_data_edinburgh() expects a survfit object.")
  }

  curve_data <- km_curve_data_edinburgh(fit)
  max_time <- max(curve_data$time, na.rm = TRUE)
  if (is.null(times)) {
    times <- pretty(c(0, max_time), n = n_breaks)
  }
  times <- sort(unique(c(0, times, max_time)))

  risk_summary <- summary(fit, times = times, extend = TRUE)
  strata_levels <- if (!is.null(fit$strata)) names(fit$strata) else "All"
  strata_values <- if (is.null(risk_summary$strata)) {
    rep("All", length(risk_summary$time))
  } else {
    as.character(risk_summary$strata)
  }

  tibble(
    time = risk_summary$time,
    n_risk = risk_summary$n.risk,
    strata = factor(strata_values, levels = strata_levels)
  )
}

km_plot_edinburgh <- function(fit,
                              title = "Kaplan--Meier survival curve",
                              subtitle = NULL,
                              x = "Follow-up time (days)",
                              y = "Estimated survival probability",
                              strata_title = "Group",
                              times = NULL,
                              risk_table = TRUE,
                              risk_table_title = "Number at risk",
                              confidence_band = TRUE,
                              confidence_alpha = 0.15,
                              line_size = 1,
                              risk_text_size = 3.8,
                              panel_heights = c(3, 1)) {
  curve_data <- km_curve_data_edinburgh(fit)
  risk_data <- km_risk_table_data_edinburgh(fit, times = times)
  strata_levels <- levels(curve_data$strata)
  if (length(strata_levels) == 0) {
    strata_levels <- "All"
  }
  palette <- survival_palette_edinburgh(length(strata_levels))
  x_breaks <- sort(unique(risk_data$time))
  x_limits <- range(x_breaks)

  curve_plot <- ggplot(curve_data, aes(x = .data$time, y = .data$surv, colour = .data$strata, fill = .data$strata)) +
    {
      if (confidence_band) {
        geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), alpha = confidence_alpha, colour = NA)
      }
    } +
    geom_step(linewidth = line_size) +
    scale_colour_manual(values = palette, 
                        ##### REMOVE THIS LINE OF CODE LATER ####
                        labels = c("New therapy", "Usual care")) +
    scale_fill_manual(values = palette, 
                      ##### REMOVE THIS LINE OF CODE LATER ####
                      labels = c("New therapy", "Usual care")) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x,
      y = y
      ##### UNCOMMENT THE LINES BELOW LATER ####
      #colour = strata_title,
      #fill = strata_title
    ) +
    theme_edinburgh()

  if (!risk_table) {
    return(curve_plot)
  }

  risk_plot <- ggplot(risk_data, aes(x = .data$time, y = .data$strata, label = .data$n_risk, colour = .data$strata)) +
    geom_text(size = risk_text_size, fontface = "bold") +
    scale_colour_manual(values = palette) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    labs(
      title = risk_table_title,
      x = x,
      y = NULL
    ) +
    theme_edinburgh() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.text.y = element_text(face = "bold"),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )

  curve_plot / risk_plot + patchwork::plot_layout(heights = panel_heights)
}

# =========================
# QQ Plot (Edinburgh Style)
# =========================

qq_plot_edinburgh <- function(residuals,
                              title = "Normal Q-Q Plot of Residuals") {
  df <- data.frame(residuals = residuals)
  p <- ggplot(df, aes(sample = residuals)) +
    stat_qq(
      colour = EDINBURGH_BLUE,
      alpha = 0.6
    ) +
    stat_qq_line(
      colour = EDINBURGH_ACCENT,
      linewidth = 1
    ) +
    labs(
      title = title,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
  return(p)
}

# =========================
# Model Diagnostics Panel
# =========================

library(patchwork)

diagnostics_panel_edinburgh <- function(model) {
  df <- data.frame(
    fitted = fitted(model),
    residuals = resid(model),
    std_resid = rstandard(model),
    leverage = hatvalues(model)
  )
  # 1. Residuals vs Fitted
  p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point_edinburgh() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = EDINBURGH_ACCENT) +
    labs(title = "Residuals vs Fitted")
  p1 <- apply_edinburgh_labels(p1) + theme_edinburgh()
  # 2. Q-Q plot
  p2 <- ggplot(df, aes(sample = residuals)) +
    stat_qq(colour = EDINBURGH_BLUE, alpha = 0.6) +
    stat_qq_line(colour = EDINBURGH_ACCENT) +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_edinburgh()
  # 3. Scale-location
  # p3 <- ggplot(df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  #   geom_point_edinburgh() +
  #   labs(title = "Scale-Location",
  #        y = "√|Standardised Residuals|") +
  #   theme_edinburgh()
  # # 4. Residuals vs leverage
  # p4 <- ggplot(df, aes(x = leverage, y = std_resid)) +
  #   geom_point_edinburgh() +
  #   geom_hline(yintercept = 0, linetype = "dashed", colour = EDINBURGH_ACCENT) +
  #   labs(title = "Residuals vs Leverage") +
  #   theme_edinburgh()
  # # Combine
  # (p1 + p2) / (p3 + p4)
  p1 + p2
}

# =========================
# Plot saving
# =========================

save_plot <- function(plot, filename, folder = "outputs", width = 6, height = 4) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  ggsave(
    filename = file.path(folder, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    limitsize = FALSE,
    bg = "white"
  )
}

# =========================
# Table styling
# =========================

table_edinburgh <- function(df, title = NULL, clean_names = TRUE, digits = 3) {
  # Apply significant figures
  df <- df %>%
    mutate(across(where(is.numeric), ~ signif(.x, digits)))
  # Clean column names
  if (clean_names) {
    new_names <- colnames(df)
    # Only clean "raw" variable names (no spaces, %, or punctuation)
    to_clean <- !grepl("[ %\\-\\.]", new_names)
    new_names[to_clean] <- clean_names_edinburgh(new_names[to_clean])
    colnames(df) <- new_names
  }
  df %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title, "**"))
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_options(
      heading.background.color = EDINBURGH_DARK,
      table.border.top.color = EDINBURGH_DARK,
      table.border.bottom.color = EDINBURGH_DARK,
      column_labels.font.weight = "bold",
      table.font.size = 12
    ) %>%
    cols_align(
      align = "center",
      everything()
    )
}

# =========================
# Table saving
# =========================

save_table <- function(gt_table, filename, folder = "outputs") {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  gtsave(
    data = gt_table,
    filename = file.path(folder, filename)
  )
}