# =========================
# Name Cleaning Utilities
# =========================

# ---- Variable dictionary ----
# Always store keys in lowercase
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

# ---- Base cleaning ----
clean_basic_names <- function(names_vec) {
  names_vec <- tolower(names_vec)
  names_vec <- gsub("_", " ", names_vec)
  names_vec <- gsub("\\bmean\\b", "Mean", names_vec, ignore.case = TRUE)
  names_vec <- gsub("\\bsd\\b", "SD", names_vec, ignore.case = TRUE)
  names_vec <- gsub("\\bpredicted_prob\\b", "Predicted Probability", names_vec, ignore.case = TRUE)
  tools::toTitleCase(names_vec)
}

# ---- Apply dictionary ----
apply_variable_dictionary <- function(names_vec) {
  # Work in lowercase for matching
  raw_names <- tolower(names_vec)
  # Replace where dictionary exists
  cleaned <- ifelse(
    raw_names %in% names(.variable_dictionary),
    .variable_dictionary[raw_names],
    names_vec
  )
  cleaned
}

# ---- Full cleaning pipeline ----
clean_names_edinburgh <- function(names_vec) {
  basic <- clean_basic_names(names_vec)
  final <- apply_variable_dictionary(names_vec)
  # Use dictionary where available, otherwise fallback to basic
  ifelse(
    tolower(names_vec) %in% names(.variable_dictionary),
    final,
    basic
  )
}

# ---- Labels (alias for clarity in plotting context) ----
clean_labels_edinburgh <- function(x) {
  clean_names_edinburgh(x)
}