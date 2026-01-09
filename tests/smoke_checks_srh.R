# tests/smoke_checks_srh.R
library(dplyr)
library(rlang)

abort_if <- function(cond, msg) if (isTRUE(cond)) abort(msg)

assert_has_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  abort_if(length(missing) > 0, paste("Missing required columns:", paste(missing, collapse = ", ")))
}

assert_srh_ranges <- function(df) {
  assert_has_cols(df, c("dataset", "srh", "srh_scale_max"))
  # check scale max
  bad_max <- df %>%
    distinct(dataset, srh_scale_max) %>%
    filter(!(dataset == "GSS" & srh_scale_max == 4) &
           !(dataset != "GSS" & srh_scale_max == 5))
  abort_if(nrow(bad_max) > 0, "srh_scale_max mismatch: expected 4 for GSS, 5 otherwise.")

  # check srh bounds by dataset
  bad <- df %>%
    filter(!is.na(srh)) %>%
    mutate(lo = 1,
           hi = if_else(dataset == "GSS", 4, 5),
           out = (srh < lo | srh > hi)) %>%
    filter(out)
  abort_if(nrow(bad) > 0, "srh out of bounds for at least one dataset.")
}

assert_fairpoor_definition <- function(df) {
  if (!("srh_fairpoor" %in% names(df))) return(invisible(TRUE))
  ok <- df %>%
    filter(!is.na(srh), !is.na(srh_fairpoor)) %>%
    mutate(expected = as.integer(srh <= 2)) %>%
    filter(srh_fairpoor != expected)
  abort_if(nrow(ok) > 0, "srh_fairpoor does not equal (srh <= 2) for some rows.")
}

assert_weights_sane <- function(df) {
  if (!("w" %in% names(df))) return(invisible(TRUE))
  bad <- df %>% filter(!is.na(w) & w < 0)
  abort_if(nrow(bad) > 0, "Negative weights detected.")
}

run_smoke_checks_srh <- function(person_df) {
  assert_has_cols(person_df, c("dataset", "year", "age", "srh"))
  assert_srh_ranges(person_df)
  assert_fairpoor_definition(person_df)
  assert_weights_sane(person_df)
  invisible(TRUE)
}
