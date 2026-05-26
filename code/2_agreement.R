##############################################################################
# ---- INTERRATER AGREEMENT AND POINT(S) OF STABILITY ----
# --- Code for Results: Agreement Indicators and Results: Points of stability
##############################################################################

# pull out relevant data
data_trait_std <- data_exp |>
  filter(exp == "attractive" | exp == "dominant"  | exp == "trustworthy"  | exp == "gender-typical"  | exp == "memorable") |>
  mutate(exp = factor(exp, levels = c("attractive", "dominant", "trustworthy", "gender-typical", "memorable"))) |>
  mutate(dv = as.numeric(dv))

data_trait_unstd <- data_exp |>
  filter(grepl("unstd", exp)) |>
  mutate(dv = as.numeric(dv)) |>
  droplevels()

data_emo_int <- data_exp |>
  filter(exp == "anger" | exp == "happiness"  | exp == "disgust"  | exp == "surprise"  | exp == "sadness" | exp == "fear") |>
  mutate(dv = as.numeric(dv)) |>
  droplevels()

data_emo_cat <- data_exp |>
  filter(grepl("^em", exp)) |>
  droplevels()

# ------------------------------------------------------------------
# ---- AGREEMENT INDICATORS FOR THE RATINGS -----
# ------------------------------------------------------------------

# ICC
icc_table_std <- data_trait_std |>
  group_by(exp) |>
  group_map(calc_icc) |>
  bind_rows() |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

icc_table_unstd <- data_trait_unstd |>
  group_by(exp) |>
  group_map(calc_icc, check_dropped_raters = FALSE) |>
  bind_rows() |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

icc_table_emo <- data_emo_int |>
  group_by(exp) |>
  group_map(calc_icc) |>
  bind_rows() |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# Corridor of stability
results_trait_std   <- run_corridor_pipeline(data_trait_std)
results_trait_unstd <- run_corridor_pipeline(data_trait_unstd)
results_emo_int     <- run_corridor_pipeline(data_emo_int)

# ------------------------------------------------------------------
# --- POINTS OF STABILITY
# ------------------------------------------------------------------

# --- STANDARDISED RATINGS

# To save time, output cached
if (!file.exists("cache/pos_traits_std.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_traits <- calc_stability_stats(data = data_trait,
                                                 N = 100,
                                                 iterations = 300,
                                                 ci_interval = 0.95, ci_method = "percentile",
                                                 cos_threshold = 0.5,
                                                 save_means = FALSE, # only save if actually needed (huge)
                                                 col_map = list(trait = "exp",
                                                                stim_id = "trial_name",
                                                                rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_traits, "cache/pos_traits_std.rds")
} else {
  stability_stats_traits <- readRDS("cache/pos_traits_std.rds")
}

summary_ci_traits <- stability_stats_traits$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")


# --- UNSTANDARDISED RATINGS
if (!file.exists("cache/pos_traits_unstd.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_traits_unstd <- calc_stability_stats(data = data_trait_unstd,
                                                       N = 100,
                                                       iterations = 300,
                                                       ci_interval = 0.95, ci_method = "percentile",
                                                       cos_threshold = 0.5,
                                                       save_means = FALSE,
                                                       col_map = list(trait = "exp",
                                                                      stim_id = "trial_name",
                                                                      rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_traits_unstd, "cache/pos_traits_unstd.rds")
} else {
  stability_stats_traits_unstd <- readRDS("cache/pos_traits_unstd.rds")
}

summary_ci_traits_unstd <- stability_stats_traits_unstd$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")


# --- EMOTION INTENSITY RATINGS
if (!file.exists("cache/pos_emo.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_emo_int <- calc_stability_stats(data = data_emo_int,
                                                  N = 100,
                                                  iterations = 300,
                                                  ci_interval = 0.95, ci_method = "percentile",
                                                  cos_threshold = 0.5,
                                                  save_means = FALSE,
                                                  col_map = list(trait = "exp",
                                                                 stim_id = "trial_name",
                                                                 rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_emo_int, "cache/pos_emo.rds")
} else {
  stability_stats_emo_int <- readRDS("cache/pos_emo.rds")
}

summary_ci_emo_int <- stability_stats_emo_int$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")
