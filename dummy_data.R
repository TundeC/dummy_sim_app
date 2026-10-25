# =============================================================================
# dummy_data.R — Placeholder data for the public demo version of the dashboard
# =============================================================================
# EVERY NUMBER IN THIS FILE IS INVENTED. Nothing here is derived from real
# screening data, and none of the real simulation logic is included.
#
# This file stands in for two things the full app relies on:
#   1. processed_data/sim_params_by_age.rds  -> DUMMY_SIM_PARAMS
#   2. run_des_simulation()                  -> dummy_run_des_simulation()
#
# The outputs have exactly the same structure (object names, column names,
# column types) as the real ones, so the UI code works unchanged. The values
# are produced by simple arbitrary arithmetic plus random noise purely so that
# the charts, tables and confidence intervals look realistic in a walkthrough.
# =============================================================================

DUMMY_BANDS <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
DUMMY_YEARS <- 2025:2035   # all years selectable in the UI

# ── Stand-in for sim_params_by_age.rds (invented values) ────────────────────
DUMMY_SIM_PARAMS <- data.frame(
  age_band   = DUMMY_BANDS,
  n_invited  = c(90000, 86000, 82000, 78000, 80000, 82000, 76000, 68000),
  n_screened = c(55000, 56000, 56000, 55000, 58000, 60000, 55000, 47000),
  p_attend   = c(0.610, 0.650, 0.680, 0.705, 0.725, 0.730, 0.720, 0.690),
  p_hpv_pos  = c(0.220, 0.160, 0.120, 0.095, 0.080, 0.070, 0.065, 0.060),
  stringsAsFactors = FALSE
)

# ── Build a full set of dummy replication-level results ─────────────────────
# Generated once at start-up for every year x band x replication, then
# filtered to the selected year range when "Run Simulation" is clicked.
.make_dummy_band_reps <- function(n_rep = 10, seed = 2024) {
  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed)

  grid <- expand.grid(replication = seq_len(n_rep),
                      age_band    = DUMMY_BANDS,
                      year        = DUMMY_YEARS,
                      stringsAsFactors = FALSE)

  sp  <- DUMMY_SIM_PARAMS[match(grid$age_band, DUMMY_SIM_PARAMS$age_band), ]
  t   <- grid$year - min(DUMMY_YEARS)
  jit <- function(sd) rnorm(nrow(grid), 1, sd)

  # Arbitrary smooth trend + a gentle wave so years look different
  screened <- round(sp$n_screened * (1 + 0.006 * t) * (1 + 0.015 * sin(t / 1.7)))
  attended <- round(screened * sp$p_attend * jit(0.004))
  hpv      <- round(attended * 1.03 * jit(0.004))
  hpv_pos  <- round(hpv * sp$p_hpv_pos * jit(0.02))
  cyto     <- round(hpv_pos * 0.92 * jit(0.025))
  colp_new <- round(hpv_pos * 0.18 * jit(0.05))
  colp_ex  <- round(hpv_pos * 0.10 * (1 + 0.02 * t) * jit(0.06))
  treated  <- round((colp_new + colp_ex) * 0.30 * jit(0.07))

  data.frame(
    year           = grid$year,
    age_band       = grid$age_band,
    replication    = grid$replication,
    screened       = screened,          # same in every rep (no CI shown in UI)
    attended       = attended,
    hpv_tests      = hpv,
    hpv_positive   = hpv_pos,
    cytology_tests = cyto,
    colposcopy     = colp_new + colp_ex,
    colp_routine   = colp_new,
    colp_nr        = colp_ex,
    treated        = treated,
    stringsAsFactors = FALSE
  )
}

DUMMY_BAND_REPS <- .make_dummy_band_reps()

# ── Stand-in for run_des_simulation() ───────────────────────────────────────
# Same signature as the real function so the call in app.R is unchanged.
# Only the year range is used; all other arguments are accepted and ignored.
dummy_run_des_simulation <- function(
    year_start            = 2025,
    year_end              = 2035,
    n_rep                 = 10,
    seed                  = 42,
    scale_factor          = 0.5,
    age_stratified        = TRUE,
    attend_uplift_by_year = NULL,
    hpv_adj_by_year       = NULL) {

  years <- seq(year_start, year_end)
  if (!all(years %in% DUMMY_YEARS))
    stop("Dummy results are only available for ", min(DUMMY_YEARS),
         "-", max(DUMMY_YEARS))

  band_reps <- DUMMY_BAND_REPS[DUMMY_BAND_REPS$year %in% years, ]

  # Scotland totals per replication
  raw <- band_reps %>%
    dplyr::group_by(year, replication) %>%
    dplyr::summarise(dplyr::across(screened:treated, sum), .groups = "drop")

  band_summary <- band_reps %>%
    dplyr::group_by(year, age_band) %>%
    dplyr::summarise(
      screened_mean     = round(mean(screened)),
      attended_mean     = round(mean(attended)),
      hpv_tests_mean    = round(mean(hpv_tests)),
      hpv_pos_mean      = round(mean(hpv_positive)),
      cyto_mean         = round(mean(cytology_tests)),
      colp_mean         = round(mean(colposcopy)),
      colp_routine_mean = round(mean(colp_routine)),
      colp_nr_mean      = round(mean(colp_nr)),
      treated_mean      = round(mean(treated)),
      .groups = "drop")

  ci <- function(x, conf = 0.95) {
    n <- length(x); se <- sd(x) / sqrt(n); q <- qt((1 + conf) / 2, df = n - 1)
    c(lo = round(mean(x) - q * se), hi = round(mean(x) + q * se))
  }

  des_summary <- raw %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      screened_mean     = round(mean(screened)),
      attended_mean     = round(mean(attended)),
      hpv_tests_mean    = round(mean(hpv_tests)),
      hpv_tests_lo      = ci(hpv_tests)["lo"],
      hpv_tests_hi      = ci(hpv_tests)["hi"],
      hpv_pos_mean      = round(mean(hpv_positive)),
      cyto_mean         = round(mean(cytology_tests)),
      cyto_lo           = ci(cytology_tests)["lo"],
      cyto_hi           = ci(cytology_tests)["hi"],
      colp_mean         = round(mean(colposcopy)),
      colp_lo           = ci(colposcopy)["lo"],
      colp_hi           = ci(colposcopy)["hi"],
      colp_routine_mean = round(mean(colp_routine)),
      colp_routine_lo   = ci(colp_routine)["lo"],
      colp_routine_hi   = ci(colp_routine)["hi"],
      colp_nr_mean      = round(mean(colp_nr)),
      colp_nr_lo        = ci(colp_nr)["lo"],
      colp_nr_hi        = ci(colp_nr)["hi"],
      treated_mean      = round(mean(treated)),
      treated_lo        = ci(treated)["lo"],
      treated_hi        = ci(treated)["hi"],
      .groups           = "drop")

  list(raw = raw, des_summary = des_summary, band_summary = band_summary)
}
