# =============================================================================
# dummy_data.R — Placeholder data for the public demo version of the dashboard
# =============================================================================
# EVERY NUMBER IN THIS FILE IS INVENTED. Nothing here is derived from real
# screening data, and none of the real simulation logic is included.
#
# This file stands in for everything the full app takes from the real data,
# the simulation engine and its helper modules:
#   1. Per-band baseline rates                -> DUMMY_SIM_PARAMS
#   2. run_des_simulation()                   -> dummy_run_des_simulation()
#   3. Pathway version registry (PATHWAYS)    -> invented demo versions
#   4. Pathway diagrams                       -> generated placeholder images
#   5. get_parameter_table()                  -> invented parameter table
#   6. Vaccination scenario helpers           -> simple stand-in arithmetic
#
# The outputs have exactly the same structure (object names, column names,
# column types) as the real ones, so the UI code works unchanged. The values
# are produced by simple arbitrary arithmetic plus random noise purely so that
# the charts, tables and confidence intervals look realistic in a walkthrough.
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

DUMMY_BANDS <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
DUMMY_YEARS <- 2025:2035   # all years selectable in the UI

# ── Stand-in for sim_params_by_age.rds (invented values) ────────────────────
DUMMY_SIM_PARAMS <- data.frame(
  age_band   = DUMMY_BANDS,
  n_invited  = c(90000, 86000, 82000, 78000, 80000, 82000, 76000, 68000),
  n_screened = c(55000, 56000, 56000, 55000, 58000, 60000, 55000, 47000),
  p_attend   = c(0.610, 0.650, 0.680, 0.705, 0.725, 0.730, 0.720, 0.690),
  p_hpv_pos  = c(0.220, 0.160, 0.120, 0.095, 0.080, 0.070, 0.065, 0.060),
  p_cyto_lg  = c(0.300, 0.270, 0.250, 0.240, 0.230, 0.220, 0.210, 0.200),
  p_cyto_hg  = c(0.080, 0.070, 0.060, 0.050, 0.045, 0.040, 0.040, 0.035),
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
# Only the year range is used; all other arguments (including the pathway
# version and its inputs) are accepted and ignored.
dummy_run_des_simulation <- function(
    year_start            = 2025,
    year_end              = 2035,
    n_rep                 = 10,
    seed                  = 42,
    scale_factor          = 0.5,
    age_stratified        = TRUE,
    attend_uplift_by_year = NULL,
    hpv_adj_by_year       = NULL,
    age_band_overrides    = NULL,
    pathway_inputs        = NULL,
    progress_callback     = NULL) {

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

# =============================================================================
# Pathway versions (stand-in for the pathway_*.R files)
# =============================================================================
# The real app builds PATHWAYS from pathway_*.R files. Here the versions are
# declared directly. Labels, descriptions and input names are placeholders --
# edit them freely; the UI adapts automatically to whatever is listed here.
# "vaccination" must keep that id: the app sends it to the Vaccination page.
# The baseline ("current") pathway is deliberately not registered: an empty
# dropdown means baseline, exactly as in the full app.

PATHWAYS <- list()
register_pathway <- function(id, label, description = NULL, inputs = list()) {
  PATHWAYS[[id]] <<- list(label = label, description = description,
                          diagram_file = id, inputs = inputs)
  invisible(id)
}

register_pathway(
  "version_b", "Version B (demo)",
  "Demo pathway version B. Inputs below are placeholders and do not change the illustrative results.",
  inputs = list(list(id = "input_1", label = "Example input 1 (% by age band and year)")))

register_pathway(
  "version_c", "Version C (demo)",
  "Demo pathway version C. Inputs below are placeholders and do not change the illustrative results.",
  inputs = list(list(id = "input_1", label = "Example input 2 (% by age band and year)")))

register_pathway(
  "version_d", "Version D (demo)",
  "Demo pathway version D. Inputs below are placeholders and do not change the illustrative results.",
  inputs = list(list(id = "input_1", label = "Example input 3 (% by age band and year)")))

register_pathway(
  "vaccination", "Vaccination impact",
  "Vaccination impact -- inputs are entered on the Vaccination page.",
  inputs = list())

# =============================================================================
# Placeholder pathway diagrams (stand-in for data_raw/Model_diagrams/)
# =============================================================================
# A generic boxes-and-arrows picture, drawn once per version into tempdir().
# It is deliberately large so the "Diagram size" slider and the scrollable
# frame behave like they do with the real diagrams.
DUMMY_DIAGRAM_W <- 1600
DUMMY_DIAGRAM_H <- 1000

dummy_diagram <- function(pathway = "current") {
  path <- file.path(tempdir(), sprintf("demo_diagram_%s.png",
                                       gsub("[^A-Za-z0-9_]", "_", pathway)))
  if (!file.exists(path)) {
    label <- PATHWAYS[[pathway]]$label %||% "Current pathway (baseline)"
    grDevices::png(path, width = DUMMY_DIAGRAM_W, height = DUMMY_DIAGRAM_H)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op), add = TRUE)
    graphics::plot.new(); graphics::plot.window(c(0, 16), c(0, 10))
    graphics::rect(0, 0, 16, 10, col = "white", border = NA)
    graphics::text(8, 9.4, sprintf("Pathway diagram placeholder -- %s", label),
                   cex = 2.6, font = 2, col = "#2c3e50")
    graphics::text(8, 8.7, "Demo version: generic illustration, not the real model structure",
                   cex = 1.6, col = "#7f8c8d")
    xs <- c(1.5, 5, 8.5, 12, 14.8); ys <- c(6.5, 3)
    k <- 0
    for (y in ys) for (x in xs) {
      k <- k + 1
      graphics::rect(x - 1.1, y - 0.7, x + 1.1, y + 0.7,
                     col = "#eaf2f8", border = "#2980b9", lwd = 3)
      graphics::text(x, y, sprintf("Step %d", k), cex = 1.8, col = "#1a5276")
    }
    for (y in ys) for (i in seq_len(length(xs) - 1))
      graphics::arrows(xs[i] + 1.15, y, xs[i + 1] - 1.15, y,
                       lwd = 3, length = 0.15, col = "#7f8c8d")
    graphics::arrows(xs[5], ys[1] - 0.75, xs[1], ys[2] + 0.75,
                     lwd = 3, length = 0.15, col = "#7f8c8d", lty = 2)
    graphics::text(15.6, 0.4, "DEMO", cex = 3, col = "#e5e8e8", font = 2, adj = 1)
  }
  list(path = path, width = DUMMY_DIAGRAM_W)
}

# =============================================================================
# Parameter table (stand-in for get_parameter_table())
# =============================================================================
# Invented arrow IDs, generic descriptions and arbitrary values. Same columns
# as the real function: arrow_id, description, status, value.
.dummy_arrows <- c(paste0("A", 1:3), paste0("B", 1:4), paste0("C", 1:3),
                   paste0("D", 1:3), paste0("E", 1:2), paste0("F", 1:2),
                   paste0("G", 1:2), paste0("H", 1:4))

get_parameter_table <- function(age_band, model_env = NULL) {
  band_i <- match(age_band, DUMMY_BANDS)
  if (is.na(band_i)) band_i <- 1L
  n <- length(.dummy_arrows)
  i <- seq_len(n)
  data.frame(
    arrow_id    = .dummy_arrows,
    description = sprintf("Illustrative transition probability %s (demo)", .dummy_arrows),
    status      = ifelse(i %% 6 == 0, "PLACEHOLDER", "calibrated"),
    value       = round(0.05 + 0.9 * ((i * 37 + band_i * 11) %% 100) / 100, 3),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Vaccination scenario helpers (stand-in for vaccination_scenario_module.R)
# =============================================================================
# Same function names, arguments and return shapes as the app expects.
# Factor = 1 - proportion vaccinated x vaccine effect (as described on the
# Vaccination page), applied to the dummy baselines above.

vax_fin_year <- function(yr) sprintf("%d/%02d", as.integer(yr), (as.integer(yr) + 1L) %% 100L)

vax_empty_uptake <- function(years = 2025:2035) {
  df <- data.frame(`Age band` = DUMMY_BANDS, check.names = FALSE,
                   stringsAsFactors = FALSE)
  for (y in years) df[[as.character(y)]] <- NA_real_
  df
}

vax_empty_manual <- function() {
  data.frame(year = integer(0), age_band = character(0),
             param_name = character(0), factor = numeric(0),
             stringsAsFactors = FALSE)
}

.vax_params <- data.frame(
  param_name  = c("p_hpv_pos", "p_cyto_lg", "p_cyto_hg"),
  arrow_id    = c("B1", "C1", "C2"),
  param_label = c("HPV positivity", "Low-grade / borderline cytology",
                  "High-grade cytology"),
  eff_key     = c("hpv", "lg", "hg"),
  stringsAsFactors = FALSE
)

vax_build_year_grid <- function(year, uptake, eff, manual, model_env = NULL) {
  y <- as.character(year)
  rows <- lapply(DUMMY_BANDS, function(ab) {
    bi <- match(ab, uptake[["Age band"]])
    u  <- if (y %in% names(uptake)) uptake[[y]][bi] else NA_real_
    do.call(rbind, lapply(seq_len(nrow(.vax_params)), function(k) {
      pn <- .vax_params$param_name[k]
      ed <- eff[[.vax_params$eff_key[k]]]
      e  <- if (!is.null(ed) && y %in% names(ed))
        ed[[y]][match(ab, ed[["Age band"]])] else NA_real_
      derived <- 1 - (if (is.na(u)) 0 else u) / 100 * (if (is.na(e)) 0 else e) / 100
      mi <- which(manual$year == as.integer(year) & manual$age_band == ab &
                    manual$param_name == pn)
      fac <- if (length(mi)) manual$factor[mi[1]] else derived
      src <- if (length(mi)) "Manual" else if (abs(derived - 1) > 1e-9) "Derived" else "Baseline"
      base <- DUMMY_SIM_PARAMS[[pn]][DUMMY_SIM_PARAMS$age_band == ab]
      data.frame(age_band = ab, arrow_id = .vax_params$arrow_id[k],
                 param_name = pn, param_label = .vax_params$param_label[k],
                 uptake_pct = u, effect_pct = e, factor = fac,
                 baseline = base, adjusted = min(1, base * fac), source = src,
                 stringsAsFactors = FALSE)
    }))
  })
  do.call(rbind, rows)
}

# Display version: Factor must be column index 5 (0-based) -- the only
# editable column in the app's table.
format_vax_year_for_display <- function(g) {
  pct <- function(x) ifelse(is.na(x), "--", sprintf("%.0f", x))
  data.frame(
    `Age band`                  = g$age_band,
    Arrow                       = g$arrow_id,
    Parameter                   = g$param_label,
    `Proportion vaccinated (%)` = pct(g$uptake_pct),
    `Vaccine effect (%)`        = pct(g$effect_pct),
    Factor                      = round(g$factor, 3),
    Baseline                    = sprintf("%.2f%%", g$baseline * 100),
    Adjusted                    = sprintf("%.2f%%", g$adjusted * 100),
    Source                      = g$source,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

build_vax_overrides <- function(years, uptake, eff, manual, model_env = NULL) {
  out <- list()
  for (yr in years) {
    g <- vax_build_year_grid(yr, uptake, eff, manual, model_env)
    g <- g[abs(g$factor - 1) > 1e-9, , drop = FALSE]
    if (nrow(g)) out[[as.character(yr)]] <- g[, c("age_band", "param_name", "factor")]
  }
  out
}
