# =============================================================================
# app.R — Scottish Cervical Screening Programme DES Dashboard  [DEMO VERSION]
# =============================================================================
# PUBLIC DEMO / UI WALKTHROUGH BUILD
# All real data, data preparation, simulation code, pathway definitions,
# model diagrams and parameter values have been removed.
# Results, parameters and diagrams shown are INVENTED placeholders generated
# in dummy_data.R. The UI and its behaviour are otherwise identical to the
# full app.
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(openxlsx)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Dummy data, dummy pathway versions and dummy helpers in place of the real
# simulation engine, processed data and helper modules
source("dummy_data.R")

# ── Demo settings ────────────────────────────────────────────────────────────
DEMO_RUN_SECONDS      <- 5      # fake run time so the progress bar is visible
DEMO_PRELOAD_RESULTS  <- FALSE  # TRUE = show default results (2025–2029) on start-up
DEMO_SHOW_BANNER      <- TRUE   # show a "demo data" notice at the top of the page
DEMO_PARALLEL_WORKERS <- 3L     # worker count displayed in the sidebar

# =============================================================================
# UI
# =============================================================================

# Pathway versions come from PATHWAYS (defined in dummy_data.R for the demo).
pathway_choices <- function() {
  if (length(PATHWAYS) == 0) {
    warning("No pathway_*.R files were loaded -- no pathway versions are available. ",
            "Save the pathway_*.R files in the app folder (", getwd(),
            ") and restart R.")
    return(setNames("", ""))
  }
  nm   <- names(PATHWAYS)
  labs <- vapply(nm, function(n) {
    l <- PATHWAYS[[n]]$label
    if (is.null(l) || !nzchar(l)) n else l
  }, character(1))
  setNames(c("", nm), c("", labs))      # "" = baseline, shown as the placeholder
}

# Selected pathway version; empty / NULL = "current" (baseline)
selected_pathway <- function(x) if (is.null(x) || length(x) == 0 || !nzchar(x[1])) "current" else x[1]

ui <- dashboardPage(
  skin = "blue",
  
  # ── Header ─────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = "Scottish Cervical Screening — DES",
    titleWidth = 360
  ),
  
  # ── Sidebar ────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Dashboard",  tabName = "dashboard",  icon = icon("dashboard")),
      menuItem("Parameters", tabName = "parameters", icon = icon("table")),
      # Version input pages: NOT shown in the menu. Choosing a version in the
      # dropdown below opens its page (hidden items keep navigation working).
      tagAppendAttributes(
        menuItem("Vaccination inputs", tabName = "vaccination", icon = icon("syringe")),
        style = "display:none;"),
      tagAppendAttributes(
        menuItem("Pathway version inputs", tabName = "pathway_inputs",
                 icon = icon("project-diagram")),
        style = "display:none;")
    ),
    
    hr(),
    
    h4("Simulation Settings",
       style = "padding-left:15px; color:#ecf0f1; margin-bottom:10px;"),
    
    # Year range — two dropdowns instead of slider
    div(style = "padding: 0 15px;",
        tags$label("Projection Years:", style = "color:#bdc3c7; font-size:12px;"),
        fluidRow(
          column(6,
                 selectInput("year_start", "From:",
                             choices  = 2025:2035,
                             selected = 2025,
                             width    = "100%")
          ),
          column(6,
                 selectInput("year_end", "To:",
                             choices  = 2025:2035,
                             selected = 2029,
                             width    = "100%")
          )
        )
    ),
    
    
    # Pathway version (ADDED 2026-10-12)
    div(style = "padding: 0 15px;",
        # No "Current" entry: leaving it empty = the current pathway (baseline).
        selectizeInput("pathway_version", "Pathway version (optional):",
                       choices  = pathway_choices(),
                       selected = "",
                       width    = "100%",
                       options  = list(
                         placeholder = "None -- current pathway (baseline)",
                         plugins     = list("clear_button"))),
        tags$p(style = "color:#95a5a6; font-size:10px; margin:-8px 0 6px 0;",
               "Leave empty for the baseline (current pathway) run. Choosing a version",
               " opens its input page."),
        if (length(PATHWAYS) == 0)
          tags$p(style = "color:#e74c3c; font-size:10px; margin:0 0 6px 0;",
                 "No pathway versions loaded. R is looking in: ", getwd(),
                 ". Files there with 'pathway' in the name: ",
                 {
                   f <- list.files(".", pattern = "pathway", ignore.case = TRUE)
                   if (length(f)) paste(f, collapse = ", ") else "none"
                 },
                 ". Engine file loaded? ",
                 if (exists("register_pathway")) "yes (new version)" else
                   "NO -- the engine is an old version without pathway support.")
    ),
    
    # CI width indicator
    div(style = "padding: 0 15px 5px 15px;",
        uiOutput("ci_width_ui")
    ),
    
    # Scenario Adjustments
    div(style = "padding: 0 15px 5px 15px;",
        tags$label(
          icon("sliders-h"), " Scenario Adjustments:",
          style = "color:#bdc3c7; font-size:12px;"
        ),
        tags$p(style = "color:#7f8c8d; font-size:9px; margin:4px 0;",
               icon("info-circle"),
               " Wired into the engine (2026-09-30): applies a relative %",
               " change to attendance / HPV positivity for the chosen",
               " year x age band on your next Run Simulation click."),
        
        # Dynamic year × band adjustment panels
        tags$p(style = "color:#95a5a6; font-size:10px; margin:6px 0 4px 0;",
               "Scenario Adjustments (per year × age band):"),
        uiOutput("year_band_panels_ui")
    ),
    
    hr(),
    
    # Run button
    div(style = "padding: 0 15px;",
        actionButton(
          "run_sim", "Run Simulation",
          icon  = icon("play"),
          class = "btn-success btn-lg",
          style = "width:100%; font-size:15px; font-weight:bold;"
        )
    ),
    
    br(),
    
    div(style = "padding: 0 15px;",
        uiOutput("sim_status_ui")
    ),
    
    br(),
    
    div(style = "padding: 0 15px 10px 15px;",
        uiOutput("parallel_status_ui")
    )
    
  ),
  
  # ── Body ───────────────────────────────────────────────────────────────────
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .box { border-top: 3px solid #3498db; border-radius: 4px; }
      .kpi-card {
        background: white; border-radius: 8px; padding: 16px 12px;
        text-align: center; box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 15px; border-left: 4px solid #3498db;
      }
      .kpi-value { font-size: 26px; font-weight: 700; color: #2c3e50; }
      .kpi-ci    { font-size: 13px; color: #2c3e50; margin: 3px 0; font-weight: 500; }
      .kpi-label { font-size: 12px; color: #7f8c8d; margin-top: 4px; }
      .kpi-icon  { font-size: 18px; margin-bottom: 5px; }
      .run-info  { font-size: 12px; color: #27ae60; margin-bottom: 10px; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li > a:hover {
        border-left: 4px solid #1abc9c;
      }
    "))),
    
    if (DEMO_SHOW_BANNER)
      div(style = "background:#fef9e7; border:1px solid #f39c12; border-radius:4px;
                   padding:8px 12px; margin-bottom:12px; font-size:12px; color:#856404;",
          icon("info-circle"),
          HTML(" <b>Demo version:</b> all figures, parameters and diagrams shown are
                illustrative dummy values and do not represent real screening data,
                model structure or model outputs.")),
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              uiOutput("main_content_ui")
      ),
      
      tabItem(tabName = "parameters",
              
              fluidRow(
                column(12, h2("Model Parameters", style = "margin-top:5px; margin-bottom:5px;"))
              ),
              
              fluidRow(
                column(4,
                       sliderInput("diagram_zoom", "Diagram size:",
                                   min = 40, max = 160, value = 80,
                                   step = 10, post = "%", width = "100%")
                )
              ),
              
              fluidRow(
                box(title = "Pathway Diagram", status = "primary", solidHeader = TRUE,
                    width = 12, collapsible = TRUE,
                    # Scrollable frame: the diagram is a large fixed-size image,
                    # so instead of squashing it into a fixed box we give it a
                    # scroll area and let the "Diagram size" slider above set
                    # its actual rendered width -- drag it down to fit more of
                    # the diagram on screen, or up to read fine text.
                    div(style = "width:100%; max-height:700px; overflow:auto;
                                 border:1px solid #eee;",
                        imageOutput("param_diagram", height = "auto")))
              ),
              
              # Filters sit directly above the parameter table they control.
              fluidRow(
                column(4,
                       selectInput("param_age_band", "Age band:",
                                   choices = c("All age bands",
                                               "25-29", "30-34", "35-39", "40-44",
                                               "45-49", "50-54", "55-59", "60-64"),
                                   selected = "All age bands", width = "100%")),
                column(4,
                       textInput("param_arrow_search", "Search Arrow ID:",
                                 value = "", placeholder = "e.g. B2, H4, A1",
                                 width = "100%"))
              ),
              
              fluidRow(
                box(title = tagList(icon("info-circle"),
                                    " Parameter values are read from the model engine",
                                    " (view only — not editable here)"),
                    status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("param_table"))
              )
              
      ) # end tabItem parameters
      
      
      ,
      tabItem(tabName = "vaccination",
              
              fluidRow(
                column(12, h2("Vaccination / Age Adjustments",
                              style = "margin-top:5px; margin-bottom:5px;"))
              ),
              
              fluidRow(
                box(title = tagList(icon("info-circle"), " How this works"),
                    status = "info", solidHeader = TRUE, width = 12,
                    tags$p(style = "color:#e67e22; font-weight:bold;",
                           icon("exclamation-circle"),
                           " These assumptions are used only when 'Vaccination impact'",
                           " is selected as the Pathway version in the sidebar."),
                    tags$p(
                      "Set vaccination assumptions ", tags$b("per age band and per year"),
                      " (years follow the From/To selectors in the sidebar)."
                    ),
                    tags$ol(
                      tags$li(tags$b("Vaccine effect (one value each):"), " type how much vaccination",
                              " reduces HPV positivity, low-grade and high-grade cytology for a",
                              " vaccinated person. One fixed value each, used for every age band",
                              " and year. All start at 0 (no defaults) -- until you enter them,",
                              " vaccination has no effect."),
                      tags$li(tags$b("Proportion vaccinated in the screening programme:"),
                              " type the % of women taking part in screening who are vaccinated,",
                              " for each age band and year (e.g. 90 for 25-29 in 2026). This is",
                              " not the wider population vaccination uptake."),
                      tags$li(tags$b("Per-year table:"), " pick a year; Factor is",
                              " derived as 1 - proportion vaccinated x effect (e.g. a 20% effect",
                              " with 90% vaccinated gives 1 - 0.9 x 0.2 = 0.82). You can also",
                              " overwrite any Factor directly (Source = Manual);",
                              " clear the cell to go back to the derived value.")
                    ),
                    tags$p(
                      "Cytology changes are absorbed by the negative (normal) result",
                      " so each group still sums to 1; unsatisfactory specimens are",
                      " untouched. Changes are session-only and apply on your ",
                      tags$b("next"), " simulation run."
                    )
                )
              ),
              
              fluidRow(
                box(title = "1. Vaccine effect -- reduction (%) for a vaccinated person (one value each; 0 = none)",
                    status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, numericInput("vax_eff_hpv_val", "HPV positivity (B1) reduced by (%):",
                                             value = 0, min = 0, max = 100, step = 1, width = "100%")),
                      column(4, numericInput("vax_eff_lg_val", "Low-grade / borderline cytology reduced by (%):",
                                             value = 0, min = 0, max = 100, step = 1, width = "100%")),
                      column(4, numericInput("vax_eff_hg_val", "High-grade cytology reduced by (%):",
                                             value = 0, min = 0, max = 100, step = 1, width = "100%"))))
              ),
              
              fluidRow(
                box(title = "2. Proportion vaccinated in the screening programme (% of women screened who are vaccinated, by age band and year) -- type a value in each box (0 = none)",
                    status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("vax_uptake_table"))
              ),
              
              fluidRow(
                column(3, actionButton("vax_reset", "Reset proportions, vaccine effects & manual factors",
                                       icon = icon("undo"), class = "btn-default")),
                column(3, actionButton("vax_run_full_btn",
                                       "Run full simulation with these settings",
                                       icon = icon("play"), class = "btn-success")),
                column(6, div(style = "padding-top:6px;", uiOutput("vax_status_ui")))
              ),
              
              br(),
              
              fluidRow(
                box(title = "3. Parameters for the selected year (Factor is editable)",
                    status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("vax_year", "Year:", choices = 2025:2027,
                                selected = 2025, width = "150px"),
                    DTOutput("vax_table"))
              )
              
      ) # end tabItem vaccination
      
      ,
      tabItem(tabName = "pathway_inputs",
              
              fluidRow(
                column(12, uiOutput("pathway_title"))
              ),
              
              fluidRow(
                box(title = tagList(icon("info-circle"), " How this works"),
                    status = "info", solidHeader = TRUE, width = 12,
                    tags$p("Pick the pathway version in the sidebar, enter the inputs",
                           " it needs below (years follow the From/To selectors), then",
                           " click Run simulation with these settings. Values are percentages",
                           " (e.g. 70 = 70%). Every box starts at 0, and 0 means 'not used', so",
                           " a version with all boxes at 0 behaves like the current pathway."),
                    uiOutput("pathway_note")
                )
              ),
              
              fluidRow(
                column(4,
                       sliderInput("pw_diagram_zoom", "Diagram size:",
                                   min = 40, max = 160, value = 80,
                                   step = 10, post = "%", width = "100%"))
              ),
              
              fluidRow(
                box(title = "Pathway Diagram", status = "primary", solidHeader = TRUE,
                    width = 12, collapsible = TRUE,
                    div(style = "width:100%; max-height:700px; overflow:auto;
                                 border:1px solid #eee;",
                        imageOutput("pw_diagram", height = "auto")))
              ),
              
              uiOutput("pathway_input_boxes"),
              
              # buttons sit BELOW the input boxes
              fluidRow(
                column(12,
                       actionButton("pw_reset", "Reset all inputs to 0",
                                    icon = icon("undo"), class = "btn-default"),
                       actionButton("pw_run_btn", "Run simulation with these settings",
                                    icon = icon("play"), class = "btn-success",
                                    style = "margin-left:10px;"),
                       br(), br())
              )
              
      ) # end tabItem pathway_inputs
      
    ) # end tabItems
    
  ) # end dashboardBody
) # end dashboardPage

# =============================================================================
# SERVER
# =============================================================================

# Per-band baseline attendance / HPV positivity for the sidebar panels, the
# baseline box and the Excel export (dummy values from dummy_data.R).
engine_band_baselines <- function(fin_year = "2025/26") {
  DUMMY_SIM_PARAMS[, c("age_band", "p_attend", "p_hpv_pos",
                       "n_screened", "n_invited")]
}

server <- function(input, output, session) {
  
  # ── Parameters page: diagram + read-only parameter table ─────────────────
  # Diagram follows the pathway version selected in the sidebar; the
  # "Diagram size" slider scales it from its native width inside a
  # scrollable frame. Demo: placeholder images from dummy_diagram().
  output$param_diagram <- renderImage({
    d <- dummy_diagram(selected_pathway(input$pathway_version))
    zoom_pct <- (input$diagram_zoom %||% 80) / 100
    list(src = d$path, width = round(d$width * zoom_pct))
  }, deleteFile = FALSE)
  
  # Same diagram as the Parameters tab, shown on the version-inputs page
  output$pw_diagram <- renderImage({
    d <- dummy_diagram(selected_pathway(input$pathway_version))
    zoom_pct <- (input$pw_diagram_zoom %||% 80) / 100
    list(src = d$path, width = round(d$width * zoom_pct))
  }, deleteFile = FALSE)
  
  output$param_table <- DT::renderDataTable({
    all_bands <- c("25-29", "30-34", "35-39", "40-44",
                   "45-49", "50-54", "55-59", "60-64")
    sel <- input$param_age_band %||% "All age bands"
    bands <- if (identical(sel, "All age bands")) all_bands else sel
    # one table per band, then side by side: one row per arrow, one column per band
    per_band <- lapply(all_bands, function(b)
      get_parameter_table(b, model_env = globalenv()))
    names(per_band) <- all_bands
    base <- per_band[[1]][, c("arrow_id", "description", "status")]
    for (b in bands)
      base[[b]] <- per_band[[b]]$value[match(base$arrow_id, per_band[[b]]$arrow_id)]
    # Arrow ID search (case-insensitive, matches part of the ID)
    q <- trimws(input$param_arrow_search %||% "")
    if (nzchar(q))
      base <- base[grepl(q, base$arrow_id, ignore.case = TRUE, fixed = TRUE), , drop = FALSE]
    names(base)[names(base) == "arrow_id"]    <- "Arrow ID"
    names(base)[names(base) == "description"] <- "Description"
    names(base)[names(base) == "status"]      <- "Status"
    DT::datatable(
      base,
      options  = list(pageLength = 50, dom = "tip", scrollX = TRUE),
      rownames = FALSE,
      editable = FALSE
    ) %>%
      DT::formatStyle(
        "Status", target = "row",
        backgroundColor = DT::styleEqual(
          c("PLACEHOLDER", "calibrated"),
          c("#FFF3CD", "white"))
      )
  })
  
  # ── Vaccination / Age Adjustments tab ─────────────────────────────────────
  # Session-only per-year vaccination state (see dummy_data.R
  # for the math): uptake matrix (band x year, %), manual factor overrides, and
  # effectiveness (typed by the user -- no defaults).
  vax_uptake <- reactiveVal(vax_empty_uptake(2025:2035))
  vax_manual <- reactiveVal(vax_empty_manual())
  
  vax_years <- reactive({
    y1 <- as.integer(input$year_start); y2 <- as.integer(input$year_end)
    if (is.na(y1) || is.na(y2) || y2 < y1) return(y1)
    seq(y1, y2)
  })
  
  # Vaccine effect: ONE fixed value each (HPV+, low-grade, high-grade), in %.
  # Expanded to the band x year tables the maths module expects, so every
  # age band and year uses the same value (0 / blank = not used).
  vax_eff <- reactive({
    mk <- function(val) {
      df <- vax_empty_uptake(2025:2035)
      v  <- suppressWarnings(as.numeric(val))
      if (length(v) == 1L && !is.na(v) && v > 0)
        for (cn in setdiff(names(df), "Age band")) df[[cn]] <- min(100, v)
      df
    }
    list(hpv = mk(input$vax_eff_hpv_val),
         lg  = mk(input$vax_eff_lg_val),
         hg  = mk(input$vax_eff_hg_val))
  })
  
  # Keep the year dropdown in step with the sidebar From/To range
  observe({
    yrs <- vax_years()
    sel <- isolate(input$vax_year)
    sel <- if (!is.null(sel) && as.integer(sel) %in% yrs) as.integer(sel) else yrs[1]
    updateSelectInput(session, "vax_year", choices = yrs, selected = sel)
  })
  
  # -- Number-box grids (rows = age bands, columns = years in range) ---------
  # Replaces the old double-click tables. Every cell is a number box that
  # starts at 0. 0 means "not used" and is stored as NA, so everything
  # downstream (empty = baseline) behaves exactly as before.
  matrix_inputs <- function(id, state) {
    cell_id <- function(b, y) paste0(id, "__", gsub("[^0-9A-Za-z]", "_", b), "__", y)
    output[[id]] <- renderUI({
      yrs   <- vax_years()
      cur   <- isolate(state())
      bands <- cur[["Age band"]]
      hdr <- tags$tr(tags$th(""),
                     lapply(yrs, function(y)
                       tags$th(style = "text-align:center; padding:2px 4px;", y)))
      rows <- lapply(seq_along(bands), function(i) {
        tags$tr(
          tags$td(style = "padding:2px 10px 2px 4px; font-weight:bold; white-space:nowrap;",
                  bands[i]),
          lapply(yrs, function(y) {
            v <- cur[[as.character(y)]][i]
            tags$td(style = "padding:1px 3px;",
                    numericInput(cell_id(bands[i], y), label = NULL,
                                 value = if (is.na(v)) 0 else v,
                                 min = 0, max = 100, step = 1, width = "85px"))
          }))
      })
      div(style = "overflow-x:auto;",
          tags$table(tags$thead(hdr), tags$tbody(rows)))
    })
    observe({
      yrs <- as.character(vax_years())
      cur <- isolate(state())
      new <- cur
      changed <- FALSE
      for (i in seq_len(nrow(cur))) {
        for (y in yrs) {
          if (!(y %in% names(cur))) next
          v <- input[[cell_id(cur[["Age band"]][i], y)]]
          if (is.null(v)) next                       # not rendered yet
          v  <- suppressWarnings(as.numeric(v))
          nv <- if (is.na(v) || v <= 0) NA_real_ else min(100, v)
          old <- new[[y]][i]
          if (!identical(is.na(old), is.na(nv)) ||
              (!is.na(old) && !is.na(nv) && old != nv)) {
            new[[y]][i] <- nv
            changed <- TRUE
          }
        }
      }
      if (changed) state(new)
    })
    # returns a function that copies the stored values into the boxes (empty
    # = 0). Used by the Reset buttons (state emptied first) and "Apply to all".
    function() {
      cur <- isolate(state())
      for (i in seq_len(nrow(cur)))
        for (y in 2025:2035) {
          v <- if (as.character(y) %in% names(cur)) cur[[as.character(y)]][i] else NA_real_
          updateNumericInput(session, cell_id(cur[["Age band"]][i], y),
                             value = if (is.na(v)) 0 else v)
        }
    }
  }
  
  # -- 2. Uptake grid ------------------------------------------------------
  vax_uptake_refresh <- matrix_inputs("vax_uptake_table", vax_uptake)
  
  # -- 3. Per-year parameter table (single render path: reactive re-render) --
  vax_year_grid <- reactive({
    req(input$vax_year)
    vax_build_year_grid(as.integer(input$vax_year), vax_uptake(), vax_eff(),
                        vax_manual(), model_env = globalenv())
  })
  
  output$vax_table <- DT::renderDataTable({
    g <- vax_year_grid()
    req(!is.null(g))
    DT::datatable(
      format_vax_year_for_display(g), rownames = FALSE,
      # Ordering/searching disabled: row order must map onto the grid.
      options  = list(paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t"),
      # Only Factor (column index 5) is editable.
      editable = list(target = "cell", disable = list(columns = c(0,1,2,3,4,6,7,8)))
    )
  }, server = TRUE)
  
  observeEvent(input$vax_table_cell_edit, {
    info <- input$vax_table_cell_edit
    g <- vax_year_grid()
    if (is.null(g) || info$row < 1 || info$row > nrow(g)) return()
    yr <- as.integer(input$vax_year)
    ab <- g$age_band[info$row]; pn <- g$param_name[info$row]
    v  <- suppressWarnings(as.numeric(info$value))
    
    m <- vax_manual()
    m <- m[!(m$year == yr & m$age_band == ab & m$param_name == pn), , drop = FALSE]
    if (!is.na(v) && v >= 0) {          # blank / invalid -> back to derived
      m <- rbind(m, data.frame(year = yr, age_band = ab, param_name = pn,
                               factor = v, stringsAsFactors = FALSE))
    }
    vax_manual(m)
  })
  
  observeEvent(input$vax_reset, {
    vax_uptake(vax_empty_uptake(2025:2035))
    vax_manual(vax_empty_manual())
    for (id in c("vax_eff_hpv_val", "vax_eff_lg_val", "vax_eff_hg_val"))
      updateNumericInput(session, id, value = 0)
    vax_uptake_refresh()
  })
  
  output$vax_status_ui <- renderUI({
    # Cheap count (no engine calls): year x band cells that will change.
    yrs <- vax_years(); u <- vax_uptake(); eff <- vax_eff(); m <- vax_manual()
    any_eff <- any(vapply(eff, function(d) any(!is.na(as.matrix(d[, -1, drop = FALSE]))), logical(1)))
    eff_has <- function(y, b) any(vapply(eff, function(d)
      y %in% names(d) && !is.na(d[[y]][match(b, d[["Age band"]])]), logical(1)))
    cells <- character(0)
    if (any_eff) {
      for (y in as.character(yrs)) {
        if (y %in% names(u)) {
          bands <- u[["Age band"]][!is.na(u[[y]]) & u[[y]] > 0]
          bands <- bands[vapply(bands, function(b) eff_has(y, b), logical(1))]
          if (length(bands)) cells <- c(cells, paste(y, bands))
        }
      }
    }
    if (nrow(m) > 0) {
      mm <- m[m$year %in% yrs & abs(m$factor - 1) > 1e-9, , drop = FALSE]
      cells <- c(cells, paste(mm$year, mm$age_band))
    }
    n_changed <- length(unique(cells))
    uptake_entered <- any(!is.na(as.matrix(u[, -1, drop = FALSE])))
    if (n_changed == 0) {
      msg <- if (uptake_entered && !any_eff)
        " Proportion vaccinated entered, but no vaccine effect yet -- no effect until you enter it."
      else " No adjustments active -- next run uses baseline values."
      tags$span(style = "color:#7f8c8d; font-size:13px;", icon("check-circle"), msg)
    } else {
      tags$span(style = "color:#e67e22; font-size:13px; font-weight:bold;",
                icon("exclamation-circle"),
                sprintf(" %d year x age-band adjustment(s) ready (applied when 'Vaccination impact' is selected).",
                        n_changed))
    }
  })
  
  # ── Main content UI — static layout ─────────────────────────────────────
  output$main_content_ui <- renderUI({
    
    # ── Header ───────────────────────────────────────────────────────────────
    tagList(
      
      # Scotland Overview header
      fluidRow(
        column(6, h2("Scotland Overview",
                     style = "margin-top:5px; margin-bottom:5px;")),
        column(2, div(style = "padding-top:12px;",
                      uiOutput("kpi_year_ui")
        )),
        column(2, div(style = "padding-top:12px; text-align:right;",
                      uiOutput("run_info_ui")
        )),
        column(2, div(style = "padding-top:10px; text-align:right;",
                      uiOutput("download_btn_ui")
        ))
      ),
      
      # KPI Cards — Scotland totals only
      fluidRow(
        column(3, uiOutput("kpi_screened")),
        column(3, uiOutput("kpi_hpv")),
        column(3, uiOutput("kpi_cytology")),
        column(3, uiOutput("kpi_colp_total"))
      ),
      
      fluidRow(column(12, uiOutput("ci_note_ui"))),
      
      # Trend charts
      fluidRow(
        box(title = "Screened Population & HPV Tests",
            status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("trend_screened_hpv", height = "320px"),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton("dl_trend_screened_hpv", "Download PNG",
                               icon = icon("download"), class = "btn-default btn-sm"))),
        box(title = "Cytology & Colposcopy Demand",
            status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("trend_cyto_colp", height = "320px"),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton("dl_trend_cyto_colp", "Download PNG",
                               icon = icon("download"), class = "btn-default btn-sm")))
      ),
      
      # Scotland totals table
      fluidRow(
        box(title = "Scotland Totals — All Years (Mean [95% CI])",
            status = "info", solidHeader = TRUE, width = 12,
            DTOutput("scotland_table"))
      ),
      
      # Baseline attendance info
      fluidRow(
        box(title = tagList(icon("info-circle"),
                            " Baseline Attendance Rates by Age Band"),
            status = "info", solidHeader = FALSE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
            div(style = "font-size:12px; color:#7f8c8d; margin-bottom:8px;",
                "Empirical attendance rates from SCCRS uptake data used as
                 the simulation baseline."),
            uiOutput("baseline_attend_ui"))
      ),
      
      # ── Age Band Breakdown section ────────────────────────────────────────
      fluidRow(
        column(6, h2("Age Band Breakdown",
                     style = "margin-top:15px; margin-bottom:5px;")),
        column(3, div(style = "padding-top:20px;",
                      uiOutput("age_year_ui")
        )),
        column(3, div(style = "padding-top:20px;",
                      selectInput("age_band_filter", "Select Age Band:",
                                  choices = c("All bands", "25-29", "30-34", "35-39",
                                              "40-44", "45-49", "50-54", "55-59", "60-64"),
                                  selected = "All bands", width = "100%")
        ))
      ),
      
      fluidRow(
        box(title = "Age Band Summary (Mean values)",
            status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("age_band_table"))
      ),
      
      fluidRow(
        box(title = "Screened Population by Age Band",
            status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("age_bar_screened", height = "300px"),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton("dl_age_bar_screened", "Download PNG",
                               icon = icon("download"), class = "btn-default btn-sm"))),
        box(title = "HPV Tests & Cytology by Age Band",
            status = "info", solidHeader = TRUE, width = 6,
            plotOutput("age_bar_hpv_cyto", height = "300px"),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton("dl_age_bar_hpv_cyto", "Download PNG",
                               icon = icon("download"), class = "btn-default btn-sm")))
      ),
      
      fluidRow(
        box(title = "Colposcopy Demand by Age Band",
            status = "warning", solidHeader = TRUE, width = 12,
            plotOutput("age_bar_colp", height = "300px"),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton("dl_age_bar_colp", "Download PNG",
                               icon = icon("download"), class = "btn-default btn-sm")))
      )
      
    ) # end tagList
  })
  
  # ── Age year selector ────────────────────────────────────────────────────
  output$age_year_ui <- renderUI({
    req(sim_res())
    yrs <- sort(unique(sim_res()$des_summary$year))
    selectInput("age_year", "Select Year:",
                choices = yrs, selected = min(yrs), width = "100%")
  })
  
  # ── Year × Band adjustment panels ────────────────────────────────────────
  all_bands <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
  
  output$year_band_panels_ui <- renderUI({
    yr1 <- as.integer(input$year_start)
    yr2 <- as.integer(input$year_end)
    years <- seq(yr1, yr2)
    
    panels <- lapply(years, function(yr) {
      yr_str  <- as.character(yr)
      dd_id   <- paste0("band_sel_", yr_str)   # dropdown input id
      
      tags$div(
        style = "margin-bottom:8px;",
        tags$details(
          tags$summary(
            style = "color:#ecf0f1; font-size:12px; font-weight:bold;
                       cursor:pointer; padding:4px 0;",
            sprintf("▶ Year %d", yr)
          ),
          tags$div(
            style = "padding:6px 0 0 4px;",
            # Age band dropdown
            tags$p(style="color:#95a5a6; font-size:10px; margin:4px 0 2px 0;",
                   "Select age band to adjust:"),
            selectInput(dd_id, label=NULL,
                        choices  = c("Select band..."="", all_bands),
                        selected = "",
                        width    = "100%"),
            # Sliders — shown only when a band is selected
            uiOutput(paste0("sliders_", yr_str)),
            # Summary of all changes made for this year
            uiOutput(paste0("preview_", yr_str))
          )
        )
      )
    })
    
    do.call(tagList, panels)
  })
  
  # ── Sliders — shown dynamically when band selected ────────────────────────
  observe({
    yr1 <- as.integer(input$year_start)
    yr2 <- as.integer(input$year_end)
    years <- seq(yr1, yr2)
    
    lapply(years, function(yr) {
      local({
        yr_str <- as.character(yr)
        dd_id  <- paste0("band_sel_", yr_str)
        
        output[[paste0("sliders_", yr_str)]] <- renderUI({
          ab <- input[[dd_id]]
          if (is.null(ab) || ab == "") return(NULL)
          
          att_id <- paste0("att_", yr_str, "_", gsub("-", "_", ab))
          hpv_id <- paste0("hpv_", yr_str, "_", gsub("-", "_", ab))
          
          # Load baseline for this band
          sp <- engine_band_baselines()
          base_info <- if (!is.null(sp) && ab %in% sp$age_band) {
            row <- sp[sp$age_band == ab, ]
            sprintf("Baseline: Attendance %.1f%%  |  HPV+ %.1f%%",
                    round(row$p_attend[[1]]  * 100, 1),
                    round(row$p_hpv_pos[[1]] * 100, 1))
          } else ""
          
          div(
            style = "background:#2c3e50; border-radius:4px;
                     padding:8px 10px; margin:6px 0;",
            tags$p(style="color:#3498db; font-size:10px; margin:0 0 6px 0;
                          font-weight:bold;",
                   sprintf("Adjustments for %s — %d", ab, yr)),
            tags$p(style="color:#7f8c8d; font-size:9px; margin:0 0 8px 0;",
                   base_info),
            tags$p(style="color:#95a5a6; font-size:10px; margin:0 0 2px 0;",
                   "Attendance uplift:"),
            sliderInput(att_id, label=NULL,
                        min=-25, max=50,
                        value=isolate(input[[att_id]] %||% 0),
                        step=5, post="%", width="100%"),
            tags$p(style="color:#95a5a6; font-size:10px; margin:4px 0 2px 0;",
                   "HPV positivity adjustment:"),
            sliderInput(hpv_id, label=NULL,
                        min=-25, max=50,
                        value=isolate(input[[hpv_id]] %||% 0),
                        step=5, post="%", width="100%")
          )
        })
      })
    })
  })
  
  # ── Preview — summary of ALL changes made for a year ─────────────────────
  observe({
    yr1 <- as.integer(input$year_start)
    yr2 <- as.integer(input$year_end)
    years <- seq(yr1, yr2)
    
    lapply(years, function(yr) {
      local({
        yr_str <- as.character(yr)
        output[[paste0("preview_", yr_str)]] <- renderUI({
          sp <- engine_band_baselines()
          
          rows <- lapply(all_bands, function(ab) {
            att_id  <- paste0("att_", yr_str, "_", gsub("-", "_", ab))
            hpv_id  <- paste0("hpv_", yr_str, "_", gsub("-", "_", ab))
            att_val <- as.numeric(input[[att_id]] %||% 0) / 100
            hpv_val <- as.numeric(input[[hpv_id]] %||% 0) / 100
            if (att_val == 0 && hpv_val == 0) return(NULL)
            
            sp_row <- if (!is.null(sp)) sp[sp$age_band == ab, ] else NULL
            base_att <- if (!is.null(sp_row) && nrow(sp_row)>0)
              round(sp_row$p_attend[[1]]  * 100, 1) else NA
            base_hpv <- if (!is.null(sp_row) && nrow(sp_row)>0)
              round(sp_row$p_hpv_pos[[1]] * 100, 1) else NA
            
            tags$tr(
              tags$td(style="padding:2px 8px; font-size:10px; font-weight:bold;
                             color:#ecf0f1;", ab),
              if (att_val != 0)
                tags$td(style="padding:2px 8px; font-size:10px;",
                        tags$span(style=if(att_val>0)"color:#2ecc71;"else"color:#e74c3c;",
                                  sprintf("Att: %.1f%%→%.1f%%", base_att,
                                          round(min(100,base_att*(1+att_val)),1))))
              else tags$td(),
              if (hpv_val != 0)
                tags$td(style="padding:2px 8px; font-size:10px;",
                        tags$span(style=if(hpv_val<0)"color:#2ecc71;"else"color:#e74c3c;",
                                  sprintf("HPV+: %.1f%%→%.1f%%", base_hpv,
                                          round(min(100,base_hpv*(1+hpv_val)),1))))
              else tags$td()
            )
          })
          
          non_null <- Filter(Negate(is.null), rows)
          if (length(non_null) == 0) return(NULL)
          
          div(style = "background:#1a2a3a; border:1px solid #27ae60;
                       border-radius:4px; padding:6px; margin-top:6px;",
              tags$span(style="color:#27ae60; font-size:10px; font-weight:bold;",
                        sprintf("Changes applied — Year %d:", yr)),
              tags$table(style="width:100%;", tags$tbody(non_null))
          )
        })
      })
    })
  })
  
  
  # ── Pathway Inputs tab ────────────────────────────────────────────────────
  # Three band x year % matrices, empty until the user types (no defaults).
  # One reactiveVal + editable table per input declared by ANY registered
  # pathway version (ids like pw_<version>_<input>).
  pw_state   <- list()
  pw_refresh <- list()
  
  for (pw_name in names(PATHWAYS)) {
    for (inp in PATHWAYS[[pw_name]]$inputs) {
      local({
        key <- paste(pw_name, inp$id, sep = "__")
        st  <- reactiveVal(vax_empty_uptake(2025:2035))
        pw_state[[key]]   <<- st
        pw_refresh[[key]] <<- matrix_inputs(paste0("pw_tbl_", key), st)
      })
    }
  }
  
  # "Set all cells to" -> fill every band x year cell of that table with one
  # value (0-100); the boxes stay editable afterwards. 0 clears the table.
  for (key_i in names(pw_state)) {
    local({
      key <- key_i
      observeEvent(input[[paste0("pw_fill_btn_", key)]], {
        v <- suppressWarnings(as.numeric(input[[paste0("pw_fill_val_", key)]]))
        if (length(v) != 1L || is.na(v)) return()
        v  <- min(100, max(0, v))
        df <- pw_state[[key]]()
        for (cn in setdiff(names(df), "Age band"))
          df[[cn]] <- if (v > 0) v else NA_real_
        pw_state[[key]](df)
        pw_refresh[[key]]()
      }, ignoreInit = TRUE)
    })
  }
  
  observeEvent(input$pw_reset, {
    for (key in names(pw_state)) pw_state[[key]](vax_empty_uptake(2025:2035))
    for (key in names(pw_refresh)) pw_refresh[[key]]()
  })
  
  output$pathway_title <- renderUI({
    spec <- PATHWAYS[[selected_pathway(input$pathway_version)]]
    h2(if (is.null(spec)) "Pathway version inputs" else paste0(spec$label, " -- inputs"),
       style = "margin-top:5px; margin-bottom:5px;")
  })
  
  # choosing a version opens its input page; clearing it returns to the
  # dashboard and the baseline (current pathway) is what runs
  observeEvent(input$pathway_version, {
    pw <- selected_pathway(input$pathway_version)
    tab <- if (identical(pw, "current")) "dashboard"
    else if (identical(pw, "vaccination")) "vaccination"
    else "pathway_inputs"
    updateTabItems(session, "sidebar_tabs", selected = tab)
  }, ignoreInit = TRUE)
  
  output$pathway_note <- renderUI({
    pw <- selected_pathway(input$pathway_version)
    spec <- PATHWAYS[[pw]]
    txt <- if (is.null(spec)) {
      "No pathway version selected (baseline run) -- the current pathway runs. Choose a version in the sidebar to enter its inputs."
    } else spec$description %||% spec$label
    tags$p(style = "font-weight:bold;", txt)
  })
  
  # boxes (one editable table per input) for the SELECTED pathway version
  output$pathway_input_boxes <- renderUI({
    pw <- selected_pathway(input$pathway_version)
    spec <- PATHWAYS[[pw]]
    if (is.null(spec) || length(spec$inputs) == 0) return(NULL)
    tagList(lapply(spec$inputs, function(inp) {
      fluidRow(
        box(title = paste0(spec$label, " -- ", inp$label),
            status = "primary", solidHeader = TRUE, width = 12,
            # one value applied to every cell; cells can still be edited after
            div(style = "display:flex; align-items:flex-end; gap:8px; flex-wrap:wrap; margin-bottom:8px;",
                div(numericInput(paste0("pw_fill_val_", pw, "__", inp$id),
                                 "Set all cells to (%):",
                                 value = 0, min = 0, max = 100, step = 1, width = "150px")),
                div(style = "margin-bottom:15px;",
                    actionButton(paste0("pw_fill_btn_", pw, "__", inp$id),
                                 "Apply to all cells", icon = icon("fill-drip"),
                                 class = "btn-primary"))),
            uiOutput(paste0("pw_tbl_", pw, "__", inp$id)))
      )
    }))
  })
  
  # all inputs of the selected pathway -> list(id = [[year]][[band]] fractions)
  collect_pathway_inputs <- function(pw, yrs) {
    spec <- PATHWAYS[[pw]]
    out <- list(pathway = pw)
    for (inp in spec$inputs) {
      st <- pw_state[[paste(pw, inp$id, sep = "__")]]
      if (!is.null(st)) out[[inp$id]] <- mat_to_list(st(), yrs)
    }
    out
  }
  
  # band x year % matrix -> nested list [[year]][[band]] of fractions
  mat_to_list <- function(df, yrs) {
    res <- lapply(yrs, function(y) {
      col <- as.character(y)
      v <- if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
      setNames(as.list(v / 100), df[["Age band"]])
    })
    setNames(res, as.character(yrs))
  }
  
  # ── Helper: build year × band adjustment list from inputs ─────────────────
  build_adj_list <- function(prefix, yr1, yr2) {
    years <- seq(yr1, yr2)
    result <- lapply(setNames(years, as.character(years)), function(yr) {
      yr_str <- as.character(yr)
      vals <- sapply(all_bands, function(ab) {
        id <- paste0(prefix, "_", yr_str, "_", gsub("-", "_", ab))
        as.numeric(input[[id]] %||% 0) / 100
      })
      names(vals) <- all_bands
      vals
    })
    result
  }
  
  # ── Simulation reactive ───────────────────────────────────────────────────
  sim_res    <- reactiveVal(NULL)
  run_params <- reactiveVal(NULL)
  sim_time   <- reactiveVal(NULL)
  
  # The full app runs the simulation in a background R process and polls it
  # once a second. The demo mimics that: a run "lasts" DEMO_RUN_SECONDS, the
  # app stays responsive meanwhile, and the progress bar steps through the
  # replications. Dummy results are produced when the timer ends.
  job <- reactiveValues(running = FALSE, prog = NULL, meta = NULL, t0 = NULL,
                        args = NULL)
  
  start_sim_run <- function() {
    
    yr1  <- as.integer(input$year_start)
    yr2  <- as.integer(input$year_end)
    sf   <- 0.50
    nr   <- 10
    seed <- 42
    age  <- TRUE
    
    # Build year × band adjustment lists from dynamic slider inputs
    att_list <- build_adj_list("att", yr1, yr2)
    hpv_list <- build_adj_list("hpv", yr1, yr2)
    
    # Vaccination overrides: only when "Vaccination impact" is selected as
    # the pathway version; otherwise the run is the baseline.
    vax_overrides <- if (identical(selected_pathway(input$pathway_version), "vaccination"))
      build_vax_overrides(seq(yr1, yr2), vax_uptake(), vax_eff(),
                          vax_manual(), model_env = globalenv()) else list()
    
    # Pathway version + its inputs (fractions)
    pw_inputs <- collect_pathway_inputs(selected_pathway(input$pathway_version),
                                        seq(yr1, yr2))
    pw_inputs$overrides <- vax_overrides
    
    if (isTRUE(job$running)) {
      showNotification("A simulation is already running -- please wait for it to finish.",
                       type = "warning", duration = 6)
      return()
    }
    
    job$args <- list(
      year_start            = yr1,
      year_end              = yr2,
      n_rep                 = nr,
      scale_factor          = sf,
      seed                  = seed,
      age_stratified        = age,
      attend_uplift_by_year = att_list,
      hpv_adj_by_year       = hpv_list,
      age_band_overrides    = NULL,
      pathway_inputs        = pw_inputs
    )
    job$t0   <- Sys.time()
    job$meta <- list(yr1 = yr1, yr2 = yr2, sf = sf, nr = nr,
                     att_list = att_list, hpv_list = hpv_list,
                     pathway = pw_inputs$pathway, pw_inputs = pw_inputs,
                     vax = if (identical(pw_inputs$pathway, "vaccination"))
                       list(uptake = vax_uptake(), eff = vax_eff()) else NULL)
    
    job$prog <- shiny::Progress$new(session, min = 0, max = 1)
    job$prog$set(value = 0.1, message = sprintf("Running DES %d\u2013%d...", yr1, yr2),
                 detail = sprintf("%d reps x %.0f%% population scale (background run)",
                                  nr, sf * 100))
    job$running <- TRUE
  }
  
  # The dashboard button, the Vaccination page button and the pathway-version
  # page button all start the same run. The two page buttons first switch to the
  # Dashboard so the progress bar and results are visible.
  observeEvent(input$run_sim, start_sim_run())
  observeEvent(input$vax_run_full_btn, {
    updateTabItems(session, "sidebar_tabs", selected = "dashboard")
    start_sim_run()
  }, ignoreInit = TRUE)
  observeEvent(input$pw_run_btn, {
    updateTabItems(session, "sidebar_tabs", selected = "dashboard")
    start_sim_run()
  }, ignoreInit = TRUE)
  
  # Polls the (fake) background run: updates the progress bar and collects
  # the dummy result when the timer ends.
  observe({
    if (!isTRUE(job$running)) return()
    invalidateLater(500, session)
    isolate({
      nr  <- job$meta$nr
      el  <- as.numeric(difftime(Sys.time(), job$t0, units = "secs"))
      rep <- min(nr, floor(el / DEMO_RUN_SECONDS * nr) + 1)
      if (!is.null(job$prog))
        job$prog$set(value = 0.2 + 0.7 * ((rep - 1) / nr),
                     detail = sprintf("Replication %d of %d running...", rep, nr))
      
      if (el >= DEMO_RUN_SECONDS) {
        result <- tryCatch(do.call(dummy_run_des_simulation, job$args),
                           error = function(e) {
                             showNotification(paste("Simulation error:", conditionMessage(e)),
                                              type = "error", duration = 15)
                             NULL
                           })
        
        sim_time(round(el))
        sim_res(result)
        m <- job$meta
        run_params(list(yr1 = m$yr1, yr2 = m$yr2, sf = m$sf, nr = m$nr,
                        att_list = m$att_list, hpv_list = m$hpv_list,
                        pathway = m$pathway, pw_inputs = m$pw_inputs, vax = m$vax))
        
        if (!is.null(job$prog)) { job$prog$set(value = 1, detail = "Done!"); job$prog$close() }
        job$prog    <- NULL
        job$running <- FALSE
        
        if (!is.null(result))
          showNotification(sprintf("\u2713 Done \u2014 %d years, %d reps",
                                   m$yr2 - m$yr1 + 1, m$nr),
                           type = "message", duration = 5)
      }
    })
  })
  
  # Optional: pre-load default results so they appear immediately on start-up
  if (DEMO_PRELOAD_RESULTS) {
    yr1 <- 2025L; yr2 <- 2029L
    zero_adj <- lapply(setNames(seq(yr1, yr2), seq(yr1, yr2)),
                       function(y) setNames(rep(0, length(all_bands)), all_bands))
    sim_res(dummy_run_des_simulation(year_start = yr1, year_end = yr2))
    run_params(list(yr1 = yr1, yr2 = yr2, sf = 0.50, nr = 10,
                    att_list = zero_adj, hpv_list = zero_adj,
                    pathway = "current", pw_inputs = NULL, vax = NULL))
  }
  
  
  # ── Baseline Attendance Info Box ──────────────────────────────────────────
  output$baseline_attend_ui <- renderUI({
    sp <- engine_band_baselines()
    if (is.null(sp)) {
      return(p(style = "color:#e74c3c;",
               icon("exclamation-circle"),
               " Baseline parameters could not be read from the engine."))
    }
    
    bands    <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
    
    # Show adjustments for the first selected year
    yr1    <- as.integer(input$year_start)
    yr_str <- as.character(yr1)
    
    rows <- lapply(bands, function(ab) {
      row <- sp[sp$age_band == ab, ]
      if (nrow(row) == 0) return(NULL)
      base_pct <- round(row$p_attend[[1]] * 100, 1)
      
      att_id <- paste0("att_", yr_str, "_", gsub("-", "_", ab))
      uplift <- as.numeric(input[[att_id]] %||% 0) / 100
      
      adj_pct    <- round(min(100, row$p_attend[[1]] * (1 + uplift)) * 100, 1)
      change_txt <- if (uplift != 0)
        tags$span(style = if (uplift > 0) "color:#27ae60; font-weight:bold;"
                  else "color:#e74c3c; font-weight:bold;",
                  sprintf(" → %.1f%%  (%+.1f%%)", adj_pct, adj_pct - base_pct))
      else NULL
      
      tags$tr(
        tags$td(style = "padding:4px 12px; font-weight:bold;", ab),
        tags$td(style = "padding:4px 12px;", sprintf("%.1f%%", base_pct)),
        tags$td(style = "padding:4px 12px;", change_txt)
      )
    })
    
    # Scotland overall
    overall_base <- round(weighted.mean(sp$p_attend, sp$n_screened, na.rm=TRUE) * 100, 1)
    # Check if any band was adjusted
    any_adj <- any(sapply(bands, function(ab) {
      att_id <- paste0("att_", yr_str, "_", gsub("-", "_", ab))
      as.numeric(input[[att_id]] %||% 0) != 0
    }))
    overall_chg <- if (any_adj)
      tags$span(style = "color:#7f8c8d; font-size:11px;",
                sprintf(" (see individual bands above — Year %d)", yr1))
    else NULL
    
    tags$table(
      style = "width:auto; border-collapse:collapse;",
      tags$thead(
        tags$tr(
          tags$th(style = "padding:4px 12px; border-bottom:2px solid #dee2e6;
                           text-align:left;", "Age Band"),
          tags$th(style = "padding:4px 12px; border-bottom:2px solid #dee2e6;
                           text-align:left;", "Baseline Attendance"),
          tags$th(style = "padding:4px 12px; border-bottom:2px solid #dee2e6;
                           text-align:left;",
                  sprintf("Adjustment — Year %d", yr1))
        )
      ),
      tags$tbody(rows),
      tags$tfoot(
        tags$tr(
          tags$td(style = "padding:6px 12px; border-top:2px solid #dee2e6;
                           font-weight:bold;", "Scotland (mean)"),
          tags$td(style = "padding:6px 12px; border-top:2px solid #dee2e6;
                           font-weight:bold;",
                  sprintf("%.1f%%", overall_base)),
          tags$td(style = "padding:6px 12px; border-top:2px solid #dee2e6;",
                  overall_chg)
        )
      )
    )
  })
  
  # ── CI width note ─────────────────────────────────────────────────────────
  output$ci_note_ui <- renderUI({
    req(sim_res())
    sf <- 1.0
    nr <- 10L
    if (sf < 0.05) {
      div(style = "background:#fef9e7; border:1px solid #f39c12;
                   border-radius:4px; padding:8px 12px; margin-bottom:8px;
                   font-size:12px; color:#856404;",
          icon("exclamation-triangle"),
          HTML(paste0(" <b>Wide CIs at 1% scale:</b> Cytology and Colposcopy CIs ",
                      "may be wide at 1% scale factor. For tighter CIs use ",
                      "<b>5% or 10% scale</b>.",
                      " HPV Tests CIs are tighter as they have larger patient counts."))
      )
    }
  })
  
  # ── Age Band Breakdown — from band_summary ──────────────────────────────
  band_derived <- reactive({
    req(sim_res())
    bs <- sim_res()$band_summary
    if (is.null(bs) || nrow(bs) == 0) return(NULL)
    # Rename to match expected column names in downstream reactives
    bs %>%
      dplyr::mutate(age_band = as.character(age_band)) %>%
      dplyr::rename(
        hpv_tests_mean    = hpv_tests_mean,
        cyto_mean         = cyto_mean,
        colp_mean         = colp_mean,
        colp_routine_mean = colp_routine_mean,
        colp_nr_mean      = colp_nr_mean,
        treated_mean      = treated_mean
      )
  })
  
  
  # ── KPI year selector ────────────────────────────────────────────────────
  output$kpi_year_ui <- renderUI({
    req(sim_res())
    yrs <- sort(unique(sim_res()$des_summary$year))
    selectInput("kpi_year", "KPI Year:",
                choices  = yrs,
                selected = min(yrs),
                width    = "100%")
  })
  
  # Update age_year choices when simulation completes
  observe({
    req(sim_res())
    yrs <- sort(unique(sim_res()$des_summary$year))
    updateSelectInput(session, "age_year",
                      choices  = yrs,
                      selected = min(yrs))
  })
  
  # ── Status ────────────────────────────────────────────────────────────────
  # ── CI width indicator ───────────────────────────────────────────────────
  output$ci_width_ui <- renderUI({
    sf  <- 0.50
    nr  <- 10L
    # Approximate CI width based on scale and reps
    # CI width ∝ 1/sqrt(n_sim) where n_sim = n_per_band × n_rep
    n_sim <- round(5000 * sf * nr)  # rough patients simulated total
    ci_pct <- round(100 / sqrt(n_sim / 100), 1)
    col <- if (ci_pct < 5) "#27ae60" else if (ci_pct < 10) "#e67e22" else "#e74c3c"
    tags$div(
      tags$span(style="color:#bdc3c7; font-size:11px;", "Approx CI width: "),
      tags$span(style=paste0("color:", col, "; font-size:12px; font-weight:bold;"),
                paste0("±", ci_pct, "%")),
      tags$span(style="color:#7f8c8d; font-size:10px;",
                if (ci_pct < 5) " ✓ tight"
                else if (ci_pct < 10) " acceptable"
                else " wide — increase scale factor")
    )
  })
  
  output$parallel_status_ui <- renderUI({
    if (DEMO_PARALLEL_WORKERS > 1) {
      tags$span(
        style = "font-size:11px;",
        tags$span(style = "color:#27ae60;", icon("bolt")),
        tags$span(style = "color:#27ae60; font-weight:bold;",
                  sprintf(" Parallel: %d workers", DEMO_PARALLEL_WORKERS))
      )
    } else {
      tags$span(
        style = "font-size:11px;",
        tags$span(style = "color:#e67e22;", icon("exclamation-triangle")),
        tags$span(style = "color:#e67e22;",
                  " Sequential mode — slower"),
        tags$br(),
        tags$span(style = "color:#7f8c8d; font-size:10px;",
                  "Run: install.packages('future.apply')")
      )
    }
  })
  
  output$sim_status_ui <- renderUI({
    if (is.null(sim_res())) {
      tags$span(style="color:#e67e22; font-size:12px;",
                icon("circle-notch"), " Not yet run")
    } else {
      tags$span(style="color:#27ae60; font-size:12px;",
                icon("check-circle"), " Results ready")
    }
  })
  
  output$run_info_ui <- renderUI({
    p <- run_params()
    if (is.null(p)) return(NULL)
    mode <- "Age-Stratified (8 bands)"
    # Check if any adjustments were made
    any_att <- !is.null(p$att_list) &&
      any(unlist(p$att_list) != 0)
    any_hpv <- !is.null(p$hpv_list) &&
      any(unlist(p$hpv_list) != 0)
    adj_txt <- if (any_att || any_hpv)
      " | Scenario adjustments applied" else ""
    tags$span(class = "run-info",
              icon("check-circle"),
              sprintf(" %s | %d–%d%s", mode, p$yr1, p$yr2, adj_txt))
  })
  
  # ── KPI Cards — always Scotland totals ───────────────────────────────────
  first_yr_summary <- reactive({
    req(sim_res())
    yr <- if (!is.null(input$kpi_year)) as.integer(input$kpi_year)
    else min(sim_res()$des_summary$year)
    sim_res()$des_summary %>% dplyr::filter(year == yr)
  })
  
  make_kpi_card <- function(val, lo, hi, label, icon_name, border_col, show_ci = TRUE) {
    div(class = "kpi-card",
        style = paste0("border-left-color:", border_col, ";"),
        div(class = "kpi-icon", style = paste0("color:", border_col),
            icon(icon_name)),
        div(class = "kpi-value", format(round(val), big.mark=",")),
        if (show_ci) div(class = "kpi-ci",
                         sprintf("[%s – %s]",
                                 format(round(lo), big.mark=","),
                                 format(round(hi), big.mark=","))) else NULL,
        div(class = "kpi-label", label)
    )
  }
  
  output$kpi_screened <- renderUI({
    s  <- first_yr_summary()
    yr <- if (!is.null(input$kpi_year)) input$kpi_year
    else min(sim_res()$des_summary$year)
    make_kpi_card(s$screened_mean, s$screened_mean, s$screened_mean,
                  paste("Screened", yr), "users", "#3498db", show_ci = FALSE)
  })
  output$kpi_hpv        <- renderUI({
    s <- first_yr_summary()
    make_kpi_card(s$hpv_tests_mean, s$hpv_tests_lo, s$hpv_tests_hi,
                  "HPV Tests", "vial", "#9b59b6")
  })
  output$kpi_cytology   <- renderUI({
    s <- first_yr_summary()
    make_kpi_card(s$cyto_mean, s$cyto_lo, s$cyto_hi,
                  "Cytology", "microscope", "#1abc9c")
  })
  output$kpi_colp_total <- renderUI({
    s <- first_yr_summary()
    make_kpi_card(s$colp_mean, s$colp_lo, s$colp_hi,
                  "Colposcopy (total)", "hospital", "#e67e22")
  })
  
  # ── ggplot2 theme (same style as original) ────────────────────────────────
  base_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(colour = "grey50", size = 11),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title.x     = element_text(size = 11),
      axis.title.y     = element_text(size = 11)
    )
  
  
  # ── Trend Charts ──────────────────────────────────────────────────────────
  base_theme_bold <- theme_minimal(base_size = 13) +
    theme(
      plot.subtitle    = element_text(colour = "grey40", size = 12),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 12, face = "bold"),
      axis.title       = element_text(size = 12),
      axis.text        = element_text(size = 11, face = "bold")
    )
  
  output$trend_screened_hpv <- renderPlot({ plot_trend_screened_hpv() })
  output$trend_cyto_colp    <- renderPlot({ plot_trend_cyto_colp() })
  
  # ── Scotland Summary Table ────────────────────────────────────────────────
  output$scotland_table <- renderDT({
    req(sim_res())
    d <- sim_res()$des_summary %>%
      transmute(
        Year          = year,
        Screened      = format(screened_mean, big.mark=","),
        `HPV Tests`   = sprintf("%s [%s–%s]",
                                format(hpv_tests_mean, big.mark=","),
                                format(hpv_tests_lo,   big.mark=","),
                                format(hpv_tests_hi,   big.mark=",")),
        Cytology      = sprintf("%s [%s–%s]",
                                format(cyto_mean, big.mark=","),
                                format(cyto_lo,   big.mark=","),
                                format(cyto_hi,   big.mark=",")),
        `Colposcopy (Total)`      = sprintf("%s [%s–%s]",
                                            format(colp_mean, big.mark=","),
                                            format(colp_lo,   big.mark=","),
                                            format(colp_hi,   big.mark=",")),
        `Colposcopy (New Entrant)` = sprintf("%s [%s–%s]",
                                             format(colp_routine_mean, big.mark=","),
                                             format(colp_routine_lo,   big.mark=","),
                                             format(colp_routine_hi,   big.mark=",")),
        `Colposcopy (Existing)`   = sprintf("%s [%s–%s]",
                                            format(colp_nr_mean, big.mark=","),
                                            format(colp_nr_lo,   big.mark=","),
                                            format(colp_nr_hi,   big.mark=","))
      )
    datatable(d,
              options = list(dom="t", pageLength=10, ordering=FALSE),
              rownames = FALSE,
              class = "table table-striped table-hover table-condensed"
    )
  })
  
  # ── Age Band Table ────────────────────────────────────────────────────────
  output$age_band_table <- renderDT({
    req(sim_res())
    bs <- band_derived()
    req(!is.null(bs))
    ab <- input$age_band_filter
    d <- bs %>%
      filter(as.integer(year) == as.integer(input$age_year %||% min(year))) %>%
      { if (!is.null(ab) && ab != "All bands")
        filter(., age_band == ab)
        else . } %>%
      transmute(
        `Age Band`              = age_band,
        Screened                = format(screened_mean,     big.mark=","),
        `HPV Tests`             = format(hpv_tests_mean,    big.mark=","),
        Cytology                = format(cyto_mean,         big.mark=","),
        `Colposcopy (Total)`    = format(colp_mean,         big.mark=","),
        `Colposcopy (New Entrant)` = format(colp_routine_mean, big.mark=","),
        `Colposcopy (Existing)` = format(colp_nr_mean,      big.mark=",")
      )
    datatable(d,
              options=list(dom="t", pageLength=10, ordering=FALSE),
              rownames=FALSE,
              class="table table-striped table-hover table-condensed"
    )
  })
  
  # ── Age Band Charts ───────────────────────────────────────────────────────
  get_band_data <- reactive({
    req(sim_res())
    bs <- band_derived()
    req(!is.null(bs))
    ab <- input$age_band_filter
    d <- bs %>% filter(as.integer(year) == as.integer(input$age_year %||% min(year)))
    if (!is.null(ab) && ab != "All bands") {
      d <- d %>% filter(age_band == ab)
    }
    d
  })
  
  output$age_bar_screened  <- renderPlot({ plot_age_bar_screened() })
  output$age_bar_colp      <- renderPlot({ plot_age_bar_colp() })
  output$age_bar_hpv_cyto  <- renderPlot({ plot_age_bar_hpv_cyto() })
  
  output$age_pct_chart <- renderPlot({
    d <- get_band_data() %>%
      mutate(
        `Screened`    = screened_mean   / sum(screened_mean)   * 100,
        `Cytology`    = cyto_mean       / sum(cyto_mean)       * 100,
        `Colposcopy`  = colp_mean       / sum(colp_mean)       * 100
      ) %>%
      pivot_longer(c(`Screened`,`Cytology`,`Colposcopy`),
                   names_to="kpi", values_to="pct")
    ggplot(d, aes(x=age_band, y=pct, colour=kpi, group=kpi)) +
      geom_line(linewidth=1.1) +
      geom_point(size=3) +
      scale_colour_manual(values=c("Screened"="#3498db",
                                   "Cytology"="#1abc9c",
                                   "Colposcopy"="#e67e22")) +
      scale_y_continuous(labels=function(x) paste0(round(x,1),"%")) +
      labs(x="Age Band", y="% of Scotland total", colour=NULL,
           subtitle=paste("% contribution —", input$age_year)) +
      base_theme
  })
  
  output$colp_split_chart <- renderPlot({
    d <- get_band_data() %>%
      mutate(
        `New referrals`   = colp_routine_mean / colp_mean * 100,
        `NR surveillance` = colp_nr_mean      / colp_mean * 100
      ) %>%
      pivot_longer(c(`New referrals`,`NR surveillance`),
                   names_to="type", values_to="pct")
    ggplot(d, aes(x=age_band, y=pct, fill=type)) +
      geom_col(position="stack", width=0.7) +
      scale_fill_manual(values=c("New referrals"="#e74c3c",
                                 "NR surveillance"="#f39c12")) +
      scale_y_continuous(labels=function(x) paste0(round(x),"%")) +
      labs(x="Age Band", y="% of band colposcopy", fill=NULL,
           subtitle=paste("Colposcopy composition —", input$age_year)) +
      base_theme
  })
  
  # ── PNG Download Handlers ─────────────────────────────────────────────────
  # Helper function to make a plot download handler
  make_plot_dl <- function(plot_fn, filename_base, width = 12, height = 6) {
    downloadHandler(
      filename = function() sprintf("%s_%s.png", filename_base,
                                    format(Sys.Date(), "%Y%m%d")),
      content  = function(file) {
        ggplot2::ggsave(file, plot = plot_fn(),
                        width = width, height = height,
                        dpi = 150, bg = "white")
      }
    )
  }
  
  # Build each plot as a reactive so it can be reused for download
  plot_trend_screened_hpv <- reactive({
    req(sim_res())
    d <- sim_res()$des_summary
    d_long <- bind_rows(
      d %>% transmute(year, kpi="Screened",   mean=screened_mean,  lo=screened_mean,  hi=screened_mean),
      d %>% transmute(year, kpi="HPV Tests",  mean=hpv_tests_mean, lo=hpv_tests_lo,   hi=hpv_tests_hi)
    )
    ggplot(d_long, aes(x=year, colour=kpi, fill=kpi)) +
      geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, colour=NA) +
      geom_line(aes(y=mean), linewidth=1.5, na.rm=TRUE) +
      geom_point(aes(y=mean), size=4, shape=21, fill="white", stroke=2.5) +
      geom_label(aes(y=mean, label=format(round(mean), big.mark=",")),
                 vjust=-1.0, size=4.2, fill="white", linewidth=0.4, fontface="bold",
                 label.padding=unit(0.25,"lines"), show.legend=FALSE) +
      scale_colour_manual(values=c("Screened"="#2980b9","HPV Tests"="#8e44ad")) +
      scale_fill_manual(values=c("Screened"="#2980b9","HPV Tests"="#8e44ad")) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0.15,0.30))) +
      scale_x_continuous(breaks=unique(d_long$year)) +
      labs(x="Year", y="Patients",
           subtitle="Shaded area = 95% CI  |  Screened has no CI (deterministic)") +
      base_theme_bold
  })
  
  plot_trend_cyto_colp <- reactive({
    req(sim_res())
    d <- sim_res()$des_summary
    d_long <- bind_rows(
      d %>% transmute(year, kpi="Cytology",   mean=cyto_mean,  lo=cyto_lo,  hi=cyto_hi),
      d %>% transmute(year, kpi="Colposcopy", mean=colp_mean,  lo=colp_lo,  hi=colp_hi)
    )
    ggplot(d_long, aes(x=year, colour=kpi, fill=kpi)) +
      geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, colour=NA) +
      geom_line(aes(y=mean), linewidth=1.5, na.rm=TRUE) +
      geom_point(aes(y=mean), size=4, shape=21, fill="white", stroke=2.5) +
      geom_label(aes(y=mean, label=format(round(mean), big.mark=",")),
                 vjust=-1.0, size=4.2, fill="white", linewidth=0.4, fontface="bold",
                 label.padding=unit(0.25,"lines"), show.legend=FALSE) +
      scale_colour_manual(values=c("Cytology"="#16a085","Colposcopy"="#d35400")) +
      scale_fill_manual(values=c("Cytology"="#16a085","Colposcopy"="#d35400")) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0.15,0.30))) +
      scale_x_continuous(breaks=unique(d_long$year)) +
      labs(x="Year", y="Patients", subtitle="Shaded area = 95% confidence interval") +
      base_theme_bold
  })
  
  output$dl_trend_screened_hpv <- make_plot_dl(plot_trend_screened_hpv,
                                               "Screened_HPV_Trend")
  output$dl_trend_cyto_colp    <- make_plot_dl(plot_trend_cyto_colp,
                                               "Cytology_Colposcopy_Trend")
  
  # Age band bar chart reactives for download
  plot_age_bar_screened <- reactive({
    d <- get_band_data()
    ggplot(d, aes(x=age_band, y=screened_mean, fill=age_band)) +
      geom_col(width=0.7, show.legend=FALSE) +
      geom_text(aes(label=format(screened_mean, big.mark=",")),
                vjust=-0.4, size=3.5, fontface="bold") +
      scale_fill_manual(values=c("25-29"="#1a5276","30-34"="#1f618d",
                                 "35-39"="#2874a6","40-44"="#2e86c1",
                                 "45-49"="#3498db","50-54"="#5dade2",
                                 "55-59"="#85c1e9","60-64"="#aed6f1")) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0,.15))) +
      labs(x="Age Band", y="Patients",
           subtitle=paste("Screened —", input$age_year)) +
      base_theme_bold
  })
  
  plot_age_bar_hpv_cyto <- reactive({
    d <- get_band_data() %>%
      pivot_longer(c(hpv_tests_mean, cyto_mean), names_to="type", values_to="n") %>%
      mutate(label=ifelse(type=="hpv_tests_mean","HPV Tests","Cytology"))
    ggplot(d, aes(x=age_band, y=n, fill=label)) +
      geom_col(position="dodge", width=0.7) +
      geom_text(aes(label=format(n, big.mark=",")),
                position=position_dodge(width=0.7), vjust=-0.4, size=3, fontface="bold") +
      scale_fill_manual(values=c("HPV Tests"="#8e44ad","Cytology"="#16a085")) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0,.15))) +
      labs(x="Age Band", y="Patients", fill=NULL,
           subtitle=paste("HPV Tests & Cytology —", input$age_year)) +
      base_theme_bold
  })
  
  plot_age_bar_colp <- reactive({
    d <- get_band_data() %>%
      pivot_longer(c(colp_routine_mean, colp_nr_mean), names_to="type", values_to="n") %>%
      mutate(label=ifelse(type=="colp_routine_mean","Colposcopy New Entrant","Colposcopy Existing"))
    totals <- d %>% group_by(age_band) %>% summarise(total=sum(n), .groups="drop")
    ggplot(d, aes(x=age_band, y=n, fill=label)) +
      geom_col(position="stack", width=0.7) +
      geom_text(aes(label=format(n, big.mark=",")),
                position=position_stack(vjust=0.5),
                size=2.8, colour="white", fontface="bold") +
      geom_text(data=totals, aes(x=age_band, y=total, label=format(total, big.mark=",")),
                inherit.aes=FALSE, vjust=-0.4, size=3.2, fontface="bold", colour="#2c3e50") +
      scale_fill_manual(values=c("Colposcopy New Entrant"="#c0392b",
                                 "Colposcopy Existing"="#e67e22")) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0,.15))) +
      labs(x="Age Band", y="Patients", fill=NULL,
           subtitle=paste("Colposcopy —", input$age_year)) +
      base_theme_bold
  })
  
  output$dl_age_bar_screened  <- make_plot_dl(plot_age_bar_screened,
                                              "Screened_by_AgeBand")
  output$dl_age_bar_hpv_cyto  <- make_plot_dl(plot_age_bar_hpv_cyto,
                                              "HPV_Cytology_by_AgeBand")
  output$dl_age_bar_colp      <- make_plot_dl(plot_age_bar_colp,
                                              "Colposcopy_by_AgeBand")
  
  # ── Preview Table (Download tab) ──────────────────────────────────────────
  output$preview_table <- renderDT({
    req(sim_res())
    d <- sim_res()$des_summary %>%
      transmute(
        Year        = year,
        Screened    = format(screened_mean,  big.mark=","),
        `HPV Tests` = format(hpv_tests_mean, big.mark=","),
        Cytology    = format(cyto_mean,      big.mark=","),
        `Colp total`= format(colp_mean,      big.mark=","),
        `Colp new`  = format(colp_routine_mean, big.mark=","),
        `Colp NR`   = format(colp_nr_mean,   big.mark=",")
      )
    datatable(d, options=list(dom="t",ordering=FALSE), rownames=FALSE,
              class="table table-condensed table-striped")
  })
  
  # ── Download button (only when results available) ─────────────────────────
  output$download_btn_ui <- renderUI({
    if (is.null(sim_res())) {
      p(style="color:#e67e22;",
        icon("exclamation-circle"),
        " Run the simulation first to enable download.")
    } else {
      downloadButton("download_excel",
                     label = "Download Excel (.xlsx)",
                     class = "btn-success btn-lg",
                     icon  = icon("file-excel"))
    }
  })
  
  # ── Excel Download ────────────────────────────────────────────────────────
  output$download_excel <- downloadHandler(
    filename = function() {
      sprintf("CervicalScreeningDES_%s_%d-%d.xlsx",
              format(Sys.Date(), "%Y%m%d"),
              as.integer(input$year_start), as.integer(input$year_end))
    },
    content = function(file) {
      req(sim_res())
      p <- run_params()
      if (is.null(p)) p <- list(yr1 = as.integer(input$year_start),
                                yr2 = as.integer(input$year_end),
                                nr = NA, sf = NA, att_list = NULL, hpv_list = NULL)
      wb <- openxlsx::createWorkbook()
      
      header_style <- openxlsx::createStyle(
        fontColour     = "#FFFFFF",
        bgFill         = "#2c3e50",
        textDecoration = "bold",
        halign         = "center",
        border         = "Bottom",
        borderColour   = "#1abc9c"
      )
      num_style <- openxlsx::createStyle(numFmt = "#,##0")
      pct_style <- openxlsx::createStyle(numFmt = "0.0%")
      
      # ── Sheet 1: Scotland Totals ─────────────────────────────────────────
      openxlsx::addWorksheet(wb, "Scotland Totals")
      s1 <- sim_res()$des_summary %>%
        transmute(
          Year                              = year,
          Screened                          = screened_mean,
          `HPV Tests (Mean)`                = hpv_tests_mean,
          `HPV Tests (95% CI Lower)`        = hpv_tests_lo,
          `HPV Tests (95% CI Upper)`        = hpv_tests_hi,
          `Cytology (Mean)`                 = cyto_mean,
          `Cytology (95% CI Lower)`         = cyto_lo,
          `Cytology (95% CI Upper)`         = cyto_hi,
          `Colposcopy Total (Mean)`         = colp_mean,
          `Colposcopy Total (95% CI Lower)` = colp_lo,
          `Colposcopy Total (95% CI Upper)` = colp_hi,
          `Colposcopy New Entrant (Mean)`         = colp_routine_mean,
          `Colposcopy New Entrant (95% CI Lower)` = colp_routine_lo,
          `Colposcopy New Entrant (95% CI Upper)` = colp_routine_hi,
          `Colposcopy Existing (Mean)`         = colp_nr_mean,
          `Colposcopy Existing (95% CI Lower)` = colp_nr_lo,
          `Colposcopy Existing (95% CI Upper)` = colp_nr_hi
        )
      openxlsx::writeData(wb, "Scotland Totals", s1)
      openxlsx::addStyle(wb, "Scotland Totals", header_style,
                         rows=1, cols=1:ncol(s1), gridExpand=TRUE)
      openxlsx::addStyle(wb, "Scotland Totals", num_style,
                         rows=2:(nrow(s1)+1), cols=2:ncol(s1),
                         gridExpand=TRUE)
      openxlsx::setColWidths(wb, "Scotland Totals",
                             cols=1:ncol(s1), widths=18)
      
      # ── Sheet 2: Age Band Breakdown ──────────────────────────────────────
      openxlsx::addWorksheet(wb, "Age Band Breakdown")
      if (!is.null(band_derived())) {
        s2 <- band_derived() %>%
          transmute(
            Year                      = year,
            `Age Band`                = age_band,
            Screened                  = screened_mean,
            `HPV Tests`               = hpv_tests_mean,
            Cytology                  = cyto_mean,
            `Colposcopy (Total)`      = colp_mean,
            `Colposcopy (New Entrant)`= colp_routine_mean,
            `Colposcopy (Existing)`   = colp_nr_mean
          )
        openxlsx::writeData(wb, "Age Band Breakdown", s2)
        openxlsx::addStyle(wb, "Age Band Breakdown", header_style,
                           rows=1, cols=1:ncol(s2), gridExpand=TRUE)
        openxlsx::addStyle(wb, "Age Band Breakdown", num_style,
                           rows=2:(nrow(s2)+1), cols=3:ncol(s2),
                           gridExpand=TRUE)
        openxlsx::setColWidths(wb, "Age Band Breakdown",
                               cols=1:ncol(s2), widths=16)
      } else {
        openxlsx::writeData(wb, "Age Band Breakdown",
                            data.frame(Note="Age band breakdown derived from Scotland totals."))
      }
      
      # ── Sheet 3: Simulation Settings ─────────────────────────────────────
      openxlsx::addWorksheet(wb, "Simulation Settings")
      
      sp <- engine_band_baselines(vax_fin_year(p$yr1))
      if (is.null(sp)) sp <- data.frame(age_band = character(0), p_attend = numeric(0),
                                        p_hpv_pos = numeric(0), n_screened = numeric(0))
      sp$w <- sp$n_screened
      wm <- function(x, w) {
        ok <- !is.na(x) & !is.na(w)
        if (!any(ok)) NA_real_ else weighted.mean(x[ok], w[ok])
      }
      baseline_attend <- round(wm(sp$p_attend,  sp$w) * 100, 1)
      baseline_hpv    <- round(wm(sp$p_hpv_pos, sp$w) * 100, 1)
      
      elapsed <- sim_time()
      runtime_str <- if (!is.null(elapsed)) {
        if (elapsed < 60) sprintf("%d seconds", elapsed)
        else sprintf("%d min %d sec", elapsed %/% 60, elapsed %% 60)
      } else "Not recorded"
      
      # Build year × band adjustment table
      all_bands_xl <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
      years_xl     <- seq(p$yr1, p$yr2)
      
      # Simulation Parameters section
      s3_params <- data.frame(
        Setting = c("── SIMULATION PARAMETERS ──", "Data Source", "Mode", "Pathway Version", "Year Start", "Year End",
                    "Replications", "Scale Factor", "Random Seed",
                    "Simulation Runtime", "Run Date & Time"),
        Value   = c("", "DEMO — illustrative dummy values only",
                    "Age-Stratified (8 bands × simmer)",
                    PATHWAYS[[p$pathway %||% "current"]]$label %||%
                      "Current pathway (Version A)",
                    as.character(p$yr1), as.character(p$yr2),
                    as.character(p$nr), sprintf("%.0f%%", p$sf * 100),
                    "42 (fixed)", runtime_str,
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        stringsAsFactors = FALSE
      )
      
      # Baseline rates section
      s3_baseline <- data.frame(
        Setting = c("", "── BASELINE RATES (Scotland weighted mean) ──",
                    "Baseline Attendance Rate", "Baseline HPV Positivity Rate"),
        Value   = c("", "",
                    if (is.na(baseline_attend)) "n/a" else sprintf("%.1f%%", baseline_attend),
                    if (is.na(baseline_hpv)) "n/a" else sprintf("%.1f%%", baseline_hpv)),
        stringsAsFactors = FALSE
      )
      
      # Year × Band attendance adjustments
      att_rows <- do.call(rbind, lapply(years_xl, function(yr) {
        yr_str <- as.character(yr)
        do.call(rbind, lapply(all_bands_xl, function(ab) {
          val <- if (!is.null(p$att_list[[yr_str]])) p$att_list[[yr_str]][[ab]] else 0
          if (is.null(val) || is.na(val)) val <- 0
          base_att <- {
            row <- sp[sp$age_band == ab, ]
            if (nrow(row) > 0) round(row$p_attend[[1]] * 100, 1) else NA
          }
          data.frame(
            Setting = sprintf("  Attendance | Year %d | %s", yr, ab),
            Value   = if (val != 0)
              sprintf("%+.0f%% (relative) → %.1f%%",
                      val * 100, if (is.na(base_att)) NA_real_ else min(100, base_att * (1 + val)))
            else "Baseline (no change)",
            stringsAsFactors = FALSE
          )
        }))
      }))
      
      # Year × Band HPV adjustments
      hpv_rows <- do.call(rbind, lapply(years_xl, function(yr) {
        yr_str <- as.character(yr)
        do.call(rbind, lapply(all_bands_xl, function(ab) {
          val <- if (!is.null(p$hpv_list[[yr_str]])) p$hpv_list[[yr_str]][[ab]] else 0
          if (is.null(val) || is.na(val)) val <- 0
          base_hpv <- {
            row <- sp[sp$age_band == ab, ]
            if (nrow(row) > 0) round(row$p_hpv_pos[[1]] * 100, 1) else NA
          }
          data.frame(
            Setting = sprintf("  HPV+ Rate  | Year %d | %s", yr, ab),
            Value   = if (val != 0)
              sprintf("%+.0f%% (relative) → %.1f%%",
                      val * 100, if (is.na(base_hpv)) NA_real_ else min(100, base_hpv * (1 + val)))
            else "Baseline (no change)",
            stringsAsFactors = FALSE
          )
        }))
      }))
      
      s3_adj_header <- data.frame(
        Setting = c("", "── SCENARIO ADJUSTMENTS (sidebar sliders only; vaccination is listed under PATHWAY VERSION INPUTS) ──"),
        Value   = c("", ""),
        stringsAsFactors = FALSE
      )
      
      # Pathway-version inputs actually entered (non-empty cells only)
      pw_rows <- data.frame(Setting = character(0), Value = character(0),
                            stringsAsFactors = FALSE)
      if (!is.null(p$pw_inputs) && !identical(p$pathway, "current")) {
        add_pw <- function(label, lst) {
          out <- list()
          for (yr in names(lst)) for (ab in names(lst[[yr]])) {
            v <- lst[[yr]][[ab]]
            if (!is.null(v) && !is.na(v))
              out[[length(out) + 1]] <- data.frame(
                Setting = sprintf("  %s | Year %s | %s", label, yr, ab),
                Value = sprintf("%.1f%%", v * 100), stringsAsFactors = FALSE)
          }
          if (length(out)) do.call(rbind, out) else NULL
        }
        pw_rows <- rbind(
          data.frame(Setting = c("", "── PATHWAY VERSION INPUTS ──"), Value = c("", ""),
                     stringsAsFactors = FALSE),
          do.call(rbind, lapply(PATHWAYS[[p$pathway]]$inputs, function(inp)
            add_pw(inp$label, p$pw_inputs[[inp$id]]))))
        
        # Vaccination impact: record the uptake and effectiveness tables used
        # (non-empty cells, years in the run's range).
        if (identical(p$pathway, "vaccination") && !is.null(p$vax)) {
          vax_rows <- function(label, df) {
            out <- list()
            for (y in as.character(seq(p$yr1, p$yr2))) {
              if (!y %in% names(df)) next
              for (i in seq_len(nrow(df))) {
                v <- df[[y]][i]
                if (!is.na(v))
                  out[[length(out) + 1]] <- data.frame(
                    Setting = sprintf("  %s | Year %s | %s", label, y, df[["Age band"]][i]),
                    Value = sprintf("%.1f%%", v), stringsAsFactors = FALSE)
              }
            }
            if (length(out)) do.call(rbind, out) else NULL
          }
          # one fixed effect value -> a single settings row
          eff_row <- function(label, df) {
            v <- suppressWarnings(as.numeric(unlist(df[, -1, drop = FALSE])))
            v <- v[!is.na(v)]
            data.frame(Setting = paste0("  ", label),
                       Value = if (length(v)) sprintf("%.1f%%", v[1]) else "0.0% (not used)",
                       stringsAsFactors = FALSE)
          }
          # Resulting HPV+ rate per year and age band (baseline -> after vaccination)
          vax_effect_rows <- tryCatch({
            out <- list()
            for (yr in seq(p$yr1, p$yr2)) {
              g <- vax_build_year_grid(yr, p$vax$uptake, p$vax$eff, isolate(vax_manual()),
                                       model_env = globalenv())
              if (is.null(g)) next
              g <- g[g$param_name == "p_hpv_pos" & abs(g$factor - 1) > 1e-9, , drop = FALSE]
              for (i in seq_len(nrow(g)))
                out[[length(out) + 1]] <- data.frame(
                  Setting = sprintf("  HPV+ rate after vaccination | Year %d | %s", yr, g$age_band[i]),
                  Value = sprintf("%.2f%% -> %.2f%% (factor %.3f)",
                                  g$baseline[i] * 100, g$adjusted[i] * 100, g$factor[i]),
                  stringsAsFactors = FALSE)
            }
            if (length(out)) do.call(rbind, out) else NULL
          }, error = function(e) NULL)
          pw_rows <- rbind(pw_rows,
                           vax_rows("Proportion vaccinated in screening", p$vax$uptake),
                           eff_row("Vaccine effect: HPV positivity (B1)", p$vax$eff$hpv),
                           eff_row("Vaccine effect: low-grade cytology", p$vax$eff$lg),
                           eff_row("Vaccine effect: high-grade cytology", p$vax$eff$hg),
                           vax_effect_rows)
        }
      }
      
      s3 <- rbind(s3_params, s3_baseline, s3_adj_header, att_rows,
                  data.frame(Setting="", Value="", stringsAsFactors=FALSE),
                  hpv_rows, pw_rows)
      
      openxlsx::writeData(wb, "Simulation Settings", s3)
      
      section_style <- openxlsx::createStyle(
        fontColour = "#FFFFFF", bgFill = "#2c3e50",
        textDecoration = "bold", halign = "left"
      )
      header_rows <- which(grepl("──", s3$Setting))
      for (r in header_rows) {
        openxlsx::addStyle(wb, "Simulation Settings", section_style,
                           rows = r + 1, cols = 1:2, gridExpand = TRUE)
      }
      openxlsx::addStyle(wb, "Simulation Settings", header_style,
                         rows = 1, cols = 1:2, gridExpand = TRUE)
      openxlsx::setColWidths(wb, "Simulation Settings",
                             cols = 1:2, widths = c(45, 40))
      
      openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

# =============================================================================
shinyApp(ui=ui, server=server)
shinyApp(ui=ui, server=server)
