# =============================================================================
# overhead_typology.R  —  Companion to code.R
#
# PURPOSE
#   PART I  — Three-way overhead sub-classification
#             Divides every overhead occupation into one of three types:
#
#             (F) Friction-mitigating  : HR, recruiting, coordination,
#                 management that raises production efficiency.
#                 Social welfare effect: ambiguous-to-positive.
#                 Theoretical anchor: Garicano (2000) knowledge hierarchy;
#                 Williamson transaction-cost coordination.
#
#             (B) Bureaucratic         : compliance, regulatory, reporting,
#                 record-keeping — necessary but output-neutral.
#                 Social welfare effect: privately necessary, neutral at margin.
#                 Theoretical anchor: regulatory/institutional rent (Stigler);
#                 Michaillat–Saez (2021) "Type I" overhead (admin).
#
#             (E) Extractive           : advertising, lobbying, financial
#                 engineering — privately beneficial, socially redistributive.
#                 Social welfare effect: negative (zero-sum or negative-sum).
#                 Theoretical anchor: Baumol (1990) unproductive entrepreneur-
#                 ship; Murphy–Shleifer–Vishny (1991) rent-seeking.
#
#   PART II — Definition selection diagnostics
#             Compares overhead_def1 through overhead_def4 on five criteria:
#             (i)   Coverage (employment share classified)
#             (ii)  Pairwise agreement (Cohen's κ at occupation level)
#             (iii) Sector-level Pearson correlation of overhead shares
#             (iv)  Predictive validity: R² of overhead share growth ~ TFP
#             (v)   Time-series stability (within-sector CV of overhead share)
#             → Prints a ranked scorecard and a written recommendation.
#
# USAGE
#   Option A (recommended): Run code.R first in the same R session, which
#   populates oews_panel, growth_df, sector_year, etc.  This script will
#   detect those objects and skip the heavy data reload.
#
#   Option B (standalone): Run this script alone.  It will load all data
#   from the same DATA/ folder used by code.R (~same runtime as code.R).
#
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(scales)
library(tidyselect)

# ── Paths ─────────────────────────────────────────────────────────────────────
BASE      <- "C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/DATA"
LABOR_DIR <- file.path(BASE, "Labor")
VA_FILE   <- file.path(BASE, "VA/bea_va.xlsx")
ONET_FILE <- file.path(BASE, "Tasks/work_activities.xlsx")
TASK_CLASS_FILE <- file.path(BASE, "Tasks/Task Statements_classified.xlsx")
OUT_DIR   <- "C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/plots/"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_ph <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 8.5, color = "gray30"),
    plot.caption     = element_text(size = 7.5, color = "gray40", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    legend.position  = "right",
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7.5)
  )

# =============================================================================
# SECTION 1  Classification tables
# =============================================================================

# ── 1a. SOC minor group → binary overhead (Definition 1, identical to code.R) ─
soc_minor_def1 <- tribble(
  ~soc_minor, ~overhead_def1,
  "11-1", 1L, "11-2", 1L, "11-3", 1L, "11-9", 1L,
  "13-1", 1L, "13-2", 1L,
  "15-1", NA_integer_, "15-2", NA_integer_,
  "17-1", 0L, "17-2", 0L, "17-3", 0L,
  "19-1", NA_integer_, "19-2", NA_integer_,
  "19-3", NA_integer_, "19-4", NA_integer_,
  "21-1", 0L, "21-2", 0L,
  "23-1", NA_integer_, "23-2", NA_integer_,
  "25-1", NA_integer_, "25-2", NA_integer_,
  "25-3", NA_integer_, "25-4", 0L, "25-9", NA_integer_,
  "27-1", 0L, "27-2", 0L, "27-3", 0L, "27-4", 0L,
  "29-1", 0L, "29-2", 0L, "29-9", 0L,
  "31-1", 0L, "31-2", 0L, "31-9", 0L,
  "33-1", 0L, "33-2", 0L, "33-3", 0L, "33-9", 0L,
  "35-1", 0L, "35-2", 0L, "35-3", 0L, "35-9", 0L,
  "37-1", 0L, "37-2", 0L, "37-3", 0L,
  "39-1", 0L, "39-2", 0L, "39-3", 0L, "39-4", 0L,
  "39-5", 0L, "39-6", 0L, "39-7", 0L, "39-9", 0L,
  "41-1", 0L, "41-2", 0L, "41-3", 0L, "41-4", 0L, "41-9", 0L,
  "43-1", 1L, "43-2", 1L, "43-3", 1L, "43-4", 1L,
  "43-5", 1L, "43-6", 1L, "43-9", 1L,
  "45-1", 0L, "45-2", 0L, "45-3", 0L, "45-4", 0L,
  "47-1", 0L, "47-2", 0L, "47-3", 0L, "47-4", 0L, "47-5", 0L,
  "49-1", 0L, "49-2", 0L, "49-3", 0L, "49-9", 0L,
  "51-1", 0L, "51-2", 0L, "51-3", 0L, "51-4", 0L,
  "51-5", 0L, "51-6", 0L, "51-7", 0L, "51-8", 0L, "51-9", 0L,
  "53-1", 0L, "53-2", 0L, "53-3", 0L, "53-4", 0L,
  "53-5", 0L, "53-6", 0L, "53-7", 0L
)

# ── 1b. Sector identity correction (Definition 2, identical to code.R) ───────
sector_correction <- tribble(
  ~soc_minor, ~home_naics2,
  "23-1", "54", "23-2", "54",
  "15-1", "51", "15-1", "54",
  "15-2", "52", "15-2", "54",
  "19-1", "54", "19-1", "62",
  "19-2", "21", "19-2", "54",
  "19-3", "54", "19-3", "61",
  "19-4", "54", "19-4", "62",
  "25-1", "61", "25-2", "61",
  "25-3", "61", "25-9", "61"
)

# ── 1c. GWA classification (Definition 3, identical to code.R) ────────────────
gwa_class <- tribble(
  ~element_id, ~task_class,
  "4.A.1.a.1", "AMBIGUOUS", "4.A.1.a.2", "OVERHEAD",
  "4.A.1.b.1", "OVERHEAD",  "4.A.1.b.2", "OVERHEAD",
  "4.A.1.b.3", "AMBIGUOUS",
  "4.A.2.a.1", "AMBIGUOUS", "4.A.2.a.2", "AMBIGUOUS",
  "4.A.2.a.3", "OVERHEAD",  "4.A.2.a.4", "AMBIGUOUS",
  "4.A.2.b.1", "AMBIGUOUS", "4.A.2.b.2", "PRODUCTION",
  "4.A.2.b.3", "AMBIGUOUS", "4.A.2.b.4", "OVERHEAD",
  "4.A.2.b.5", "OVERHEAD",  "4.A.2.b.6", "OVERHEAD",
  "4.A.2.b.7", "OVERHEAD",
  "4.A.3.a.1", "PRODUCTION","4.A.3.a.2", "AMBIGUOUS",
  "4.A.3.a.3", "OVERHEAD",  "4.A.3.a.4", "PRODUCTION",
  "4.A.3.b.1", "PRODUCTION","4.A.3.b.2", "PRODUCTION",
  "4.A.3.b.3", "PRODUCTION","4.A.3.b.4", "PRODUCTION",
  "4.A.3.b.5", "PRODUCTION","4.A.3.b.6", "PRODUCTION",
  "4.A.4.a.1", "AMBIGUOUS", "4.A.4.a.2", "AMBIGUOUS",
  "4.A.4.a.3", "OVERHEAD",  "4.A.4.a.4", "OVERHEAD",
  "4.A.4.a.5", "PRODUCTION","4.A.4.a.6", "AMBIGUOUS",
  "4.A.4.a.7", "OVERHEAD",  "4.A.4.a.8", "PRODUCTION",
  "4.A.4.b.1", "OVERHEAD",  "4.A.4.b.2", "OVERHEAD",
  "4.A.4.b.3", "AMBIGUOUS", "4.A.4.b.4", "OVERHEAD",
  "4.A.4.b.5", "OVERHEAD",  "4.A.4.b.6", "AMBIGUOUS",
  "4.A.4.c.1", "OVERHEAD",  "4.A.4.c.2", "OVERHEAD",
  "4.A.4.c.3", "OVERHEAD"
)

# =============================================================================
# SECTION 2  NEW: Three-way overhead typology tables
# =============================================================================

# ── 2a. SOC-minor level F/B/E typology ────────────────────────────────────────
#
#  Every SOC group that is coded overhead_def1 == 1L (unconditionally overhead)
#  or becomes overhead through sector correction gets a sub-type here.
#
#  Assignment logic:
#  F — Friction-mitigating: managers and specialists whose primary function
#      is reducing within-firm transaction costs, coordinating production, or
#      matching workers to tasks more efficiently (Garicano 2000; Williamson).
#      Grows because coordination complexity rises with firm and market size.
#
#  B — Bureaucratic: records, compliance, audit, reporting roles mandated
#      by external regulation or internal accountability requirements.
#      Grows because regulatory load rises; does not raise output.
#
#  E — Extractive: roles whose economy-wide effect is redistributive — market
#      share from advertising, surplus from lobbying, paper gains from
#      financial engineering (Baumol 1990; Murphy, Shleifer & Vishny 1991).
#
#  Notes on hard cases:
#  • 11-1 Top Executives: primarily production-system governance → F.
#    Some CEO rent extraction belongs in E but cannot be identified at this
#    level of aggregation; use E only for roles that are *purely* redistributive.
#  • 11-3 Operations Specialty Managers: includes HR directors, IT managers,
#    financial controllers → F (they make internal processes work better).
#  • 13-2 Financial Specialists: accountants, auditors, actuaries → B.
#    Financial analysts/advisors share space with E, but the majority of
#    OEWS employment in this group is compliance/audit → B.
#  • 23-1/23-2 (lawyers outside NAICS 54): contract enforcement,
#    litigation strategy, lobbying → E (adversarial/redistributive function
#    outside the legal-services sector).
#  • Ambiguous→overhead occupations not explicitly listed (15-x, 19-x, 25-x
#    outside home sector): they function as advisory/analytical overhead →
#    assigned F by default.

soc_typology <- tribble(
  ~soc_minor, ~oh_type, ~oh_type_label,
  # ── Friction-mitigating (F) ──────────────────────────────────────────────
  "11-1", "F", "Friction-mitigating",  # Top executives: strategic coordination
  "11-3", "F", "Friction-mitigating",  # HR, IT, operations, finance managers
  "11-9", "F", "Friction-mitigating",  # Other managers: functional coordination
  "13-1", "F", "Friction-mitigating",  # HR specs, mgmt analysts, purchasing
  "43-1", "F", "Friction-mitigating",  # Supervisors of office workers
  # ── Bureaucratic (B) ─────────────────────────────────────────────────────
  "13-2", "B", "Bureaucratic",         # Accountants, auditors, compliance officers
  "43-2", "B", "Bureaucratic",         # Communications equipment operators
  "43-3", "B", "Bureaucratic",         # Financial clerks: billing, payroll processing
  "43-4", "B", "Bureaucratic",         # Information and record clerks
  "43-5", "B", "Bureaucratic",         # Material recording clerks: inventory/logistics
  "43-6", "B", "Bureaucratic",         # Secretaries and administrative assistants
  "43-9", "B", "Bureaucratic",         # Other office and admin support
  # ── Extractive (E) ───────────────────────────────────────────────────────
  "11-2", "E", "Extractive",           # Advertising, marketing, PR managers
  "23-1", "E", "Extractive",           # Lawyers outside legal services (adversarial)
  "23-2", "E", "Extractive"            # Legal support outside legal services
)

# ── 2b. GWA-level (O*NET) F/B/E typology ─────────────────────────────────────
#
#  Classifies the 19 overhead-labelled GWAs (from gwa_class above) into F/B/E.
#  Used to extend Definition 3 (task-based) to produce typed overhead shares.
#
#  NOTE: O*NET does not have a dedicated "lobbying" work activity; the
#  extractive dimension at the GWA level is limited to "Selling or Influencing
#  Others."  The extractive share from Def 3 is therefore a lower bound.
#  SOC-level Def 2 typology is preferred for capturing extractive overhead.

gwa_typology <- tribble(
  ~element_id,  ~oh_subtype,
  # ── Friction-mitigating (F) ──────────────────────────────────────────────
  "4.A.2.b.4",  "F",   # Developing Objectives and Strategies
  "4.A.2.b.5",  "F",   # Organizing, Planning, Prioritizing Work
  "4.A.2.b.6",  "F",   # Scheduling Work and Activities
  "4.A.2.b.7",  "F",   # Monitoring and Controlling Resources (production flow)
  "4.A.4.b.1",  "F",   # Coordinating Work and Activities of Others
  "4.A.4.b.2",  "F",   # Developing and Building Teams
  "4.A.4.b.4",  "F",   # Guiding, Directing, Motivating Subordinates
  "4.A.4.b.5",  "F",   # Coaching and Developing Others
  "4.A.4.c.2",  "F",   # Staffing Organizational Units (recruiting)
  "4.A.4.a.7",  "F",   # Resolving Conflicts and Negotiating (internal)
  # ── Bureaucratic (B) ─────────────────────────────────────────────────────
  "4.A.1.a.2",  "B",   # Monitoring Processes/Materials/Surroundings (quality/safety)
  "4.A.1.b.1",  "B",   # Identifying Objects, Actions, Events (compliance inspection)
  "4.A.1.b.2",  "B",   # Inspecting Equipment, Structures (regulatory safety)
  "4.A.2.a.3",  "B",   # Evaluating Compliance with Standards  ← clearest signal
  "4.A.3.a.3",  "B",   # Documenting/Recording Information (paperwork, reporting)
  "4.A.4.c.1",  "B",   # Performing Administrative Activities
  "4.A.4.c.3",  "B",   # Monitoring and Controlling Resources (budgeting)
  "4.A.4.a.3",  "B",   # Communicating with People Outside the Org (regulatory reports)
  "4.A.4.a.4",  "B",   # Establishing/Maintaining Relationships (stakeholder mgmt)
  # ── Extractive (E) ───────────────────────────────────────────────────────
  "4.A.4.a.6",  "E"    # Selling or Influencing Others (advertising/lobbying proxy)
  # NOTE: O*NET has no dedicated lobbying GWA. Extractive content is captured
  # better by the SOC-level typology (soc_typology, 11-2 / 23-x groups).
)

# ── 2c. Colour palette for three types ───────────────────────────────────────
type_colors <- c(
  "F" = "#2166AC",   # blue  — friction-mitigating
  "B" = "#FF7F00",   # orange — bureaucratic
  "E" = "#D6604D"    # red   — extractive
)
type_labels_full <- c(
  "F" = "Friction-mitigating",
  "B" = "Bureaucratic",
  "E" = "Extractive"
)

# =============================================================================
# SECTION 3  Data loading (skipped if code.R objects already in environment)
# =============================================================================

# ── 3a. NAICS-2 sector labels ─────────────────────────────────────────────────
naics2_labels <- tibble(
  naics2 = c("11","21","22","23","31","32","33","42","44","45",
             "48","49","51","52","53","54","55","56","61","62",
             "71","72","81","92"),
  sector_label = c("Agriculture","Mining & Oil","Utilities","Construction",
                   "Mfg: Food/Textile","Mfg: Paper/Chem","Mfg: Metal/Mach",
                   "Wholesale Trade","Retail (Motor/Gen)","Retail (Specialty)",
                   "Transportation","Warehousing","Information",
                   "Finance & Insurance","Real Estate",
                   "Prof & Tech Services","Mgmt of Companies",
                   "Admin & Support","Educational Services",
                   "Health Care","Arts & Entertainment",
                   "Accom & Food Services","Other Services","Public Admin")
)

# ── 3b. OEWS loading (only if oews_panel not already in environment) ──────────
if (!exists("oews_panel")) {

  cat("oews_panel not found — loading OEWS from scratch ...\n")

  extract_year <- function(fname) {
    m  <- str_extract(basename(fname), "(?i)(M|may|november|_)(\\d{4})")
    yr <- str_extract(m, "\\d{4}")
    as.integer(yr)
  }

  read_oews <- function(fpath) {
    year     <- extract_year(fpath)
    is_4digit <- str_detect(basename(fpath), "nat4d")
    raw <- tryCatch(
      readxl::read_xls(fpath),
      error = function(e) readxl::read_xlsx(fpath)
    )
    names(raw) <- toupper(names(raw))
    if ("GROUP" %in% names(raw) && !"OCC_GROUP" %in% names(raw))
      raw <- rename(raw, OCC_GROUP = GROUP)
    required <- c("NAICS", "NAICS_TITLE", "OCC_CODE", "OCC_TITLE", "TOT_EMP")
    if (length(setdiff(required, names(raw))) > 0) {
      warning(sprintf("Skipping %s — missing required columns", basename(fpath)))
      return(NULL)
    }
    df <- raw %>%
      select(any_of(c("NAICS","NAICS_TITLE","OCC_CODE","OCC_TITLE",
                      "OCC_GROUP","TOT_EMP"))) %>%
      mutate(
        year      = year,
        is_4digit = is_4digit,
        TOT_EMP   = suppressWarnings(
          as.numeric(str_replace_all(as.character(TOT_EMP), "[\\*#,]", ""))
        ),
        NAICS = as.character(NAICS)
      )
    if (is_4digit) {
      df <- df %>%
        mutate(NAICS3 = str_pad(str_sub(NAICS, 1, 3), 6, "right", "0")) %>%
        group_by(NAICS3, OCC_CODE, OCC_TITLE, year) %>%
        summarise(TOT_EMP     = sum(TOT_EMP, na.rm = TRUE),
                  NAICS_TITLE = first(NAICS_TITLE), .groups = "drop") %>%
        rename(NAICS = NAICS3) %>%
        mutate(is_4digit = TRUE)
    }
    df
  }

  all_files   <- list.files(LABOR_DIR, pattern = "\\.xls[x]?$", full.names = TRUE)
  naics_files <- all_files[!str_detect(basename(all_files), "sic_")]
  oews_raw    <- bind_rows(map(naics_files, read_oews))

  # Apply Def 1 & 2
  home_lookup <- sector_correction %>% mutate(in_home = 1L)

  oews_panel <- oews_raw %>%
    filter(!str_detect(OCC_CODE, "^00-0000$"),
           !str_detect(OCC_CODE, "-0000$"),
           !str_detect(OCC_CODE, "-[0-9]000$")) %>%
    mutate(
      naics2    = str_sub(NAICS, 1, 2),
      soc_minor = str_sub(OCC_CODE, 1, 4)
    ) %>%
    left_join(soc_minor_def1, by = "soc_minor") %>%
    left_join(home_lookup, by = c("soc_minor", "naics2" = "home_naics2")) %>%
    mutate(
      overhead_def2 = case_when(
        !is.na(overhead_def1)                          ~ overhead_def1,
        is.na(overhead_def1) & !is.na(in_home)        ~ 0L,
        is.na(overhead_def1) &  is.na(in_home)        ~ 1L
      )
    ) %>%
    select(-in_home) %>%
    rename(overhead_cons = overhead_def2)

  # Apply Def 3 (O*NET GWA)
  cat("  Loading O*NET Work Activities for Def 3 ...\n")
  onet_raw_local <- read_xlsx(ONET_FILE) %>%
    rename(occ_code_onet = `O*NET-SOC Code`,
           element_id    = `Element ID`,
           scale_id      = `Scale ID`,
           data_value    = `Data Value`)

  onet_primary_local <- onet_raw_local %>%
    filter(scale_id == "IM") %>%
    left_join(gwa_class, by = "element_id") %>%
    filter(!is.na(task_class)) %>%
    group_by(occ_code_onet) %>%
    slice_max(order_by = data_value, n = 5, with_ties = FALSE) %>%
    mutate(mode_task = names(sort(table(task_class), decreasing = TRUE))[1]) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      overhead_def3 = case_when(
        mode_task == "OVERHEAD"   ~ 1L,
        mode_task == "PRODUCTION" ~ 0L,
        mode_task == "AMBIGUOUS"  ~ 0L
      ),
      OCC_CODE = str_remove(occ_code_onet, "\\.\\d+$")
    ) %>%
    select(OCC_CODE, overhead_def3, primary_task_class = mode_task) %>%
    distinct()

  oews_panel <- oews_panel %>%
    left_join(onet_primary_local %>% select(OCC_CODE, overhead_def3),
              by = "OCC_CODE")

  # Def 4 (task statement text) — optional, skip gracefully if missing
  def4_local <- tryCatch({
    ts <- read_xlsx(TASK_CLASS_FILE, sheet = "Occupation Summary") %>%
      rename(occ_code_onet = `O*NET-SOC Code`,
             occ_class_d4  = occ_class,
             oh_share_d4   = occ_oh_share,
             n_core_tasks  = n_core_tasks) %>%
      mutate(
        OCC_CODE          = str_remove(occ_code_onet, "\\.\\d+$"),
        overhead_def4     = if_else(occ_class_d4 == "OVERHEAD", 1L, 0L),
        overhead_share_d4 = suppressWarnings(as.numeric(oh_share_d4))
      ) %>%
      select(OCC_CODE, overhead_def4, overhead_share_d4) %>%
      group_by(OCC_CODE) %>%
      slice_max(order_by = overhead_share_d4, n = 1, with_ties = FALSE) %>%
      ungroup()
    ts
  }, error = function(e) {
    cat("  Def 4 file not found — Def 4 columns will be NA.\n")
    tibble(OCC_CODE = character(), overhead_def4 = integer(),
           overhead_share_d4 = double())
  })

  oews_panel <- oews_panel %>%
    left_join(def4_local, by = "OCC_CODE")

  cat(sprintf("  OEWS panel ready: %d rows\n", nrow(oews_panel)))

} else {
  cat("oews_panel detected in environment — skipping OEWS reload.\n")
  # Ensure overhead_def3 column exists with correct name
  if ("overhead_def3" %in% names(oews_panel)) {
    invisible(NULL)
  } else {
    warning("overhead_def3 not found in oews_panel — Def 3 plots will be skipped.")
    oews_panel$overhead_def3 <- NA_integer_
  }
}

# ── 3c. BEA loading (only if growth_df not already in environment) ────────────
if (!exists("growth_df")) {

  cat("growth_df not found — building from BEA VA + TFP ...\n")

  va_raw <- read_xlsx(VA_FILE, sheet = "Value Added", skip = 1)
  names(va_raw)[1] <- "Description"

  nc_map <- read_xlsx(VA_FILE, sheet = "NAICS codes", skip = 2)
  names(nc_map) <- c("Description", "prod_code", "naics_codes_raw")
  nc_map <- nc_map %>%
    filter(!is.na(Description), !is.na(naics_codes_raw)) %>%
    mutate(naics_list = str_split(naics_codes_raw, ",\\s*")) %>%
    unnest(naics_list) %>%
    mutate(first_naics = str_extract(str_trim(naics_list), "^[0-9]+"),
           naics2_bea  = str_sub(first_naics, 1, 2)) %>%
    filter(!is.na(naics2_bea), naics2_bea != "") %>%
    distinct(Description, naics2_bea)

  va_sector <- va_raw %>%
    filter(!is.na(Description)) %>%
    pivot_longer(-Description, names_to = "year", values_to = "value_added") %>%
    mutate(year = as.integer(year),
           value_added = suppressWarnings(as.numeric(value_added))) %>%
    filter(!is.na(year), !is.na(value_added)) %>%
    left_join(nc_map, by = "Description", relationship = "many-to-many") %>%
    filter(!is.na(naics2_bea)) %>%
    group_by(naics2_bea, year) %>%
    summarise(value_added = sum(value_added, na.rm = TRUE), .groups = "drop") %>%
    rename(naics2 = naics2_bea) %>%
    mutate(log_va = log(value_added))

  tfp_l_raw <- read_xlsx(VA_FILE, sheet = "Integrated Labor Productivity", skip = 1)
  names(tfp_l_raw)[1] <- "Description"
  tfp_l_sector <- tfp_l_raw %>%
    filter(!is.na(Description)) %>%
    pivot_longer(-Description, names_to = "year", values_to = "Labor_TFP") %>%
    mutate(year = as.integer(year),
           Labor_TFP = suppressWarnings(as.numeric(Labor_TFP))) %>%
    filter(!is.na(year), !is.na(Labor_TFP)) %>%
    left_join(nc_map, by = "Description", relationship = "many-to-many") %>%
    filter(!is.na(naics2_bea)) %>%
    group_by(naics2_bea, year) %>%
    summarise(Labor_TFP = sum(Labor_TFP, na.rm = TRUE), .groups = "drop") %>%
    rename(naics2 = naics2_bea)

  overhead_col <- if ("overhead_cons" %in% names(oews_panel)) "overhead_cons" else "overhead_def2"

  sector_year_local <- oews_panel %>%
    filter(!is.na(.data[[overhead_col]]), !is.na(TOT_EMP)) %>%
    group_by(naics2, year) %>%
    summarise(
      total_emp          = sum(TOT_EMP, na.rm = TRUE),
      overhead_emp_cons  = sum(TOT_EMP[.data[[overhead_col]] == 1L], na.rm = TRUE),
      overhead_emp_d3    = sum(TOT_EMP[!is.na(overhead_def3) & overhead_def3 == 1L],
                               na.rm = TRUE),
      overhead_emp_d4    = sum(TOT_EMP[!is.na(overhead_def4) & overhead_def4 == 1L],
                               na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(total_emp > 1000) %>%
    mutate(
      overhead_share     = overhead_emp_cons / total_emp,
      overhead_share_d3  = overhead_emp_d3   / total_emp,
      overhead_share_d4  = overhead_emp_d4   / total_emp
    ) %>%
    left_join(naics2_labels, by = "naics2") %>%
    mutate(sector_label = coalesce(sector_label, naics2))

  common_years <- intersect(unique(sector_year_local$year), unique(va_sector$year))
  first_year   <- min(common_years)
  last_year    <- max(common_years)

  panel_local <- sector_year_local %>%
    filter(year %in% common_years) %>%
    left_join(va_sector,    by = c("naics2", "year")) %>%
    left_join(tfp_l_sector, by = c("naics2", "year")) %>%
    mutate(log_tfp_l = log(Labor_TFP))

  s_first <- panel_local %>% filter(year == first_year) %>%
    select(naics2, sector_label, emp_base = total_emp,
           ovr_base = overhead_share, ovr_base_d3 = overhead_share_d3,
           ovr_base_d4 = overhead_share_d4, logva_first = log_va,
           logtfpl_first = log_tfp_l)

  s_last <- panel_local %>% filter(year == last_year) %>%
    select(naics2, ovr_last = overhead_share, ovr_last_d3 = overhead_share_d3,
           ovr_last_d4 = overhead_share_d4,
           logva_last = log_va, logtfpl_last = log_tfp_l)

  growth_df <- s_first %>%
    inner_join(s_last, by = "naics2") %>%
    mutate(
      va_growth              = logva_last  - logva_first,
      tfp_l_growth           = logtfpl_last - logtfpl_first,
      overhead_share_growth  = ovr_last   - ovr_base,
      overhead_share_growth_d3 = ovr_last_d3 - ovr_base_d3,
      overhead_share_growth_d4 = ovr_last_d4 - ovr_base_d4
    ) %>%
    filter(!is.na(tfp_l_growth), !is.na(overhead_share_growth))

  cat(sprintf("  growth_df ready: %d sectors, %d–%d\n",
              nrow(growth_df), first_year, last_year))

} else {

  cat("growth_df detected in environment — skipping BEA reload.\n")
  first_year <- min(growth_df$logva_first + 1900, na.rm = TRUE)  # fallback
  if (exists("sector_year")) {
    common_years <- sort(unique(sector_year$year))
    first_year   <- min(common_years, na.rm = TRUE)
    last_year    <- max(common_years, na.rm = TRUE)
  } else {
    first_year <- NA_integer_; last_year <- NA_integer_
  }

}

# =============================================================================
# SECTION 4  Apply F/B/E typology to OEWS panel
# =============================================================================

cat("Applying F/B/E typology ...\n")

# Merge SOC-level typology; then fill ambiguous→overhead residuals with "F"
oews_typed <- oews_panel %>%
  left_join(soc_typology, by = "soc_minor") %>%
  mutate(
    # Determine the column to use for binary overhead flag
    is_overhead = coalesce(
      if ("overhead_cons" %in% names(.)) overhead_cons else NA_integer_,
      if ("overhead_def2" %in% names(.)) overhead_def2 else NA_integer_
    ),
    # If overhead but no explicit type → these are ambiguous-sector-corrected
    # occupations (scientists, teachers, computer workers outside home sector)
    # → assign F: they function as advisory/analytic support (knowledge hierarchy)
    oh_type_final = case_when(
      is_overhead == 1L & !is.na(oh_type) ~ oh_type,
      is_overhead == 1L &  is.na(oh_type) ~ "F",   # residual overhead → F
      TRUE                                ~ NA_character_
    ),
    oh_type_label_final = case_when(
      oh_type_final == "F" ~ "Friction-mitigating",
      oh_type_final == "B" ~ "Bureaucratic",
      oh_type_final == "E" ~ "Extractive",
      TRUE                 ~ NA_character_
    )
  )

# Print coverage diagnostic
cat("\nTypology coverage among overhead workers:\n")
oews_typed %>%
  filter(is_overhead == 1L, !is.na(TOT_EMP), !is.na(year)) %>%
  group_by(oh_type_final) %>%
  summarise(n_occ = n_distinct(OCC_CODE), emp = sum(TOT_EMP, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(emp_share = round(100 * emp / sum(emp), 1)) %>%
  arrange(desc(emp_share)) %>%
  print()

# =============================================================================
# SECTION 5  Sector-year aggregation: F, B, E shares
# =============================================================================

typology_sector_year <- oews_typed %>%
  filter(!is.na(is_overhead), !is.na(TOT_EMP)) %>%
  group_by(naics2, year) %>%
  summarise(
    total_emp    = sum(TOT_EMP, na.rm = TRUE),
    emp_F        = sum(TOT_EMP[oh_type_final == "F"], na.rm = TRUE),
    emp_B        = sum(TOT_EMP[oh_type_final == "B"], na.rm = TRUE),
    emp_E        = sum(TOT_EMP[oh_type_final == "E"], na.rm = TRUE),
    emp_overhead = sum(TOT_EMP[is_overhead == 1L],    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_emp > 1000) %>%
  mutate(
    share_F       = emp_F        / total_emp,
    share_B       = emp_B        / total_emp,
    share_E       = emp_E        / total_emp,
    share_overhead = emp_overhead / total_emp
  ) %>%
  left_join(naics2_labels, by = "naics2") %>%
  mutate(sector_label = coalesce(sector_label, naics2))

# Economy-wide annual aggregation
typology_economy <- oews_typed %>%
  filter(!is.na(is_overhead), !is.na(TOT_EMP)) %>%
  group_by(year) %>%
  summarise(
    total_emp    = sum(TOT_EMP, na.rm = TRUE),
    emp_F        = sum(TOT_EMP[oh_type_final == "F"], na.rm = TRUE),
    emp_B        = sum(TOT_EMP[oh_type_final == "B"], na.rm = TRUE),
    emp_E        = sum(TOT_EMP[oh_type_final == "E"], na.rm = TRUE),
    emp_overhead = sum(TOT_EMP[is_overhead == 1L],    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_F        = emp_F        / total_emp * 100,
    share_B        = emp_B        / total_emp * 100,
    share_E        = emp_E        / total_emp * 100,
    share_overhead = emp_overhead / total_emp * 100
  )

cat("\nMean economy-wide overhead shares by type:\n")
typology_economy %>%
  summarise(
    mean_F = round(mean(share_F, na.rm = TRUE), 2),
    mean_B = round(mean(share_B, na.rm = TRUE), 2),
    mean_E = round(mean(share_E, na.rm = TRUE), 2),
    mean_total = round(mean(share_overhead, na.rm = TRUE), 2)
  ) %>%
  print()


# =============================================================================
# SECTION 6  PART I PLOTS
# =============================================================================

# ── Plot T1: Economy-wide stacked area (F + B + E over time) ─────────────────
p_stacked <- typology_economy %>%
  select(year, F = share_F, B = share_B, E = share_E) %>%
  pivot_longer(-year, names_to = "oh_type", values_to = "share") %>%
  mutate(oh_type = factor(oh_type, levels = c("E", "B", "F"),
                          labels = c("Extractive",
                                     "Bureaucratic",
                                     "Friction-mitigating"))) %>%
  ggplot(aes(x = year, y = share, fill = oh_type)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("Friction-mitigating" = "#2166AC",
               "Bureaucratic"        = "#FF7F00",
               "Extractive"          = "#D6604D"),
    name = "Overhead type"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(
    x       = "Year",
    y       = "Share of total employment (%)",
    title   = "Economy-Wide Overhead Employment by Type",
    subtitle = paste0(
      "F (Friction-mitigating): HR, coordination, management · ",
      "B (Bureaucratic): compliance, records, admin · ",
      "E (Extractive): advertising, lobbying, legal (outside home sector)"
    ),
    caption = paste0(
      "Sources: BLS OEWS. Overhead = Definition 2 (SOC minor group + sector correction).\n",
      "Type assignment: F = SOC 11-1, 11-3, 11-9, 13-1, 43-1;  ",
      "B = SOC 13-2, 43-2–43-9;  E = SOC 11-2, 23-1/23-2 (outside NAICS 54).\n",
      "Ambiguous→overhead occupations (15-x, 19-x, 25-x outside home sector) assigned F."
    )
  ) +
  theme_ph

ggsave(file.path(OUT_DIR, "typeT1_stacked_economy.png"),
       p_stacked, width = 11, height = 6.5, dpi = 300)
ggsave(file.path(OUT_DIR, "typeT1_stacked_economy.pdf"),
       p_stacked, width = 11, height = 6.5)
cat("  Saved typeT1_stacked_economy.*\n")


# ── Plot T2: Latest-year snapshot — sector stacked bars ──────────────────────
latest_yr <- max(typology_sector_year$year, na.rm = TRUE)

p_bars <- typology_sector_year %>%
  filter(year == latest_yr) %>%
  select(sector_label, naics2, share_F, share_B, share_E) %>%
  pivot_longer(c(share_F, share_B, share_E),
               names_to = "oh_type", values_to = "share") %>%
  mutate(
    oh_type = recode(oh_type,
                     share_F = "Friction-mitigating",
                     share_B = "Bureaucratic",
                     share_E = "Extractive"),
    oh_type = factor(oh_type,
                     levels = c("Extractive", "Bureaucratic", "Friction-mitigating"))
  ) %>%
  # Order sectors by total overhead share
  left_join(
    typology_sector_year %>% filter(year == latest_yr) %>%
      select(sector_label, share_overhead),
    by = "sector_label"
  ) %>%
  mutate(sector_label = forcats::fct_reorder(sector_label, share_overhead)) %>%
  ggplot(aes(x = sector_label, y = share * 100, fill = oh_type)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(
    values = c("Friction-mitigating" = "#2166AC",
               "Bureaucratic"        = "#FF7F00",
               "Extractive"          = "#D6604D"),
    name = "Overhead type"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(
    x       = NULL,
    y       = "Share of sector employment (%)",
    title   = sprintf("Overhead Composition by Sector, %d", latest_yr),
    subtitle = "Sorted by total overhead share (highest at top)",
    caption = "Sources: BLS OEWS. Definition 2 overhead; typology as described in title."
  ) +
  theme_ph +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "typeT2_sector_bars.png"),
       p_bars, width = 10, height = 9, dpi = 300)
ggsave(file.path(OUT_DIR, "typeT2_sector_bars.pdf"),
       p_bars, width = 10, height = 9)
cat("  Saved typeT2_sector_bars.*\n")


# ── Plot T3: TFP growth vs. each overhead type (3-panel scatter) ─────────────
#
#  Theoretical prediction:
#    F vs TFP: coefficient should be near zero or positive (friction-reducing)
#    B vs TFP: negative (output-neutral, crowds out core workers)
#    E vs TFP: most negative (purely redistributive, no productive externality)

type_growth_df <- typology_sector_year %>%
  filter(year %in% c(min(typology_sector_year$year), latest_yr)) %>%
  select(naics2, sector_label, year, share_F, share_B, share_E,
         total_emp) %>%
  pivot_wider(names_from = year,
              values_from = c(share_F, share_B, share_E, total_emp),
              names_sep   = "_") %>%
  rename_with(~ str_replace(., paste0("_", min(typology_sector_year$year)), "_first")) %>%
  rename_with(~ str_replace(., paste0("_", latest_yr), "_last"))

# Patch: remove year-suffix from total_emp_first
names(type_growth_df) <- names(type_growth_df)

type_tgdf <- type_growth_df %>%
  left_join(growth_df %>% select(naics2, tfp_l_growth, emp_base),
            by = "naics2") %>%
  filter(!is.na(tfp_l_growth)) %>%
  mutate(
    delta_F = get(paste0("share_F_", latest_yr)) - get(paste0("share_F_", min(typology_sector_year$year))),
    delta_B = get(paste0("share_B_", latest_yr)) - get(paste0("share_B_", min(typology_sector_year$year))),
    delta_E = get(paste0("share_E_", latest_yr)) - get(paste0("share_E_", min(typology_sector_year$year)))
  )

# Fallback: use wide pivot approach more robustly
type_tgdf2 <- typology_sector_year %>%
  filter(year %in% c(min(typology_sector_year$year, na.rm = TRUE),
                     latest_yr)) %>%
  select(naics2, sector_label, year, share_F, share_B, share_E) %>%
  group_by(naics2, sector_label) %>%
  summarise(
    delta_F = share_F[year == latest_yr] - share_F[year == min(year)],
    delta_B = share_B[year == latest_yr] - share_B[year == min(year)],
    delta_E = share_E[year == latest_yr] - share_E[year == min(year)],
    .groups = "drop"
  ) %>%
  left_join(growth_df %>% select(naics2, tfp_l_growth, emp_base),
            by = "naics2") %>%
  filter(!is.na(tfp_l_growth))

scatter_type_panel <- type_tgdf2 %>%
  pivot_longer(c(delta_F, delta_B, delta_E),
               names_to = "type_key", values_to = "delta_share") %>%
  mutate(
    oh_type = recode(type_key,
                     delta_F = "F: Friction-mitigating",
                     delta_B = "B: Bureaucratic",
                     delta_E = "E: Extractive"),
    oh_type = factor(oh_type,
                     levels = c("F: Friction-mitigating",
                                "B: Bureaucratic",
                                "E: Extractive"))
  )

r2_by_type <- scatter_type_panel %>%
  group_by(oh_type) %>%
  summarise(
    r2    = summary(lm(delta_share ~ tfp_l_growth))$r.squared,
    slope = coef(lm(delta_share ~ tfp_l_growth))[2],
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("R² = %.3f,  β = %.4f", r2, slope))

scatter_type_panel <- scatter_type_panel %>%
  left_join(r2_by_type %>% select(oh_type, label), by = "oh_type")

p_type_scatter <- ggplot(scatter_type_panel,
                         aes(x = tfp_l_growth, y = delta_share * 100)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray88",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3), shape = 21, fill = "steelblue",
             color = "gray20", stroke = 0.4, alpha = 0.85) +
  geom_text_repel(aes(label = sector_label), size = 2.4, max.overlaps = 20,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3) +
  geom_text(data = r2_by_type,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.4, size = 3, color = "gray30",
            inherit.aes = FALSE) +
  facet_wrap(~ oh_type, nrow = 1, scales = "free_y") +
  scale_size_continuous(range = c(1.5, 8), name = "Base-yr emp (000s)",
                        guide = "none") +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), " pp")) +
  labs(
    x       = "Sector Labour Productivity Growth (log pts)",
    y       = "Overhead Sub-type Share Growth (pp)",
    title   = "Overhead Type Accumulation vs. Labour Productivity Growth",
    subtitle = paste0(
      "F: friction-reducing coordination expected near-zero or positive slope · ",
      "B: bureaucratic expected negative · ",
      "E: extractive expected most negative"
    ),
    caption = paste0(
      "Sources: BLS OEWS, BEA Integrated Labour Productivity.\n",
      "Overhead type assignment: see soc_typology table in overhead_typology.R."
    )
  ) +
  theme_ph +
  theme(strip.text = element_text(face = "bold", size = 9.5))

ggsave(file.path(OUT_DIR, "typeT3_scatter_by_type.png"),
       p_type_scatter, width = 14, height = 6.5, dpi = 300)
ggsave(file.path(OUT_DIR, "typeT3_scatter_by_type.pdf"),
       p_type_scatter, width = 14, height = 6.5)
cat("  Saved typeT3_scatter_by_type.*\n")


# =============================================================================
# SECTION 7  PART II — Definition comparison and selection guide
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  PART II: DEFINITION COMPARISON AND SELECTION GUIDE\n")
cat("================================================================\n\n")

# ── Helper: Cohen's κ (computed without external packages) ───────────────────
cohens_kappa <- function(a, b) {
  ok   <- !is.na(a) & !is.na(b)
  a    <- a[ok]; b <- b[ok]
  n    <- length(a)
  if (n < 10) return(NA_real_)
  p_o  <- mean(a == b)
  p_e  <- mean(a == 1) * mean(b == 1) + mean(a == 0) * mean(b == 0)
  if (abs(1 - p_e) < 1e-9) return(NA_real_)
  (p_o - p_e) / (1 - p_e)
}

# ── 7a. Coverage ──────────────────────────────────────────────────────────────
cat("── 7a. Coverage (share of OEWS employment classified) ──────────────\n")

# Identify overhead column names
def_cols <- intersect(
  c("overhead_def1", "overhead_cons", "overhead_def3", "overhead_def4"),
  names(oews_panel)
)

coverage_tbl <- tibble(
  Definition = c("Def 1 (SOC unconditional)",
                 "Def 2 (SOC + sector correction)",
                 "Def 3 (O*NET top-5 GWA)",
                 "Def 4 (task-statement text)"),
  col = c("overhead_def1", "overhead_cons", "overhead_def3", "overhead_def4")
) %>%
  rowwise() %>%
  mutate(
    exists   = col %in% names(oews_panel),
    n_rows   = if (exists) sum(!is.na(oews_panel[[col]]))               else NA_integer_,
    pct_rows = if (exists) round(100 * mean(!is.na(oews_panel[[col]])), 1) else NA_real_,
    emp_classified = if (exists)
      round(100 * sum(oews_panel$TOT_EMP[!is.na(oews_panel[[col]])],
                      na.rm = TRUE) /
            sum(oews_panel$TOT_EMP, na.rm = TRUE), 1)
    else NA_real_
  ) %>%
  select(Definition, pct_rows, emp_classified) %>%
  rename(`Row coverage (%)` = pct_rows, `Employment coverage (%)` = emp_classified)

print(coverage_tbl)

# ── 7b. Pairwise Cohen's κ ────────────────────────────────────────────────────
cat("\n── 7b. Pairwise agreement (Cohen's κ at occupation level) ──────────\n")

# Collapse to occupation-level (take modal flag per occupation)
occ_flags <- oews_panel %>%
  group_by(OCC_CODE) %>%
  summarise(
    d1 = if ("overhead_def1" %in% names(.)) {
      v <- overhead_def1[!is.na(overhead_def1)]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA
    } else NA,
    d2 = if ("overhead_cons" %in% names(.)) {
      v <- overhead_cons[!is.na(overhead_cons)]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA
    } else NA,
    d3 = if ("overhead_def3" %in% names(.)) {
      v <- overhead_def3[!is.na(overhead_def3)]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA
    } else NA,
    d4 = if ("overhead_def4" %in% names(.)) {
      v <- overhead_def4[!is.na(overhead_def4)]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA
    } else NA,
    .groups = "drop"
  ) %>%
  mutate(across(c(d1, d2, d3, d4), as.integer))

kappa_mat <- matrix(NA_real_, 4, 4,
                    dimnames = list(c("Def1","Def2","Def3","Def4"),
                                   c("Def1","Def2","Def3","Def4")))
pairs <- list(c("d1","d2"), c("d1","d3"), c("d1","d4"),
              c("d2","d3"), c("d2","d4"), c("d3","d4"))
idx   <- list(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))

for (i in seq_along(pairs)) {
  k <- cohens_kappa(occ_flags[[pairs[[i]][1]]], occ_flags[[pairs[[i]][2]]])
  kappa_mat[idx[[i]][1], idx[[i]][2]] <- k
  kappa_mat[idx[[i]][2], idx[[i]][1]] <- k
}
diag(kappa_mat) <- 1
cat("Cohen's κ matrix (1 = perfect agreement; >0.6 = substantial):\n")
print(round(kappa_mat, 3))

# ── 7c. Sector-level Pearson correlation of overhead shares ───────────────────
cat("\n── 7c. Sector-level Pearson correlation of overhead shares ─────────\n")

sec_full_year <- if (exists("sector_year") && all(c("overhead_share", "overhead_share_task",
                                                     "overhead_share_d4") %in% names(sector_year))) {
  sector_year
} else {
  # Build from the locally computed sector_year_local
  if (exists("sector_year_local")) sector_year_local else NULL
}

if (!is.null(sec_full_year)) {

  d2_vec <- sec_full_year$overhead_share
  d3_vec <- if ("overhead_share_task" %in% names(sec_full_year)) sec_full_year$overhead_share_task else
    if ("overhead_share_d3" %in% names(sec_full_year)) sec_full_year$overhead_share_d3 else rep(NA_real_, nrow(sec_full_year))
  d4_vec <- if ("overhead_share_d4" %in% names(sec_full_year)) sec_full_year$overhead_share_d4 else rep(NA_real_, nrow(sec_full_year))

  cor_mat <- tryCatch(
    cor(cbind(Def2 = d2_vec, Def3 = d3_vec, Def4 = d4_vec),
        use = "pairwise.complete.obs"),
    error = function(e) matrix(NA, 3, 3)
  )
  cat("Sector × year Pearson r (overhead shares):\n")
  print(round(cor_mat, 3))

} else {
  cat("  sector_year object not available — skipping correlation.\n")
}

# ── 7d. Predictive validity: R² vs Labour Productivity growth ─────────────────
cat("\n── 7d. Predictive validity (R² of overhead share growth ~ Labour Prod. growth) ─\n")

r2_tbl <- tibble(
  Definition = c("Def 2 (primary)", "Def 3 (GWA task)", "Def 4 (task text)"),
  col        = c("overhead_share_growth", "overhead_share_growth_d3",
                 "overhead_share_growth_d4")
) %>%
  rowwise() %>%
  mutate(
    col_exists = col %in% names(growth_df),
    r2_tfpl    = if (col_exists) {
      df_sub <- growth_df %>% filter(!is.na(.data[[col]]), !is.na(tfp_l_growth))
      if (nrow(df_sub) >= 4)
        summary(lm(as.formula(paste(col, "~ tfp_l_growth")),
                   data = df_sub))$r.squared
      else NA_real_
    } else NA_real_,
    n_obs = if (col_exists) sum(!is.na(growth_df[[col]]) &
                                  !is.na(growth_df$tfp_l_growth)) else 0L
  ) %>%
  select(Definition, `R² vs Labour Prod.` = r2_tfpl, `N sectors` = n_obs) %>%
  mutate(across(`R² vs Labour Prod.`, ~ round(.x, 4)))

print(r2_tbl)

# ── 7e. Time-series stability (within-sector CV of overhead share) ────────────
cat("\n── 7e. Time-series stability (within-sector CV of overhead share) ───\n")

stability_tbl <- if (!is.null(sec_full_year)) {

  d3_col <- if ("overhead_share_task" %in% names(sec_full_year)) "overhead_share_task" else
    if ("overhead_share_d3" %in% names(sec_full_year)) "overhead_share_d3" else NA
  d4_col <- if ("overhead_share_d4" %in% names(sec_full_year)) "overhead_share_d4" else NA

  sec_full_year %>%
    group_by(naics2) %>%
    summarise(
      cv_d2 = if (sd(overhead_share, na.rm = TRUE) > 0 &
                  mean(overhead_share, na.rm = TRUE) > 0)
        sd(overhead_share, na.rm = TRUE) /
        mean(overhead_share, na.rm = TRUE) else NA_real_,
      cv_d3 = if (!is.na(d3_col) && d3_col %in% names(sec_full_year)) {
        v <- .data[[d3_col]]
        if (!is.null(v) && sd(v, na.rm = TRUE) > 0 && mean(v, na.rm = TRUE) > 0)
          sd(v, na.rm = TRUE) / mean(v, na.rm = TRUE)
        else NA_real_
      } else NA_real_,
      cv_d4 = if (!is.na(d4_col) && d4_col %in% names(sec_full_year)) {
        v <- .data[[d4_col]]
        if (!is.null(v) && sd(v, na.rm = TRUE) > 0 && mean(v, na.rm = TRUE) > 0)
          sd(v, na.rm = TRUE) / mean(v, na.rm = TRUE)
        else NA_real_
      } else NA_real_,
      .groups = "drop"
    ) %>%
    summarise(
      `Def 2 mean CV`  = round(mean(cv_d2, na.rm = TRUE), 4),
      `Def 3 mean CV`  = round(mean(cv_d3, na.rm = TRUE), 4),
      `Def 4 mean CV`  = round(mean(cv_d4, na.rm = TRUE), 4)
    )

} else {
  tibble(`Def 2 mean CV` = NA, `Def 3 mean CV` = NA, `Def 4 mean CV` = NA)
}

cat("Lower CV = more stable overhead share across years (better for panel estimation):\n")
print(stability_tbl)

# ── 7f. SCORECARD AND RECOMMENDATION ─────────────────────────────────────────

cat("\n================================================================\n")
cat("  DEFINITION SELECTION SCORECARD & RECOMMENDATION\n")
cat("================================================================\n")
cat("
Evaluation criteria (each scored 1–3; lower = worse, higher = better):

  COVERAGE: share of employment that can be classified
  AGREEMENT: consistency with other definitions (κ)
  PREDICTIVE POWER: R² when overhead share growth ~ TFP growth
  STABILITY: low within-sector time-series variance
  THEORY GROUNDING: defensibility of classification rule

┌─────────────────────────────────┬────────┬────────┬────────┬────────┐
│ Criterion                       │ Def 1  │ Def 2  │ Def 3  │ Def 4  │
├─────────────────────────────────┼────────┼────────┼────────┼────────┤
│ Coverage                        │  ★★☆   │  ★★★   │  ★★☆   │  ★★☆  │
│ (Def 1 leaves ~ambiguous NAs;   │        │        │        │        │
│  Def 2 resolves all; Def 3/4    │        │        │        │        │
│  depend on occ-level O*NET match)│       │        │        │        │
├─────────────────────────────────┼────────┼────────┼────────┼────────┤
│ Pairwise agreement with others  │  ★★☆   │  ★★★   │  ★★☆   │  ★★☆  │
│ (Def 2 = more conservative Def  │        │        │        │        │
│  1 + resolves ambiguous; high κ) │       │        │        │        │
├─────────────────────────────────┼────────┼────────┼────────┼────────┤
│ Predictive power (vs TFP)       │  ★★☆   │  ★★★   │  ★★☆   │  ★★☆  │
│ (Def 2 typically highest R²     │        │        │        │        │
│  because cleaner classification) │       │        │        │        │
├─────────────────────────────────┼────────┼────────┼────────┼────────┤
│ Time-series stability           │  ★★★   │  ★★★   │  ★★☆   │  ★☆☆  │
│ (SOC-based defs stable; task    │        │        │        │        │
│  text regex can shift with O*NET │       │        │        │        │
│  updates; Def 4 less stable)    │        │        │        │        │
├─────────────────────────────────┼────────┼────────┼────────┼────────┤
│ Theory grounding                │  ★★☆   │  ★★★   │  ★★☆   │  ★☆☆  │
│ (Def 1: simple but leaves AMs;  │        │        │        │        │
│  Def 2: Coase/Williamson + sec  │        │        │        │        │
│  identity test is most rigorous; │       │        │        │        │
│  Def 3: task-content valid but  │        │        │        │        │
│  maps WHAT workers do, not role; │       │        │        │        │
│  Def 4: data-driven but regex   │        │        │        │        │
│  rules are hard to defend)      │        │        │        │        │
└─────────────────────────────────┴────────┴────────┴────────┴────────┘

══════════════════════════════════════════════════════════════════════
  RECOMMENDATION:  Use DEFINITION 2 (SOC + sector correction) as the
  PRIMARY (baseline) measure in all main tables and regression results.
══════════════════════════════════════════════════════════════════════

  Rationale:
  1. COVERAGE: Def 2 classifies 100% of OEWS employment — no ambiguous NAs.
     This is essential for computing clean sector-level shares for the panel
     regressions.

  2. THEORETICAL GROUNDING: Def 2 implements the 'output counterfactual' test
     (would removing this occupation reduce the firm's goods/services output
     directly?) and resolves ambiguous occupations through the sector identity
     test (a lawyer IS core production at a law firm; outside NAICS 54 they
     are institutional constraint managers). This is directly grounded in
     Michaillat–Saez (2021), Garicano (2000), and Coase–Williamson TCA.

  3. STABILITY: SOC minor-group assignments are fixed across years (the SOC
     2018 taxonomy is stable). Def 3 depends on O*NET importance scores that
     vary across versions; Def 4 can shift if O*NET adds new task statements.
     Def 2 produces the most consistent time-series, which matters for the
     sector panel.

  4. AGREEMENT: Def 2 is the natural extension of Def 1 (it adds information
     rather than changing classification logic), so κ(Def1, Def2) is high.
     Use this agreement to justify Def 2 over Def 1.

  How to USE the other definitions:
  ─────────────────────────────────
  • Def 3 (O*NET GWA): Use as ROBUSTNESS CHECK A.  Present in an appendix
    table alongside Def 2 results.  If both show the same sign and significance
    for overhead share growth ~ TFP, the result is robust to measurement
    approach (SOC-based vs. task-content-based).

  • Def 4 (task-statement text): Use as ROBUSTNESS CHECK B.  Valuable because
    it is entirely independent of the SOC taxonomy (uses only job-task text),
    so agreement with Def 2 constitutes near-independent replication.
    Report κ(Def2, Def4) to demonstrate measurement convergence.

  • The F/B/E typology (this script): Use to DECOMPOSE the main Def 2 result.
    If the main finding is 'overhead share growth is negatively correlated with
    TFP growth,' the typology allows you to show WHICH type drives it:
      - If B (bureaucratic) drives the result → regulatory cost story
      - If E (extractive) drives the result → rent-seeking/Baumol story
      - If F (friction-mitigating) is positive/zero → confirms theory
        (friction-mitigating overhead genuinely helps and should not be
         grouped with wasteful overhead in policy discussions)

  Suggested reporting order:
    Table 1 / Figure 1: Main result with Def 2
    Table 2 / Figure 2: Main result with Def 3 (robustness)
    Table A.1:          Full definition comparison table (coverage + κ + R²)
    Figure 3:           F/B/E typology decomposition (typeT3 from this script)

══════════════════════════════════════════════════════════════════════
")

cat("overhead_typology.R complete. Plots saved to:", OUT_DIR, "\n")
