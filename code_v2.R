# =============================================================================
# code_v2.R   Overhead Accumulation Analysis  (Version 2)
#
# Key changes from code.R:
#  1. PRIMARY DEFINITION: Definition 2 only (SOC minor-group classification
#     + sector identity correction).  Definitions 1 / 3 / 4 are retained as
#     a brief robustness appendix at the end but drive no main plots.
#
#  2. FOUR-WAY TYPOLOGY: Every overhead occupation is sub-classified as:
#       F â€” Friction-mitigating: HR, coordination, management that raises
#           internal production efficiency (Garicano 2000; Williamson 1985).
#       B â€” Bureaucratic: compliance, regulatory reporting, record-keeping â€”
#           privately necessary but output-neutral at the margin
#           (Michaillat & Saez 2021; Stigler 1971).
#       E â€” Extractive: advertising & marketing managers (11-2); activities
#           that compete for market share / rents without adding net value â€”
#           privately beneficial but socially redistributive / zero-sum
#           (Baumol 1990; Murphy, Shleifer & Vishny 1991).
#           Note: in-house lawyers (23-x) are classified B (compliance/contracts),
#           not E â€” the MSV argument operates at profession level, not firm level.
#       R â€” Knowledge-generating / R&D: scientists and researchers working
#           at non-research firms; produce future productive capacity, not
#           current goods (Romer 1990; Jones 1995; Griliches 1979).
#
#  3. PLOTS: replicates ALL plots from code.R (shift-share, macro trend,
#     sector spaghetti, overhead Ã— TFP/labor-prod), plus four new plots:
#       â€“ Stacked area: economy-wide F/B/E/R over time
#       â€“ Stacked sector bars: F/B/E/R composition latest year
#       â€“ Four-panel scatter: Î”(type-share) vs labour-productivity growth
#       â€“ R-type spotlight: R&D overhead vs TFP (tests Romer prediction)
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

# Explicitly pin dplyr verbs in case another package (e.g. MASS) masks them
select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
rename    <- dplyr::rename
summarise <- dplyr::summarise


# â”€â”€ Paths â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
BASE      <- "C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/DATA"
LABOR_DIR <- file.path(BASE, "Labor")
VA_FILE   <- file.path(BASE, "VA/bea_va.xlsx")
ONET_FILE <- file.path(BASE, "Tasks/work_activities.xlsx")
ONET_TASK <- file.path(BASE, "Tasks/work_tasks.xlsx")
TASK_CLASS_FILE <- file.path(BASE, "Tasks/Task Statements_classified.xlsx")
OUT_DIR   <- "C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/plots/"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# â”€â”€ Shared theme â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

# Overhead type colour palette (used throughout)
type_colors <- c(
  "F" = "#2166AC",   # blue  â€” friction-mitigating
  "B" = "#FF7F00",   # orange â€” bureaucratic
  "E" = "#D6604D",   # red   â€” extractive
  "R" = "#4DAF4A"    # green â€” R&D / knowledge-generating
)
type_labels_full <- c(
  "F" = "Friction-mitigating (F)",
  "B" = "Bureaucratic (B)",
  "E" = "Extractive (E)",
  "R" = "R&D / Knowledge-generating (R)"
)

# â”€â”€ NAICS-2 sector labels â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

# =============================================================================
# STEP A  Classification tables
# =============================================================================

# â”€â”€ A1. Def 2: SOC minor group binary overhead (unconditional) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
soc_minor_def1 <- tribble(
  ~soc_minor, ~overhead_def1, ~group_label,
  # â”€â”€ Management (unconditionally overhead) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "11-1",  1L, "Top Executives",
  "11-2",  1L, "Advertising/Marketing/PR Managers",
  "11-3",  1L, "Operations Specialty Managers",
  "11-9",  1L, "Other Managers",
  # â”€â”€ Business & Financial Operations (unconditionally overhead) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "13-1",  1L, "Business Operations Specialists",
  "13-2",  1L, "Financial Specialists",
  # â”€â”€ Computer & Mathematical (ambiguous â†’ resolved by sector) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "15-1",  NA_integer_, "Computer Occupations",
  "15-2",  NA_integer_, "Mathematical Science Occupations",
  # â”€â”€ Architecture & Engineering (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "17-1",  0L, "Architects and Surveyors",
  "17-2",  0L, "Engineers",
  "17-3",  0L, "Engineering Technicians",
  # â”€â”€ Life/Physical/Social Science (ambiguous â†’ resolved by sector) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "19-1",  NA_integer_, "Life Scientists",
  "19-2",  NA_integer_, "Physical Scientists",
  "19-3",  NA_integer_, "Social Scientists",
  "19-4",  NA_integer_, "Science Technicians",
  # â”€â”€ Community & Social Services (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "21-1",  0L, "Counselors and Social Workers",
  "21-2",  0L, "Religious Workers",
  # â”€â”€ Legal (ambiguous â†’ overhead outside NAICS 54) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "23-1",  NA_integer_, "Lawyers and Judicial Workers",
  "23-2",  NA_integer_, "Legal Support Workers",
  # â”€â”€ Education (ambiguous â†’ overhead outside NAICS 61) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "25-1",  NA_integer_, "Postsecondary Teachers",
  "25-2",  NA_integer_, "Primary/Secondary Teachers",
  "25-3",  NA_integer_, "Other Teachers",
  "25-4",  0L,          "Librarians and Curators",
  "25-9",  NA_integer_, "Other Education Workers",
  # â”€â”€ Arts/Design/Entertainment (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "27-1",  0L, "Art and Design Workers",
  "27-2",  0L, "Entertainers and Performers",
  "27-3",  0L, "Media and Communication Workers",
  "27-4",  0L, "Media Equipment Workers",
  # â”€â”€ Healthcare (core and ambiguous) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 29-x Practitioners: remove them â†’ patients get no diagnosis/treatment â†’ CORE.
  # 31-1/31-2: remove them â†’ patients receive no home care / therapy â†’ CORE.
  # 31-9 OTHER Healthcare Support: ambiguous residual group.  Within NAICS 62
  #   most workers (dental assistants, phlebotomists, massage therapists) are
  #   direct-service core.  BUT the group also captures medical transcriptionists,
  #   medical equipment preparers, and clinical admin workers who are institutional
  #   support, not direct care delivery.  â†’ treat as AMBIGUOUS, resolved by sector:
  #   inside NAICS 62 (Healthcare) = core; outside NAICS 62 = overhead.
  #   This is identical in logic to the 23-x (legal) and 25-x (education) treatment.
  "29-1",  0L,          "Health Diagnosing/Treating Practitioners",
  "29-2",  0L,          "Health Technologists",
  "29-9",  0L,          "Other Healthcare Practitioners",
  "31-1",  0L,          "Home Health and Personal Care Aides",
  "31-2",  0L,          "Therapy Assistants",
  "31-9",  NA_integer_, "Other Healthcare Support",  # ambiguous: resolved by sector
  # â”€â”€ Protective Service (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "33-1",  0L, "Supervisors of Protective Service",
  "33-2",  0L, "Firefighters",
  "33-3",  0L, "Law Enforcement Officers",
  "33-9",  0L, "Other Protective Service",
  # â”€â”€ Food Preparation (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "35-1",  0L, "Supervisors of Food Preparation",
  "35-2",  0L, "Cooks and Food Preparation Workers",
  "35-3",  0L, "Food and Beverage Serving Workers",
  "35-9",  0L, "Other Food Preparation",
  # â”€â”€ Building/Grounds (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "37-1",  0L, "Supervisors of Building Workers",
  "37-2",  0L, "Building Cleaning Workers",
  "37-3",  0L, "Grounds Maintenance Workers",
  # â”€â”€ Personal Care (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "39-1",  0L, "Supervisors of Personal Care",
  "39-2",  0L, "Animal Care Workers",
  "39-3",  0L, "Entertainment Attendants",
  "39-4",  0L, "Funeral Service Workers",
  "39-5",  0L, "Personal Appearance Workers",
  "39-6",  0L, "Baggage Porters and Concierges",
  "39-7",  0L, "Tour Guides",
  "39-9",  0L, "Other Personal Care",
  # â”€â”€ Sales (mixed) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 41-1 Supervisors of Sales Workers: overhead F unconditionally.
  #   A sales supervisor coordinates the sales team but does not sell.
  #   Consistent with 43-1 (office supervisors) = overhead.
  # 41-2 Retail Sales Workers: CORE.  At a retail firm the sales worker IS the
  #   point-of-sale service; removing them eliminates the firm's output delivery.
  #   (output counterfactual test: YES).
  # 41-3/41-4/41-9 Sales Representatives: AMBIGUOUS.
  #   Core in their home sector (wholesale trade, professional services) where
  #   the sales transaction constitutes the primary firm output.  Outside that
  #   sector they compete for product market share (Baumol 1990 redistribution
  #   channel), though they also transmit product information (reducing buyer
  #   search costs) â€” so not purely extractive.  Advertising managers (11-2,
  #   already coded Extractive) capture the clearest competitive-overhead signal.
  #   Sales reps outside home sector â†’ overhead F (market-matching overhead).
  "41-1",  1L,          "Supervisors of Sales Workers",       # unconditionally overhead
  "41-2",  0L,          "Retail Sales Workers",               # core: point-of-sale = output
  "41-3",  NA_integer_, "Sales Representatives (Services)",   # ambiguous â†’ sector
  "41-4",  NA_integer_, "Sales Representatives (Wholesale/Mfg)", # ambiguous â†’ sector
  "41-9",  NA_integer_, "Other Sales",                        # ambiguous â†’ sector
  # â”€â”€ Office & Admin Support (unconditionally overhead) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "43-1",  1L, "Supervisors of Office Workers",
  "43-2",  1L, "Communications Equipment Operators",
  "43-3",  1L, "Financial Clerks",
  "43-4",  1L, "Information and Record Clerks",
  "43-5",  1L, "Material Recording Clerks",
  "43-6",  1L, "Secretaries and Admin Assistants",
  "43-9",  1L, "Other Office and Admin Support",
  # â”€â”€ Farming/Fishing/Forestry (core) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "45-1",  0L, "Supervisors of Farming Workers",
  "45-2",  0L, "Agricultural Workers",
  "45-3",  0L, "Fishing and Hunting Workers",
  "45-4",  0L, "Forest and Logging Workers",
  # â”€â”€ Construction & Extraction (core / ambiguous supervisor) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 47-1 Supervisors of Construction: AMBIGUOUS.
  #   Within NAICS 23 (Construction) or 21 (Mining/Extraction) these are working
  #   foremen who also direct physical work on-site â†’ core.
  #   Elsewhere (e.g. a construction supervisor at a retailer's facilities dept)
  #   they coordinate without producing â†’ overhead F.
  "47-1",  NA_integer_, "Supervisors of Construction",  # ambiguous â†’ sector
  "47-2",  0L, "Construction Trades Workers",
  "47-3",  0L, "Construction Helpers",
  "47-4",  0L, "Other Construction",
  "47-5",  0L, "Extraction Workers",
  # â”€â”€ Installation/Maintenance/Repair (core / ambiguous supervisor) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 49-1 Supervisors of Mechanics: AMBIGUOUS.
  #   Within NAICS 81 (Other Services, incl. repair shops) or 23 (Construction)
  #   these are shop foremen who often also do repairs â†’ core.
  #   Elsewhere (hospital maintenance director, facilities supervisor at a bank)
  #   they coordinate support staff with no connection to primary output â†’ overhead F.
  "49-1",  NA_integer_, "Supervisors of Mechanics",  # ambiguous â†’ sector
  "49-2",  0L, "Electrical Equipment Mechanics",
  "49-3",  0L, "Vehicle Mechanics",
  "49-9",  0L, "Other Maintenance and Repair",
  # â”€â”€ Production (core / ambiguous supervisor) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 51-1 Supervisors of Production Workers: AMBIGUOUS.
  #   Within NAICS 31/32/33 (Manufacturing) these are floor supervisors embedded
  #   in the production process; many BLS publications include them in production
  #   headcounts â†’ core within manufacturing.
  #   Outside manufacturing (a 'production supervisor' at a logistics or financial
  #   firm) they are pure coordinators with no direct output contribution â†’ overhead F.
  #   This resolves the asymmetry: 43-1 (office supervisors) = overhead; and now
  #   production supervisors outside their home sector = overhead too.
  "51-1",  NA_integer_, "Supervisors of Production Workers",  # ambiguous â†’ sector
  "51-2",  0L, "Assemblers and Fabricators",
  "51-3",  0L, "Food Processing Workers",
  "51-4",  0L, "Metal and Plastic Workers",
  "51-5",  0L, "Printing Workers",
  "51-6",  0L, "Textile and Apparel Workers",
  "51-7",  0L, "Woodworkers",
  "51-8",  0L, "Plant and System Operators",
  "51-9",  0L, "Other Production Occupations",
  # â”€â”€ Transportation & Material Moving (core / ambiguous supervisor) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # 53-1 Supervisors of Transportation: AMBIGUOUS.
  #   Within NAICS 48/49 (Transportation/Warehousing) these are dispatch
  #   supervisors and terminal managers embedded in the logistics network â†’ core.
  #   Elsewhere (fleet supervisor at a manufacturer, logistics coordinator at
  #   a retailer) they coordinate support functions â†’ overhead F.
  "53-1",  NA_integer_, "Supervisors of Transportation",  # ambiguous â†’ sector
  "53-2",  0L, "Air Transportation Workers",
  "53-3",  0L, "Motor Vehicle Operators",
  "53-4",  0L, "Rail Transportation Workers",
  "53-5",  0L, "Water Transportation Workers",
  "53-6",  0L, "Other Transportation Workers",
  "53-7",  0L, "Material Moving Workers"
)

# â”€â”€ A2. Sector identity correction (Def 2) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
sector_correction <- tribble(
  ~soc_minor, ~home_naics2,
  "23-1",  "54",
  "23-2",  "54",
  "15-1",  "51",
  "15-1",  "54",
  "15-2",  "52",
  "15-2",  "54",
  "19-1",  "54",
  "19-1",  "62",
  "19-2",  "21",
  "19-2",  "54",
  "19-3",  "54",
  "19-3",  "61",
  "19-4",  "54",
  "19-4",  "62",
  "25-1",  "61",
  "25-2",  "61",
  "25-3",  "61",
  "25-9",  "61",
  # 31-9 Other Healthcare Support: core WITHIN Healthcare (NAICS 62); overhead elsewhere
  # (e.g. corporate wellness staff, spa support workers at hotels, gym attendants at
  # fitness firms outside NAICS 62 â€” they are supporting internal ops, not clinical output).
  "31-9",  "62",
  # 41-3/41-4/41-9 Sales Representatives: core in sectors where selling IS the product
  # (wholesale distribution, retail, professional/tech services).
  "41-3",  "54",   # Sales reps (services) â†’ core at Prof/Tech Services
  "41-4",  "42",   # Sales reps (wholesale/mfg) â†’ core at Wholesale Trade
  "41-4",  "44",   # Sales reps â†’ also core at Retail (Motor/Gen)
  "41-4",  "45",   # Sales reps â†’ also core at Retail (Specialty)
  "41-9",  "44",   # Other sales â†’ core at Retail
  "41-9",  "45",   # Other sales â†’ also core at Retail (Specialty)
  # 47-1/49-1/51-1/53-1 Supervisors of production/trade workers:
  # core within their own industry (working foremen / embedded supervisors);
  # overhead (F) outside â€” consistent with 43-1 (office supervisors) = overhead.
  "47-1",  "23",   # Supervisors of Construction â†’ core in Construction
  "47-1",  "21",   # Supervisors of Construction â†’ also core in Mining/Extraction
  "49-1",  "81",   # Supervisors of Mechanics â†’ core in Other Services (repair shops)
  "49-1",  "23",   # Supervisors of Mechanics â†’ also core in Construction
  "51-1",  "31",   # Supervisors of Production â†’ core in Manufacturing
  "51-1",  "32",   # Supervisors of Production â†’ core in Manufacturing
  "51-1",  "33",   # Supervisors of Production â†’ core in Manufacturing
  "53-1",  "48",   # Supervisors of Transportation â†’ core in Transportation
  "53-1",  "49"    # Supervisors of Transportation â†’ also core in Warehousing
)

# â”€â”€ A3. Four-way typology: SOC-minor â†’ F / B / E / R â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
#
# TYPE F â€” Friction-mitigating overhead
#   Role: reduce internal transaction costs, coordinate labour allocation,
#   manage information flows within the firm.
#   Theory: Garicano (2000) knowledge hierarchy â€” managers handle problems
#   that production workers cannot solve, raising average worker productivity.
#   Williamson (1985) governance structures â€” hierarchies internalise market
#   failures in factor markets at lower transaction cost.
#   SOC groups: top executives, operations/HR/IT managers, management
#   analysts, purchasing agents, supervisors of office workers.
#
# TYPE B â€” Bureaucratic overhead
#   Role: process regulatory requirements, maintain audit trails, produce
#   mandatory reports, handle compliance correspondence.
#   Theory: Michaillat & Saez (2021) "Type I" overhead â€” administrative
#   workers hired to meet institutional mandates, not to raise output.
#   Stigler (1971) regulatory capture â€” compliance load grows with
#   regulatory accumulation independent of production needs.
#   SOC groups: accountants/auditors, financial clerks, admin assistants,
#   information/record clerks, material recording clerks.
#
# TYPE E â€” Extractive overhead
#   Role: shift market share, rents, or surplus to the firm at the expense
#   of rivals or taxpayers; produce no new value in aggregate.
#   Theory: Baumol (1990) "Productive, Unproductive and Destructive
#   Entrepreneurship" â€” talent misallocation toward rent-seeking.
#   Murphy, Shleifer & Vishny (1991) â€” high-ability workers in rent-seeking
#   activities earn private returns above social returns.  CRITICAL SCOPE:
#   MSV (1991) is a PROFESSION-LEVEL talent-allocation argument (talented
#   graduates choosing law over engineering reduces aggregate TFP growth) â€”
#   it is NOT an occupation-level within-firm function claim.  Applying it
#   to classify in-house corporate counsel as E would conflate economy-wide
#   talent misallocation with the firm-level overhead welfare test.
#   SOC group: advertising/marketing/PR managers (11-2) â€” clearest within-firm
#   E signal: brand advertising is zero-sum in aggregate (one firm's market-
#   share gain from advertising is a rival's loss; no net welfare gain).
#   WHY NOT 23-x (Lawyers / Legal Support outside NAICS 54):
#   In-house corporate counsel's dominant work is regulatory compliance (B)
#   and contract management (F: reduces Williamson transaction costs).  The
#   adversarial subset â€” lobbying, offensive patent litigation â€” is real but
#   a minority of in-house workload.  23-x outside NAICS 54 is classified B.
#
# TYPE R â€” R&D / Knowledge-generating overhead
#   Role: produce new knowledge, designs, and innovation blueprints â€”
#   increases future productive capacity, not current goods output.
#   Theory:
#   â€¢ Romer (1990): A-sector workers produce new designs È¦ = Î´L_AÂ·A.
#     Allocating workers to R is growth-enhancing but represents a
#     diversion from current goods production Y.
#   â€¢ Jones (1995), Jones & Williams (2000): social return to R&D â‰¥ 2Ã—
#     private return; firms UNDER-invest in R-type workers; R-type
#     overhead growth should correlate POSITIVELY with long-run TFP.
#   â€¢ Griliches (1979, 1992): R&D as investment, not current-period
#     consumption; R&D capital stock Î£áµ¢ R_i(1-Î´)^t enters production
#     function Y = f(K, L, R_stock).
#   â€¢ Cohen & Levinthal (1990): absorptive capacity â€” firms need internal
#     R&D workers to identify and assimilate external knowledge; this
#     creates a complementarity between R-overhead and total-factor-
#     productivity catch-up.
#   â€¢ Bloom, Jones, Van Reenen & Webb (2020): research productivity
#     declining â†’ secular upward trend in R-type headcount required to
#     sustain constant TFP growth, distinct from B/E trends.
#   Frascati Manual (OECD 2015): R&D personnel = researchers + technicians
#     + support staff engaged in R&D projects; maps primarily to SOC 15-x,
#     17-x, 19-x. Here restricted to 19-x outside home sector.
#   SOC groups: life/physical/social scientists and science technicians
#   working OUTSIDE their home NAICS (resolved as overhead by Def 2 sector
#   correction) â€” these are the embedded R&D workers at manufacturing and
#   service firms that most closely match Romer's A-sector allocation.
#
# AMBIGUOUS â†’ overhead RESIDUAL ASSIGNMENT:
#   15-1 Computer Occs outside NAICS 51/54 â†’ F (IT operations/support)
#   15-2 Math Occs outside NAICS 52/54    â†’ F (analytic support)
#   25-x Teachers outside NAICS 61        â†’ F (training and L&D functions)
#   23-1/23-2 outside NAICS 54            â†’ B (see note below)
#        In-house corporate counsel: dominant function is regulatory compliance
#        (B) + contract management (F-adjacent).  The MSV (1991) â€œmore lawyers
#        reduce TFPâ€ result operates at the profession / talent-allocation level,
#        not at the occupation / within-firm function level.  Lobbying and
#        offensive litigation are real but a minority of in-house workload.
#        Classifying 23-x as B is consistent with the output counterfactual test.
#   19-x outside home sector              â†’ R (embedded R&D/science)
#   31-9 outside NAICS 62                 â†’ B (institutional support)
#   41-3/41-4/41-9 outside home sector    â†’ F (market-matching overhead;
#        information/transaction coordination without direct output contribution)
#   41-1 unconditionally overhead         â†’ F (supervisor of sales = coordination)
#   47-1/49-1/51-1/53-1 outside home sector â†’ F (production coordinator outside
#        their own sector = pure management layer, no direct output contribution;
#        resolves asymmetry with 43-1 office supervisors already coded overhead)

soc_typology <- tribble(
  ~soc_minor, ~oh_type, ~oh_type_label,
  # â”€â”€ F: Friction-mitigating â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "11-1", "F", "Friction-mitigating",   # Top executives: strategic coord.
  "11-3", "F", "Friction-mitigating",   # HR, IT, operations, finance mgrs
  "11-9", "F", "Friction-mitigating",   # Other managers: functional coord.
  "13-1", "F", "Friction-mitigating",   # HR specs, mgmt analysts, purchasing
  "43-1", "F", "Friction-mitigating",   # Supervisors of office workers
  # 41-1 unconditionally overhead: a sales supervisor manages the sales team
  # but does not sell.  Consistent with 43-1 (office supervisors) = overhead.
  # Theoretically: Garicano layer-1 manager above sales workers.
  "41-1", "F", "Friction-mitigating",   # Supervisors of sales workers
  # â”€â”€ B: Bureaucratic â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  "13-2", "B", "Bureaucratic",          # Accountants, auditors, compliance
  "43-2", "B", "Bureaucratic",          # Communications equipment operators
  "43-3", "B", "Bureaucratic",          # Financial clerks: billing/payroll
  "43-4", "B", "Bureaucratic",          # Information and record clerks
  "43-5", "B", "Bureaucratic",          # Material recording / inventory clerks
  "43-6", "B", "Bureaucratic",          # Secretaries and admin assistants
  "43-9", "B", "Bureaucratic",          # Other office and admin support
  # 31-9 outside NAICS 62: institutional support overhead â€” corporate wellness,
  # hotel spa workers, gym staff at non-healthcare firms.  They support the
  # workforce but produce no primary output of the employing firm â†’ Bureaucratic
  # (closest to Michaillatâ€“Saez Type I: hired to meet institutional/HR mandate).
  "31-9", "B", "Bureaucratic",          # Healthcare support outside NAICS 62
  # 23-1/23-2 outside NAICS 54: in-house corporate counsel and legal support.
  # Dominant workload: regulatory compliance, employment law, contract review.
  # â†’ B (Bureaucratic): privately mandatory overhead, output-neutral at margin.
  # MSV (1991) profession-level argument does not map to occupation-level test.
  "23-1", "B", "Bureaucratic",          # Lawyers outside legal services
  "23-2", "B", "Bureaucratic",          # Legal support outside legal services
  # â”€â”€ E: Extractive â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Only 11-2: advertising is zero-sum in aggregate (brand competition).
  # The MSV "more lawyers/financiers â†’ less TFP" result is a profession-level
  # talent-allocation finding; within-firm 23-x function is compliance (B).
  "11-2", "E", "Extractive",            # Advertising, marketing, PR managers
  # â”€â”€ R: R&D / Knowledge-generating â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Assigned to 19-x groups only when outside home sector (i.e., they are
  # coded overhead by Def 2 sector correction).  Within home sector (NAICS
  # 54, 62, 21, 61) they remain core workers (overhead_def2 = 0).
  "19-1", "R", "R&D / Knowledge-generating",  # Life scientists outside 54/62
  "19-2", "R", "R&D / Knowledge-generating",  # Physical scientists outside 21/54
  "19-3", "R", "R&D / Knowledge-generating",  # Social scientists outside 54/61
  "19-4", "R", "R&D / Knowledge-generating"   # Science technicians outside 54/62
)

# Residual mapping for ambiguousâ†’overhead occupations NOT in soc_typology above
# (15-x and 25-x resolved as overhead outside home sector â†’ F by default)
oh_type_residual <- "F"

# =============================================================================
# STEP B  Load OEWS files
# =============================================================================

cat("Reading OEWS files ...\n")

extract_year <- function(fname) {
  m  <- str_extract(basename(fname), "(?i)(M|may|november|_)(\\d{4})")
  yr <- str_extract(m, "\\d{4}")
  as.integer(yr)
}

all_files   <- list.files(LABOR_DIR, pattern = "\\.xls[x]?$", full.names = TRUE)
naics_files <- all_files[!str_detect(basename(all_files), "sic_")]

read_oews <- function(fpath) {
  year      <- extract_year(fpath)
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
    warning(sprintf("Skipping %s â€” missing required columns", basename(fpath)))
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

oews_raw <- bind_rows(map(naics_files, read_oews))
cat(sprintf("  Loaded %d rows from %d OEWS files\n", nrow(oews_raw), length(naics_files)))

# =============================================================================
# STEP C  Apply Def 2 + F/B/E/R typology
# =============================================================================

cat("Applying Def 2 classification and F/B/E/R typology ...\n")

home_lookup <- sector_correction %>% mutate(in_home = 1L)

oews_panel <- oews_raw %>%
  filter(
    !str_detect(OCC_CODE, "^00-0000$"),
    !str_detect(OCC_CODE, "-0000$"),
    !str_detect(OCC_CODE, "-[0-9]000$")
  ) %>%
  mutate(
    naics2    = str_sub(NAICS, 1, 2),
    soc_minor = str_sub(OCC_CODE, 1, 4)
  ) %>%
  # Def 1 (unconditional)
  left_join(soc_minor_def1 %>% select(soc_minor, overhead_def1),
            by = "soc_minor") %>%
  # Def 2 (sector-relative)
  left_join(home_lookup, by = c("soc_minor", "naics2" = "home_naics2")) %>%
  mutate(
    overhead_def2 = case_when(
      !is.na(overhead_def1)                       ~ overhead_def1,
      is.na(overhead_def1) & !is.na(in_home)      ~ 0L,   # in home sector â†’ core
      is.na(overhead_def1) &  is.na(in_home)      ~ 1L    # outside home sector â†’ overhead
    )
  ) %>%
  select(-in_home) %>%
  # F/B/E/R typology
  left_join(soc_typology %>% select(soc_minor, oh_type, oh_type_label),
            by = "soc_minor") %>%
  mutate(
    # Assign explicit type where given; for ambiguousâ†’overhead residuals use F
    oh_type_final = case_when(
      overhead_def2 == 1L & !is.na(oh_type) ~ oh_type,
      overhead_def2 == 1L &  is.na(oh_type) ~ oh_type_residual,
      TRUE                                   ~ NA_character_
    ),
    oh_type_label = case_when(
      oh_type_final == "F" ~ "Friction-mitigating (F)",
      oh_type_final == "B" ~ "Bureaucratic (B)",
      oh_type_final == "E" ~ "Extractive (E)",
      oh_type_final == "R" ~ "R&D / Knowledge-gen. (R)",
      TRUE                 ~ NA_character_
    )
  )

# Coverage check
cat("\nDef 2 coverage:\n")
cat(sprintf("  Classified rows:      %.1f%%\n",
            100 * mean(!is.na(oews_panel$overhead_def2))))
cat(sprintf("  Overhead share (emp): %.1f%%\n",
            100 * sum(oews_panel$TOT_EMP[oews_panel$overhead_def2 == 1L],
                      na.rm = TRUE) /
              sum(oews_panel$TOT_EMP, na.rm = TRUE)))

cat("\nF/B/E/R type distribution (overhead employment):\n")
oews_panel %>%
  filter(overhead_def2 == 1L, !is.na(TOT_EMP)) %>%
  group_by(oh_type_final) %>%
  summarise(emp = sum(TOT_EMP, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = round(100 * emp / sum(emp), 1)) %>%
  arrange(desc(share)) %>%
  print()

# =============================================================================
# STEP D  Sector-year employment aggregation
# =============================================================================

sector_year <- oews_panel %>%
  filter(!is.na(overhead_def2), !is.na(TOT_EMP)) %>%
  group_by(naics2, year) %>%
  summarise(
    total_emp   = sum(TOT_EMP, na.rm = TRUE),
    # Total overhead
    overhead_emp = sum(TOT_EMP[overhead_def2 == 1L], na.rm = TRUE),
    # Typed
    emp_F        = sum(TOT_EMP[oh_type_final == "F"], na.rm = TRUE),
    emp_B        = sum(TOT_EMP[oh_type_final == "B"], na.rm = TRUE),
    emp_E        = sum(TOT_EMP[oh_type_final == "E"], na.rm = TRUE),
    emp_R        = sum(TOT_EMP[oh_type_final == "R"], na.rm = TRUE),
    core_emp     = sum(TOT_EMP[overhead_def2 == 0L], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_emp > 1000) %>%
  mutate(
    overhead_share = overhead_emp / total_emp,
    share_F        = emp_F        / total_emp,
    share_B        = emp_B        / total_emp,
    share_E        = emp_E        / total_emp,
    share_R        = emp_R        / total_emp,
    core_labor_share = core_emp   / total_emp
  ) %>%
  left_join(naics2_labels, by = "naics2") %>%
  mutate(sector_label = coalesce(sector_label, naics2))

# Mean overhead share diagnostics
cat("\nMean overhead share by sector (Def 2):\n")
sector_year %>%
  group_by(naics2, sector_label) %>%
  summarise(mean_oh = round(mean(overhead_share, na.rm = TRUE), 3),
            .groups = "drop") %>%
  arrange(desc(mean_oh)) %>%
  print(n = 30)

# =============================================================================
# STEP E  BEA Value Added and TFP
# =============================================================================

cat("Reading BEA Value Added ...\n")

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
  mutate(year        = as.integer(year),
         value_added = suppressWarnings(as.numeric(value_added))) %>%
  filter(!is.na(year), !is.na(value_added)) %>%
  left_join(nc_map, by = "Description", relationship = "many-to-many") %>%
  filter(!is.na(naics2_bea)) %>%
  group_by(naics2_bea, year) %>%
  summarise(value_added = sum(value_added, na.rm = TRUE), .groups = "drop") %>%
  rename(naics2 = naics2_bea) %>%
  mutate(log_va = log(value_added))

cat("Reading BEA Integrated TFP Index ...\n")

tfp_raw <- read_xlsx(VA_FILE, sheet = "Integrated TFP Index", skip = 1)
names(tfp_raw)[1] <- "Description"
tfp_sector <- tfp_raw %>%
  filter(!is.na(Description)) %>%
  pivot_longer(-Description, names_to = "year", values_to = "TFP") %>%
  mutate(year = as.integer(year),
         TFP  = suppressWarnings(as.numeric(TFP))) %>%
  filter(!is.na(year), !is.na(TFP)) %>%
  left_join(nc_map, by = "Description", relationship = "many-to-many") %>%
  filter(!is.na(naics2_bea)) %>%
  group_by(naics2_bea, year) %>%
  summarise(TFP = sum(TFP, na.rm = TRUE), .groups = "drop") %>%
  rename(naics2 = naics2_bea)

cat("Reading BEA Integrated Labour Productivity ...\n")

tfp_l_raw <- read_xlsx(VA_FILE, sheet = "Integrated Labor Productivity", skip = 1)
names(tfp_l_raw)[1] <- "Description"
tfp_l_sector <- tfp_l_raw %>%
  filter(!is.na(Description)) %>%
  pivot_longer(-Description, names_to = "year", values_to = "Labor_TFP") %>%
  mutate(year      = as.integer(year),
         Labor_TFP = suppressWarnings(as.numeric(Labor_TFP))) %>%
  filter(!is.na(year), !is.na(Labor_TFP)) %>%
  left_join(nc_map, by = "Description", relationship = "many-to-many") %>%
  filter(!is.na(naics2_bea)) %>%
  group_by(naics2_bea, year) %>%
  summarise(Labor_TFP = sum(Labor_TFP, na.rm = TRUE), .groups = "drop") %>%
  rename(naics2 = naics2_bea)

# =============================================================================
# STEP F  Growth variables and shift-share
# =============================================================================

common_years <- intersect(unique(sector_year$year), unique(va_sector$year))
first_year   <- min(common_years)
last_year    <- max(common_years)
cat(sprintf("Analysis period: %d â€“ %d\n", first_year, last_year))

panel <- sector_year %>%
  filter(year %in% common_years) %>%
  left_join(va_sector %>% filter(year %in% common_years),
            by = c("naics2", "year")) %>%
  left_join(tfp_sector %>% filter(year %in% common_years),
            by = c("naics2", "year")) %>%
  left_join(tfp_l_sector %>% filter(year %in% common_years),
            by = c("naics2", "year")) %>%
  mutate(log_tfp   = log(TFP),
         log_tfp_l = log(Labor_TFP))

sector_first <- panel %>%
  filter(year == first_year) %>%
  select(naics2, sector_label, emp_base = total_emp,
         ovr_base      = overhead_share,
         base_F        = share_F,
         base_B        = share_B,
         base_E        = share_E,
         base_R        = share_R,
         core_base     = core_labor_share,
         logva_first   = log_va,
         logtfp_first  = log_tfp,
         logtfpl_first = log_tfp_l)

sector_last <- panel %>%
  filter(year == last_year) %>%
  select(naics2,
         ovr_last    = overhead_share,
         last_F      = share_F,
         last_B      = share_B,
         last_E      = share_E,
         last_R      = share_R,
         core_last   = core_labor_share,
         logva_last  = log_va,
         logtfp_last = log_tfp,
         logtfpl_last= log_tfp_l)

growth_df <- sector_first %>%
  inner_join(sector_last, by = "naics2") %>%
  mutate(
    va_growth             = logva_last    - logva_first,
    tfp_growth            = logtfp_last   - logtfp_first,
    tfp_l_growth          = logtfpl_last  - logtfpl_first,
    overhead_share_growth = ovr_last      - ovr_base,
    core_share_growth     = core_last     - core_base,
    delta_F               = last_F        - base_F,
    delta_B               = last_B        - base_B,
    delta_E               = last_E        - base_E,
    delta_R               = last_R        - base_R
  ) %>%
  filter(!is.na(tfp_growth), !is.na(overhead_share_growth))

# â”€â”€ Shift-share expected overhead growth (national occupation-mix channel) â”€â”€â”€â”€
nat_first <- oews_panel %>%
  filter(year == first_year, !is.na(TOT_EMP), !is.na(overhead_def2)) %>%
  group_by(OCC_CODE) %>%
  summarise(nat_emp = sum(TOT_EMP, na.rm = TRUE), .groups = "drop") %>%
  mutate(nat_share_first = nat_emp / sum(nat_emp))

nat_last <- oews_panel %>%
  filter(year == last_year, !is.na(TOT_EMP), !is.na(overhead_def2)) %>%
  group_by(OCC_CODE) %>%
  summarise(nat_emp = sum(TOT_EMP, na.rm = TRUE), .groups = "drop") %>%
  mutate(nat_share_last = nat_emp / sum(nat_emp))

sec_occ_base <- oews_panel %>%
  filter(year == first_year, !is.na(TOT_EMP), !is.na(overhead_def2)) %>%
  group_by(naics2) %>%
  mutate(sector_total = sum(TOT_EMP, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sec_occ_share = TOT_EMP / sector_total) %>%
  select(naics2, OCC_CODE, sec_occ_share, overhead_def2)

expected_growth <- sec_occ_base %>%
  left_join(nat_first %>% select(OCC_CODE, nat_share_first), by = "OCC_CODE") %>%
  left_join(nat_last  %>% select(OCC_CODE, nat_share_last),  by = "OCC_CODE") %>%
  mutate(
    nat_ratio    = if_else(nat_share_first > 0,
                           nat_share_last / nat_share_first - 1, 0),
    contribution = sec_occ_share * overhead_def2 * nat_ratio
  ) %>%
  group_by(naics2) %>%
  summarise(expected_growth = sum(contribution, na.rm = TRUE), .groups = "drop")

growth_df <- growth_df %>%
  left_join(expected_growth, by = "naics2") %>%
  mutate(
    expected_growth   = coalesce(expected_growth, 0),
    residual_overhead = overhead_share_growth - expected_growth,
    residual_core     = core_share_growth - expected_growth
  )

# Economy-wide trend (for macro-trend plot)
economy_trend <- oews_panel %>%
  filter(!is.na(overhead_def2), !is.na(TOT_EMP)) %>%
  group_by(year) %>%
  summarise(
    total_emp  = sum(TOT_EMP, na.rm = TRUE),
    oh_total   = sum(TOT_EMP[overhead_def2 == 1L], na.rm = TRUE),
    oh_F       = sum(TOT_EMP[oh_type_final == "F"], na.rm = TRUE),
    oh_B       = sum(TOT_EMP[oh_type_final == "B"], na.rm = TRUE),
    oh_E       = sum(TOT_EMP[oh_type_final == "E"], na.rm = TRUE),
    oh_R       = sum(TOT_EMP[oh_type_final == "R"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_total = oh_total / total_emp * 100,
    share_F     = oh_F    / total_emp * 100,
    share_B     = oh_B    / total_emp * 100,
    share_E     = oh_E    / total_emp * 100,
    share_R     = oh_R    / total_emp * 100
  )

# Add core share to economy_trend
economy_trend <- economy_trend %>%
  left_join(
    oews_panel %>%
      filter(!is.na(overhead_def2), !is.na(TOT_EMP)) %>%
      group_by(year) %>%
      summarise(core_emp = sum(TOT_EMP[overhead_def2 == 0L], na.rm = TRUE),
                .groups = "drop"),
    by = "year"
  ) %>%
  mutate(share_core = core_emp / total_emp * 100)

# =============================================================================
# SHARED PLOT ELEMENTS
# =============================================================================

sector_colors <- c(
  "Agriculture"         = "#4daf4a",
  "Mining & Oil"        = "#a65628",
  "Utilities"           = "#ff7f00",
  "Construction"        = "#f781bf",
  "Mfg: Food/Textile"   = "#e41a1c",
  "Mfg: Paper/Chem"     = "#c44040",
  "Mfg: Metal/Mach"     = "#a83232",
  "Wholesale Trade"     = "#984ea3",
  "Retail (Motor/Gen)"  = "#377eb8",
  "Retail (Specialty)"  = "#2260a0",
  "Transportation"      = "#999999",
  "Warehousing"         = "#777777",
  "Information"         = "#a6cee3",
  "Finance & Insurance" = "#1f78b4",
  "Real Estate"         = "#b2df8a",
  "Prof & Tech Services"= "#33a02c",
  "Mgmt of Companies"   = "#fb9a99",
  "Admin & Support"     = "#fdbf6f",
  "Educational Services"= "#cab2d6",
  "Health Care"         = "#6a3d9a",
  "Arts & Entertainment"= "#d4a800",
  "Accom & Food Services"= "#b15928",
  "Other Services"      = "#d9d9d9",
  "Public Admin"        = "#708090"
)

caption_src <- paste0(
  "Sources: BLS OEWS, BEA GDP-by-Industry (Value Added + Integrated TFP).\n",
  "Overhead = Definition 2: SOC minor group classification + sector identity correction.\n",
  "Typology: F = friction-mitigating  Â·  B = bureaucratic  Â·  E = extractive  Â·  ",
  "R = R&D / knowledge-generating."
)

# =============================================================================
# HELPER: generic per-type scatter plot (saves PDF + PNG)
# =============================================================================

save_type_scatter <- function(df, y_col, x_col, type_code,
                               x_label_str, y_label_str,
                               plot_title, file_stem) {
  df_c <- df[!is.na(df[[y_col]]) & !is.na(df[[x_col]]), ]
  if (nrow(df_c) < 5) {
    cat("    Skipped", file_stem, "(n < 5)\n"); return(invisible(NULL))
  }
  fit   <- lm(reformulate(x_col, y_col), data = df_c)
  r2    <- summary(fit)$r.squared
  slope <- coef(fit)[[x_col]]
  sub   <- sprintf("OLS R\u00b2 = %.3f  |  \u03b2 = %+.4f", r2, slope)
  tc    <- type_colors[[type_code]]
  p <- ggplot(df_c, aes(x = .data[[x_col]], y = .data[[y_col]] * 100)) +
    geom_smooth(method = "lm", se = TRUE, color = tc, fill = tc,
                linewidth = 1, alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.6) +
    geom_point(aes(size = emp_base / 1e3), shape = 21,
               fill = tc, color = "gray20", stroke = 0.4, alpha = 0.9) +
    geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                    segment.color = "gray60", segment.size = 0.3,
                    box.padding = 0.3, min.segment.length = 0.2) +
    scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                          breaks = c(500, 2000, 5000, 10000),
                          labels = scales::comma) +
    scale_y_continuous(labels = function(x) paste0(round(x, 2), " pp")) +
    labs(x = x_label_str, y = y_label_str,
         title = plot_title, subtitle = sub, caption = caption_src) +
    theme_ph
  ggsave(file.path(OUT_DIR, paste0(file_stem, ".png")), p, width = 11, height = 7.5, dpi = 300)
  cat("    Saved", file_stem, "\n")
  invisible(p)
}

# Recession shading bands â€” shared across Section 1 trend plots
recession_bands <- data.frame(xmin = c(2008, 2020), xmax = c(2009, 2021),
                               ymin = -Inf, ymax = Inf) %>%
  filter(xmin >= min(economy_trend$year), xmax <= max(economy_trend$year))

# =============================================================================
# SECTION 1  Â·  EMPLOYMENT SHARE TRENDS
# [Core -> Total Overhead -> F/B/E/R]  x  [Economy-wide -> By Sector]
# =============================================================================
#   S1A_01  Core labour share, economy-wide
#   S1A_02  Total overhead share, economy-wide
#   S1A_03  F/B/E/R stacked area, economy-wide
#   S1A_04  Per-type individual trends, 4-panel
#   S1B_01  Core sector spaghetti
#   S1B_02  Total overhead sector spaghetti
#   S1B_03  Sector typology bars (latest year)
#   S1B_04  Per-type sector spaghetti, 4-panel
# =============================================================================

cat("\n=== SECTION 1: EMPLOYMENT SHARE TRENDS ===\n")

# ---- 1-A: Economy-wide trends -----------------------------------------------

# S1A_01 -- Core employment share over time ------------------------------------
cat("  S1A_01: core employment trend ...\n")
mean_core_sh <- mean(economy_trend$share_core, na.rm = TRUE)

pS1A_01 <- ggplot(economy_trend, aes(x = year)) +
  geom_rect(data = recession_bands,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = mean_core_sh, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  annotate("text", x = min(economy_trend$year) + 0.3, y = mean_core_sh + 0.5,
           label = sprintf("Mean: %.1f%%", mean_core_sh),
           size = 3, color = "gray40", hjust = 0) +
  geom_line(aes(y = share_core), color = "#2b6343", linewidth = 1.4) +
  geom_point(aes(y = share_core), color = "#2b6343", size = 2) +
  scale_x_continuous(breaks = seq(min(economy_trend$year),
                                  max(economy_trend$year), by = 2)) +
  labs(x = NULL, y = "Core Labour Share (%)",
       title = "Core Labour as Share of Total US Employment  (Def 2)",
       subtitle = sprintf(
         "Core = direct output workers (SOC groups not classified as overhead) | %d-%d",
         min(economy_trend$year), max(economy_trend$year)),
       caption = caption_src) +
  theme_ph + theme(panel.grid.major.x = element_blank())

ggsave(file.path(OUT_DIR, "v2_S1A_01_core_trend.png"),  pS1A_01, width = 10, height = 6, dpi = 300)
cat("    Saved v2_S1A_01_core_trend\n")


# S1A_02 -- Total overhead share over time -------------------------------------
cat("  S1A_02: total overhead trend ...\n")
mean_overhead_sh <- mean(economy_trend$share_total, na.rm = TRUE)

pS1A_02 <- ggplot(economy_trend, aes(x = year)) +
  geom_rect(data = recession_bands,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = mean_overhead_sh, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  annotate("text", x = min(economy_trend$year) + 0.3, y = mean_overhead_sh + 0.3,
           label = sprintf("Mean: %.1f%%", mean_overhead_sh),
           size = 3, color = "gray40", hjust = 0) +
  geom_line(aes(y = share_total), color = "#1a4f8a", linewidth = 1.4) +
  geom_point(aes(y = share_total), color = "#1a4f8a", size = 2) +
  annotate("text", x = max(economy_trend$year) - 0.5,
           y = last(economy_trend$share_total) + 0.4,
           label = "Total\noverhead", size = 2.8, color = "#1a4f8a", hjust = 1) +
  scale_x_continuous(breaks = seq(min(economy_trend$year),
                                  max(economy_trend$year), by = 2)) +
  labs(x = NULL, y = "Overhead Employment Share (%)",
       title = "Overhead Labour as Share of Total US Employment  (Def 2)",
       subtitle = sprintf(
         "Overhead = SOC 11 (Mgmt), 13 (Business/Finance), 23 (Legal outside law), 43 (Admin) | %d-%d",
         min(economy_trend$year), max(economy_trend$year)),
       caption = caption_src) +
  theme_ph + theme(panel.grid.major.x = element_blank())


ggsave(file.path(OUT_DIR, "v2_S1A_02_overhead_trend.png"),  pS1A_02, width = 10, height = 6, dpi = 300)
cat("    Saved v2_S1A_02_overhead_trend\n")


# S1A_03 -- F/B/E/R stacked area, economy-wide --------------------------------
cat("  S1A_03: stacked F/B/E/R area ...\n")

pS1A_03 <- economy_trend %>%
  select(year, F = share_F, B = share_B, E = share_E, R = share_R) %>%
  pivot_longer(-year, names_to = "oh_type", values_to = "share") %>%
  mutate(oh_type = factor(oh_type, levels = c("E", "B", "R", "F"),
                          labels = c("Extractive (E)", "Bureaucratic (B)",
                                     "R&D / Knowledge-gen. (R)",
                                     "Friction-mitigating (F)"))) %>%
  ggplot(aes(x = year, y = share, fill = oh_type)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("Friction-mitigating (F)"   = "#2166AC",
               "Bureaucratic (B)"           = "#FF7F00",
               "Extractive (E)"             = "#D6604D",
               "R&D / Knowledge-gen. (R)"   = "#4DAF4A"),
    name = "Overhead type") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(x = NULL, y = "Share of total employment (%)",
       title = "Economy-Wide Overhead Employment by Type  (Def 2)",
       subtitle = paste0(
         "F = HR/coordination  |  B = compliance/records  |  ",
         "E = advertising/lobbying  |  R = scientists outside research sector"),
       caption = caption_src) +
  theme_ph


ggsave(file.path(OUT_DIR, "v2_S1A_03_typology_stacked.png"),  pS1A_03, width = 11, height = 6.5, dpi = 300)
cat("    Saved v2_S1A_03_typology_stacked\n")


# S1A_04 -- Per-type individual trend lines, 4-panel --------------------------
cat("  S1A_04: per-type economy trends, 4-panel ...\n")

pS1A_04 <- economy_trend %>%
  select(year, F = share_F, B = share_B, E = share_E, R = share_R) %>%
  pivot_longer(-year, names_to = "oh_type", values_to = "share") %>%
  mutate(oh_type = factor(oh_type, levels = c("F", "B", "E", "R"),
                          labels = c("F: Friction-mitigating", "B: Bureaucratic",
                                     "E: Extractive", "R: R&D / Knowledge-gen."))) %>%
  ggplot(aes(x = year, y = share)) +
  geom_rect(
    data = recession_bands %>%
      tidyr::crossing(oh_type = factor(
        c("F: Friction-mitigating", "B: Bureaucratic",
          "E: Extractive", "R: R&D / Knowledge-gen."),
        levels = c("F: Friction-mitigating", "B: Bureaucratic",
                   "E: Extractive", "R: R&D / Knowledge-gen."))),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "gray80", alpha = 0.4) +
  geom_line(aes(color = oh_type), linewidth = 1.2) +
  geom_point(aes(color = oh_type), size = 1.5) +
  scale_color_manual(
    values = c("F: Friction-mitigating"   = "#2166AC",
               "B: Bureaucratic"           = "#FF7F00",
               "E: Extractive"             = "#D6604D",
               "R: R&D / Knowledge-gen."   = "#4DAF4A"),
    guide = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  facet_wrap(~ oh_type, nrow = 2, ncol = 2, scales = "free_y") +
  labs(x = NULL, y = "Share of total employment (%)",
       title = "Economy-Wide Employment Share by Overhead Type",
       subtitle = "Each panel shows one overhead subtype's share of total US employment over time",
       caption = caption_src) +
  theme_ph


ggsave(file.path(OUT_DIR, "v2_S1A_04_type_trends.png"),  pS1A_04, width = 13, height = 8, dpi = 300)
cat("    Saved v2_S1A_04_type_trends\n")


# ---- 1-B: Sector-level trends -----------------------------------------------

top12_sectors <- sector_year %>%
  group_by(naics2) %>%
  summarise(avg_emp = mean(total_emp, na.rm = TRUE), .groups = "drop") %>%
  slice_max(avg_emp, n = 12) %>%
  pull(naics2)

sector_year_top12 <- sector_year %>% filter(naics2 %in% top12_sectors)

econ_avg      <- economy_trend %>% mutate(share_cons = share_total / 100)
econ_avg_core <- economy_trend %>% mutate(core_cons  = share_core  / 100)

# S1B_01 -- Core sector spaghetti ---------------------------------------------
cat("  S1B_01: core sector spaghetti ...\n")
label_core_df <- sector_year_top12 %>%
  filter(year == last_year) %>%
  select(naics2, sector_label, core_labor_share)

pS1B_01 <- ggplot(sector_year_top12,
                  aes(x = year, y = core_labor_share,
                      color = sector_label, group = sector_label)) +
  geom_line(data = econ_avg_core, aes(x = year, y = core_cons),
            inherit.aes = FALSE, color = "black", linewidth = 1.4) +
  annotate("text", x = max(econ_avg_core$year) + 0.2,
           y = last(econ_avg_core$core_cons),
           label = "Economy\naverage", size = 2.6, hjust = 0, color = "black") +
  geom_line(linewidth = 0.75, alpha = 0.85) +
  geom_text_repel(
    data = label_core_df,
    aes(x = last_year, y = core_labor_share, label = sector_label,
        color = sector_label),
    inherit.aes = FALSE, size = 2.5, hjust = 0, direction = "y",
    nudge_x = 0.3, segment.size = 0.3,
    max.overlaps = 20, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.1))) +
  scale_x_continuous(breaks = seq(first_year, last_year, by = 2),
                     expand = expansion(add = c(0, 3))) +
  scale_color_manual(values = scales::hue_pal()(12)) +
  labs(x = NULL, y = "Core Labour Employment Share",
       title = "Core Labour Share by Sector Over Time  (Definition 2)",
       subtitle = "Top 12 sectors by employment  |  Bold = economy-wide average",
       caption = caption_src) +
  theme_ph +
  theme(legend.position = "none", panel.grid.major.x = element_blank())


ggsave(file.path(OUT_DIR, "v2_S1B_01_core_sector.png"),  pS1B_01, width = 11, height = 7, dpi = 300)
cat("    Saved v2_S1B_01_core_sector\n")


# S1B_02 -- Total overhead sector spaghetti -----------------------------------
cat("  S1B_02: overhead sector spaghetti ...\n")
label_df <- sector_year_top12 %>%
  filter(year == last_year) %>%
  select(naics2, sector_label, overhead_share)

pS1B_02 <- ggplot(sector_year_top12,
                  aes(x = year, y = overhead_share,
                      color = sector_label, group = sector_label)) +
  geom_line(data = econ_avg, aes(x = year, y = share_cons),
            inherit.aes = FALSE, color = "black", linewidth = 1.4) +
  annotate("text", x = max(econ_avg$year) + 0.2,
           y = last(econ_avg$share_cons),
           label = "Economy\naverage", size = 2.6, hjust = 0, color = "black") +
  geom_line(linewidth = 0.75, alpha = 0.85) +
  geom_text_repel(
    data = label_df,
    aes(x = last_year, y = overhead_share, label = sector_label,
        color = sector_label),
    inherit.aes = FALSE, size = 2.5, hjust = 0, direction = "y",
    nudge_x = 0.3, segment.size = 0.3,
    max.overlaps = 20, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.1))) +
  scale_x_continuous(breaks = seq(first_year, last_year, by = 2),
                     expand = expansion(add = c(0, 3))) +
  scale_color_manual(values = scales::hue_pal()(12)) +
  labs(x = NULL, y = "Overhead Employment Share",
       title = "Overhead Share by Sector Over Time  (Definition 2)",
       subtitle = "Top 12 sectors by employment  |  Bold = economy-wide average",
       caption = caption_src) +
  theme_ph +
  theme(legend.position = "none", panel.grid.major.x = element_blank())


ggsave(file.path(OUT_DIR, "v2_S1B_02_overhead_sector.png"),  pS1B_02, width = 11, height = 7, dpi = 300)
cat("    Saved v2_S1B_02_overhead_sector\n")


# S1B_03 -- Sector typology bars, F/B/E/R composition (latest year) -----------
cat("  S1B_03: sector typology bars ...\n")
sector_latest <- sector_year %>%
  filter(year == last_year) %>%
  select(sector_label, naics2, share_F, share_B, share_E, share_R, overhead_share)

pS1B_03 <- sector_latest %>%
  pivot_longer(c(share_F, share_B, share_E, share_R),
               names_to = "type_key", values_to = "share") %>%
  mutate(
    oh_type = recode(type_key,
                     share_F = "Friction-mitigating (F)",
                     share_B = "Bureaucratic (B)",
                     share_E = "Extractive (E)",
                     share_R = "R&D / Knowledge-gen. (R)"),
    oh_type = factor(oh_type, levels = c("Extractive (E)",
                                          "R&D / Knowledge-gen. (R)",
                                          "Bureaucratic (B)",
                                          "Friction-mitigating (F)"))) %>%
  left_join(sector_latest %>% select(sector_label, overhead_share),
            by = "sector_label") %>%
  mutate(sector_label = forcats::fct_reorder(sector_label, overhead_share.y)) %>%
  ggplot(aes(x = sector_label, y = share * 100, fill = oh_type)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(
    values = c("Friction-mitigating (F)"  = "#2166AC",
               "Bureaucratic (B)"          = "#FF7F00",
               "Extractive (E)"            = "#D6604D",
               "R&D / Knowledge-gen. (R)"  = "#4DAF4A"),
    name = "Overhead type") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Share of sector employment (%)",
       title = sprintf("Overhead Composition by Sector, %d  (Definition 2)", last_year),
       subtitle = "Sorted by total overhead share.  R = embedded R&D/scientists; E = advertising (outside home sector).",
       caption = caption_src) +
  theme_ph + theme(legend.position = "bottom")


ggsave(file.path(OUT_DIR, "v2_S1B_03_sector_typology_bars.png"),  pS1B_03, width = 10, height = 9, dpi = 300)
cat("    Saved v2_S1B_03_sector_typology_bars\n")


# S1B_04 -- Per-type sector spaghetti, 4-panel --------------------------------
cat("  S1B_04: per-type sector spaghetti, 4-panel ...\n")

econ_type_avg <- economy_trend %>%
  select(year, F = share_F, B = share_B, E = share_E, R = share_R) %>%
  pivot_longer(-year, names_to = "type_code", values_to = "avg_share") %>%
  mutate(avg_share_prop = avg_share / 100,
         type_label = recode(type_code,
                             "F" = "F: Friction-mitigating",
                             "B" = "B: Bureaucratic",
                             "E" = "E: Extractive",
                             "R" = "R: R&D / Knowledge-gen."),
         type_label = factor(type_label,
                             levels = c("F: Friction-mitigating", "B: Bureaucratic",
                                        "E: Extractive", "R: R&D / Knowledge-gen.")))

sector_long_type <- sector_year_top12 %>%
  select(naics2, sector_label, year, share_F, share_B, share_E, share_R) %>%
  pivot_longer(c(share_F, share_B, share_E, share_R),
               names_to = "type_key", values_to = "type_share") %>%
  mutate(type_label = recode(type_key,
                             "share_F" = "F: Friction-mitigating",
                             "share_B" = "B: Bureaucratic",
                             "share_E" = "E: Extractive",
                             "share_R" = "R: R&D / Knowledge-gen."),
         type_label = factor(type_label,
                             levels = c("F: Friction-mitigating", "B: Bureaucratic",
                                        "E: Extractive", "R: R&D / Knowledge-gen.")))

pS1B_04 <- ggplot(sector_long_type,
                  aes(x = year, y = type_share,
                      color = sector_label, group = sector_label)) +
  geom_line(data = econ_type_avg,
            aes(x = year, y = avg_share_prop),
            inherit.aes = FALSE, color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_line(linewidth = 0.6, alpha = 0.75) +
  facet_wrap(~ type_label, nrow = 2, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_color_manual(values = rep(scales::hue_pal()(12), 2), guide = "none") +
  labs(x = NULL, y = "Share of sector employment",
       title = "Overhead Sub-type Employment Share by Sector Over Time",
       subtitle = "Each panel = one overhead type  |  Dashed = economy-wide average  |  Top-12 sectors",
       caption = caption_src) +
  theme_ph +
  theme(strip.text = element_text(face = "bold", size = 9),
        legend.position = "none",
        panel.grid.major.x = element_blank())

ggsave(file.path(OUT_DIR, "v2_S1B_04_sector_type_spaghetti.png"),  pS1B_04, width = 13, height = 9, dpi = 300)
cat("    Saved v2_S1B_04_sector_type_spaghetti\n")


# =============================================================================
# SECTION 2  Â·  TFP (INTEGRATED)
# [Core -> Total Overhead -> F -> B -> E -> R]
# =============================================================================
#   S2_01  Core labour share growth vs Integrated TFP growth
#   S2_02  Total overhead share growth vs Integrated TFP growth
#   S2_F   F-type share growth vs Integrated TFP growth
#   S2_B   B-type share growth vs Integrated TFP growth
#   S2_E   E-type share growth vs Integrated TFP growth
#   S2_R   R-type share growth vs Integrated TFP growth
# =============================================================================

cat("\n=== SECTION 2: TFP (INTEGRATED) ===\n")

x_tfp     <- "tfp_growth"
x_tfp_lab <- sprintf("Sector Integrated TFP Growth (log pts), %d-%d", first_year, last_year)

# S2_01 -- Core vs integrated TFP ---------------------------------------------
cat("  S2_01: core vs integrated TFP ...\n")
fit_S2_01 <- lm(core_share_growth ~ tfp_growth, data = growth_df)
sub_S2_01 <- sprintf(
  "OLS R2 = %.3f  |  b = %+.4f  |  Emp-wtd avg core: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S2_01)$r.squared, coef(fit_S2_01)[["tfp_growth"]],
  weighted.mean(growth_df$core_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$core_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS2_01 <- ggplot(growth_df, aes(x = tfp_growth, y = core_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = sector_label),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.9) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_tfp_lab,
       y = sprintf("Core Labour Share Growth (pp), %d-%d", first_year, last_year),
       title = "Core Labour Accumulation vs. Sector Integrated TFP Growth",
       subtitle = sub_S2_01, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S2_01_core_vs_tfp.png"),  pS2_01, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S2_01_core_vs_tfp\n")


# S2_02 -- Total overhead vs integrated TFP -----------------------------------
cat("  S2_02: overhead vs integrated TFP ...\n")
fit_S2_02 <- lm(overhead_share_growth ~ tfp_growth, data = growth_df)
sub_S2_02 <- sprintf(
  "OLS R2 = %.3f  |  b = %+.4f  |  Emp-wtd avg overhead: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S2_02)$r.squared, coef(fit_S2_02)[["tfp_growth"]],
  weighted.mean(growth_df$ovr_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$ovr_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS2_02 <- ggplot(growth_df, aes(x = tfp_growth, y = overhead_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = sector_label),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.9) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_tfp_lab,
       y = sprintf("Overhead Share Growth (pp), %d-%d", first_year, last_year),
       title = "Overhead Accumulation vs. Sector Integrated TFP Growth",
       subtitle = sub_S2_02, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S2_02_overhead_vs_tfp.png"),  pS2_02, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S2_02_overhead_vs_tfp\n")


# S2_F / S2_B / S2_E / S2_R -- Per-type vs integrated TFP --------------------
for (tc in list(
  list(code = "F", col = "delta_F", label = "Friction-mitigating"),
  list(code = "B", col = "delta_B", label = "Bureaucratic"),
  list(code = "E", col = "delta_E", label = "Extractive"),
  list(code = "R", col = "delta_R", label = "R&D / Knowledge-gen."))) {
  cat(sprintf("  S2_%s: %s vs integrated TFP ...\n", tc$code, tc$label))
  save_type_scatter(
    df          = growth_df,
    y_col       = tc$col,
    x_col       = x_tfp,
    type_code   = tc$code,
    x_label_str = x_tfp_lab,
    y_label_str = sprintf("%s (%s) Share Growth (pp), %d-%d",
                          tc$label, tc$code, first_year, last_year),
    plot_title  = sprintf("%s (%s) Overhead Accumulation vs. Integrated TFP Growth",
                          tc$label, tc$code),
    file_stem   = sprintf("v2_S2_%s_vs_tfp", tc$code))
}


# =============================================================================
# SECTION 3  Â·  LABOUR PRODUCTIVITY
# [Core -> Total Overhead -> F -> B -> E -> R]  + 4-panel summary
# =============================================================================
#   S3_01  Core labour share growth vs Labour Productivity
#   S3_02  Total overhead share growth vs Labour Productivity
#   S3_F   F-type vs Labour Productivity
#   S3_B   B-type vs Labour Productivity
#   S3_E   E-type vs Labour Productivity
#   S3_R   R-type vs Labour Productivity (R&D spotlight)
#   S3_4P  Summary: all 4 types in one 4-panel plot
# =============================================================================

cat("\n=== SECTION 3: LABOUR PRODUCTIVITY ===\n")

x_lp     <- "tfp_l_growth"
x_lp_lab <- sprintf("Sector Labour Productivity Growth (log pts), %d-%d", first_year, last_year)

# S3_01 -- Core vs labour productivity ----------------------------------------
cat("  S3_01: core vs labour productivity ...\n")
fit_S3_01 <- lm(core_share_growth ~ tfp_l_growth, data = growth_df)
sub_S3_01 <- sprintf(
  "OLS R2 = %.3f  |  b = %+.4f  |  Emp-wtd avg core: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S3_01)$r.squared, coef(fit_S3_01)[["tfp_l_growth"]],
  weighted.mean(growth_df$core_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$core_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS3_01 <- ggplot(growth_df, aes(x = tfp_l_growth, y = core_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = sector_label),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.9) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_lp_lab,
       y = sprintf("Core Labour Share Growth (pp), %d-%d", first_year, last_year),
       title = "Core Labour Accumulation vs. Sector Labour Productivity Growth",
       subtitle = sub_S3_01, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S3_01_core_vs_laborprod.png"),  pS3_01, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S3_01_core_vs_laborprod\n")


# S3_02 -- Total overhead vs labour productivity ------------------------------
cat("  S3_02: overhead vs labour productivity ...\n")
fit_S3_02 <- lm(overhead_share_growth ~ tfp_l_growth, data = growth_df)
sub_S3_02 <- sprintf(
  "OLS R2 = %.3f  |  b = %+.4f  |  Emp-wtd avg overhead: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S3_02)$r.squared, coef(fit_S3_02)[["tfp_l_growth"]],
  weighted.mean(growth_df$ovr_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$ovr_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS3_02 <- ggplot(growth_df, aes(x = tfp_l_growth, y = overhead_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = sector_label),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.9) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_lp_lab,
       y = sprintf("Overhead Share Growth (pp), %d-%d", first_year, last_year),
       title = "Overhead Accumulation vs. Sector Labour Productivity Growth",
       subtitle = sub_S3_02, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S3_02_overhead_vs_laborprod.png"),  pS3_02, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S3_02_overhead_vs_laborprod\n")


# S3_F / S3_B / S3_E -- F, B, E individual vs labour productivity ----------------
for (tc in list(
  list(code = "F", col = "delta_F", label = "Friction-mitigating"),
  list(code = "B", col = "delta_B", label = "Bureaucratic"),
  list(code = "E", col = "delta_E", label = "Extractive"))) {
  cat(sprintf("  S3_%s: %s vs labour productivity ...\n", tc$code, tc$label))
  save_type_scatter(
    df          = growth_df,
    y_col       = tc$col,
    x_col       = x_lp,
    type_code   = tc$code,
    x_label_str = x_lp_lab,
    y_label_str = sprintf("%s (%s) Share Growth (pp), %d-%d",
                          tc$label, tc$code, first_year, last_year),
    plot_title  = sprintf("%s (%s) Overhead Accumulation vs. Labour Productivity Growth",
                          tc$label, tc$code),
    file_stem   = sprintf("v2_S3_%s_vs_laborprod", tc$code))
}


# S3_R -- R-type vs labour productivity (R&D spotlight) -----------------------
# Romer/Jones & Williams: R-type should correlate POSITIVELY with productivity.
# Cohen & Levinthal (1990): absorptive capacity drives TFP catch-up.
cat("  S3_R: R-type vs labour productivity (R&D spotlight) ...\n")
if (any(!is.na(growth_df$delta_R) & !is.na(growth_df[[x_lp]]))) {
  fit_R <- lm(delta_R ~ tfp_l_growth,
              data = growth_df %>% filter(!is.na(delta_R)))
  sub_R <- sprintf(
    "OLS R2 = %.3f  |  b = %+.4f\nRomer (1990) / Jones & Williams (2000): b > 0 (growing sectors attract more embedded scientists)",
    summary(fit_R)$r.squared, coef(fit_R)[["tfp_l_growth"]])

  pS3_R <- ggplot(growth_df %>% filter(!is.na(delta_R)),
                  aes(x = tfp_l_growth, y = delta_R * 100)) +
    geom_smooth(method = "lm", se = TRUE,
                color = type_colors["R"], fill = "#b3e6b3",
                linewidth = 1, alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    geom_point(aes(size = emp_base / 1e3), shape = 21,
               fill = type_colors["R"], color = "gray20", stroke = 0.4, alpha = 0.9) +
    geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                    segment.color = "gray60", segment.size = 0.3,
                    box.padding = 0.3, min.segment.length = 0.2) +
    scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                          breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
    scale_y_continuous(labels = function(x) paste0(round(x, 2), " pp")) +
    labs(
      x       = x_lp_lab,
      y       = sprintf("R-type Share Growth (pp), %d-%d", first_year, last_year),
      title   = "R&D / Knowledge-Generating Overhead vs. Labour Productivity Growth",
      subtitle = sub_R,
      caption  = paste0(
        caption_src, "\n",
        "R-type = life/physical/social scientists & science technicians (SOC 19-x) outside home sector.\n",
        "Per Cohen & Levinthal (1990), positive b supports the absorptive-capacity channel."
      )
    ) +
    theme_ph

    ggsave(file.path(OUT_DIR, "v2_S3_R_vs_laborprod.png"),  pS3_R, width = 11, height = 7.5, dpi = 300)
  cat("    Saved v2_S3_R_vs_laborprod\n")
} else {
  cat("    S3_R skipped: insufficient R-type data across sectors.\n")
}


# S3_4P -- Summary: all 4 types vs labour productivity, 4-panel ---------------
# Predictions: F ~ 0  |  B < 0  |  E < 0 (most negative)  |  R > 0
cat("  S3_4P: 4-panel summary (F/B/E/R vs labour productivity) ...\n")

type_panel_df <- growth_df %>%
  select(naics2, sector_label, emp_base, tfp_l_growth,
         delta_F, delta_B, delta_E, delta_R) %>%
  pivot_longer(c(delta_F, delta_B, delta_E, delta_R),
               names_to = "type_key", values_to = "delta_share") %>%
  mutate(
    oh_type = recode(type_key,
                     delta_F = "F: Friction-mitigating",
                     delta_B = "B: Bureaucratic",
                     delta_E = "E: Extractive",
                     delta_R = "R: R&D / Knowledge-gen."),
    oh_type = factor(oh_type,
                     levels = c("F: Friction-mitigating", "B: Bureaucratic",
                                "E: Extractive", "R: R&D / Knowledge-gen.")))

r2_by_type <- type_panel_df %>%
  group_by(oh_type) %>%
  filter(!is.na(delta_share), !is.na(tfp_l_growth)) %>%
  summarise(
    r2    = summary(lm(delta_share ~ tfp_l_growth))$r.squared,
    slope = coef(lm(delta_share ~ tfp_l_growth))[["tfp_l_growth"]],
    .groups = "drop") %>%
  mutate(label = sprintf("R2 = %.3f,  b = %+.4f", r2, slope))

type_panel_df <- type_panel_df %>%
  left_join(r2_by_type %>% select(oh_type, label), by = "oh_type")

pS3_4P <- ggplot(type_panel_df, aes(x = tfp_l_growth, y = delta_share * 100)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray88",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3), shape = 21, fill = "steelblue",
             color = "gray20", stroke = 0.4, alpha = 0.85) +
  geom_text_repel(aes(label = sector_label), size = 2.3, max.overlaps = 18,
                  segment.color = "gray60", segment.size = 0.3, box.padding = 0.3) +
  geom_text(data = r2_by_type,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.08, vjust = 1.5, size = 2.9, color = "gray30",
            inherit.aes = FALSE) +
  facet_wrap(~ oh_type, nrow = 2, ncol = 2, scales = "free_y") +
  scale_size_continuous(range = c(1.5, 8), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), " pp")) +
  labs(
    x       = "Sector Labour Productivity Growth (log pts)",
    y       = "Overhead Sub-type Share Change (pp)",
    title   = "Overhead Type Accumulation vs. Labour Productivity Growth  [Summary]",
    subtitle = paste0(
      "Theoretical predictions: F ~ 0  |  B < 0  |  E < 0 (most negative)  |  R > 0 (Romer/Cohen-Levinthal)\n",
      "Point size proportional to base-year employment"),
    caption = caption_src) +
  theme_ph +
  theme(strip.text = element_text(face = "bold", size = 9.5))

ggsave(file.path(OUT_DIR, "v2_S3_4P_type_vs_laborprod.png"),  pS3_4P, width = 13, height = 10, dpi = 300)
cat("    Saved v2_S3_4P_type_vs_laborprod\n")


# =============================================================================
# SECTION 4  Â·  VALUE ADDED (SHIFT-SHARE)
# [Core -> Total Overhead -> F -> B -> E -> R]
# =============================================================================
#   S4_01  Core labour share growth vs VA growth
#   S4_02  Total overhead share growth vs VA growth
#   S4_F   F-type vs VA growth
#   S4_B   B-type vs VA growth
#   S4_E   E-type vs VA growth
#   S4_R   R-type vs VA growth
# =============================================================================

cat("\n=== SECTION 4: VALUE ADDED ===\n")

x_va     <- "va_growth"
x_va_lab <- sprintf("Sector VA Growth (log pts), %d-%d", first_year, last_year)

# S4_01 -- Core vs VA growth --------------------------------------------------
cat("  S4_01: core vs VA growth ...\n")
fit_S4_01 <- lm(core_share_growth ~ va_growth, data = growth_df)
sub_S4_01 <- sprintf(
  "OLS R2 = %.3f  |  Emp-wtd avg core: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S4_01)$r.squared,
  weighted.mean(growth_df$core_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$core_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS4_01 <- ggplot(growth_df, aes(x = va_growth, y = core_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = residual_core),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.85) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, name = "Shift-share\nresidual (pp)",
                       labels = function(x) round(x * 100, 1)) +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_va_lab,
       y = sprintf("Core Labour Share Growth (pp), %d-%d", first_year, last_year),
       title = "Core Labour Accumulation vs. Sector Value-Added Growth",
       subtitle = sub_S4_01, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S4_01_core_vs_va.png"),  pS4_01, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S4_01_core_vs_va\n")


# S4_02 -- Total overhead vs VA growth ----------------------------------------
cat("  S4_02: overhead vs VA growth ...\n")
fit_S4_02 <- lm(overhead_share_growth ~ va_growth, data = growth_df)
sub_S4_02 <- sprintf(
  "OLS R2 = %.3f  |  Emp-wtd avg overhead: %.1f%% (%d) -> %.1f%% (%d)",
  summary(fit_S4_02)$r.squared,
  weighted.mean(growth_df$ovr_base, growth_df$emp_base, na.rm = TRUE) * 100, first_year,
  weighted.mean(growth_df$ovr_last, growth_df$emp_base, na.rm = TRUE) * 100, last_year)

pS4_02 <- ggplot(growth_df, aes(x = va_growth, y = overhead_share_growth)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(aes(size = emp_base / 1e3, fill = residual_overhead),
             shape = 21, color = "gray20", stroke = 0.4, alpha = 0.85) +
  geom_text_repel(aes(label = sector_label), size = 2.6, max.overlaps = 30,
                  segment.color = "gray60", segment.size = 0.3,
                  box.padding = 0.3, min.segment.length = 0.2) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, name = "Shift-share\nresidual (pp)",
                       labels = function(x) round(x * 100, 1)) +
  scale_size_continuous(range = c(2, 12), name = "Base-yr emp (000s)",
                        breaks = c(500, 2000, 5000, 10000), labels = scales::comma) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), " pp")) +
  labs(x = x_va_lab,
       y = sprintf("Overhead Share Growth (pp), %d-%d", first_year, last_year),
       title = "Overhead Accumulation vs. Sector Value-Added Growth",
       subtitle = sub_S4_02, caption = caption_src) +
  theme_ph

ggsave(file.path(OUT_DIR, "v2_S4_02_overhead_vs_va.png"),  pS4_02, width = 11, height = 7.5, dpi = 300)
cat("    Saved v2_S4_02_overhead_vs_va\n")


# S4_F / S4_B / S4_E / S4_R -- Per-type vs VA growth -------------------------
for (tc in list(
  list(code = "F", col = "delta_F", label = "Friction-mitigating"),
  list(code = "B", col = "delta_B", label = "Bureaucratic"),
  list(code = "E", col = "delta_E", label = "Extractive"),
  list(code = "R", col = "delta_R", label = "R&D / Knowledge-gen."))) {
  cat(sprintf("  S4_%s: %s vs VA growth ...\n", tc$code, tc$label))
  save_type_scatter(
    df          = growth_df,
    y_col       = tc$col,
    x_col       = x_va,
    type_code   = tc$code,
    x_label_str = x_va_lab,
    y_label_str = sprintf("%s (%s) Share Growth (pp), %d-%d",
                          tc$label, tc$code, first_year, last_year),
    plot_title  = sprintf("%s (%s) Overhead Accumulation vs. Sector Value-Added Growth",
                          tc$label, tc$code),
    file_stem   = sprintf("v2_S4_%s_vs_va", tc$code))
}


# =============================================================================
# STEP G  Save outputs
# =============================================================================

write.csv(sector_year,
          file.path(OUT_DIR, "v2_sector_year_panel.csv"),
          row.names = FALSE)

write.csv(growth_df,
          file.path(OUT_DIR, "v2_growth_df.csv"),
          row.names = FALSE)

occ_lookup <- oews_panel %>%
  distinct(OCC_CODE, OCC_TITLE, soc_minor, naics2,
           overhead_def2, oh_type_final, oh_type_label) %>%
  arrange(OCC_CODE)

write.csv(occ_lookup,
          file.path(OUT_DIR, "v2_occupation_classification_lookup.csv"),
          row.names = FALSE)

cat("\n=== ALL DONE ===\n")
cat("Outputs written to:", OUT_DIR, "\n")
cat("\n--- SECTION 1: Employment Share Trends ---\n")
cat("  v2_S1A_01_core_trend.*             -- core employment share over time\n")
cat("  v2_S1A_02_overhead_trend.*         -- total overhead share over time\n")
cat("  v2_S1A_03_typology_stacked.*       -- F/B/E/R stacked area, economy-wide\n")
cat("  v2_S1A_04_type_trends.*            -- per-type individual trends, 4-panel\n")
cat("  v2_S1B_01_core_sector.*            -- core sector spaghetti\n")
cat("  v2_S1B_02_overhead_sector.*        -- overhead sector spaghetti\n")
cat("  v2_S1B_03_sector_typology_bars.*   -- sector F/B/E/R bars (latest year)\n")
cat("  v2_S1B_04_sector_type_spaghetti.*  -- per-type sector spaghetti, 4-panel\n")
cat("\n--- SECTION 2: TFP (Integrated) ---\n")
cat("  v2_S2_01_core_vs_tfp.*             -- core vs integrated TFP growth\n")
cat("  v2_S2_02_overhead_vs_tfp.*         -- overhead vs integrated TFP growth\n")
cat("  v2_S2_F_vs_tfp.*                   -- F-type vs integrated TFP growth\n")
cat("  v2_S2_B_vs_tfp.*                   -- B-type vs integrated TFP growth\n")
cat("  v2_S2_E_vs_tfp.*                   -- E-type vs integrated TFP growth\n")
cat("  v2_S2_R_vs_tfp.*                   -- R-type vs integrated TFP growth\n")
cat("\n--- SECTION 3: Labour Productivity ---\n")
cat("  v2_S3_01_core_vs_laborprod.*       -- core vs labour productivity growth\n")
cat("  v2_S3_02_overhead_vs_laborprod.*   -- overhead vs labour productivity growth\n")
cat("  v2_S3_F_vs_laborprod.*             -- F-type vs labour productivity\n")
cat("  v2_S3_B_vs_laborprod.*             -- B-type vs labour productivity\n")
cat("  v2_S3_E_vs_laborprod.*             -- E-type vs labour productivity\n")
cat("  v2_S3_R_vs_laborprod.*             -- R-type vs labour productivity (R&D spotlight)\n")
cat("  v2_S3_4P_type_vs_laborprod.*       -- 4-panel summary: all types vs LabProd\n")
cat("\n--- SECTION 4: Value Added ---\n")
cat("  v2_S4_01_core_vs_va.*              -- core vs VA growth\n")
cat("  v2_S4_02_overhead_vs_va.*          -- overhead vs VA growth\n")
cat("  v2_S4_F_vs_va.*                    -- F-type vs VA growth\n")
cat("  v2_S4_B_vs_va.*                    -- B-type vs VA growth\n")
cat("  v2_S4_E_vs_va.*                    -- E-type vs VA growth\n")
cat("  v2_S4_R_vs_va.*                    -- R-type vs VA growth\n")
cat("\n--- Data outputs ---\n")
cat("  v2_sector_year_panel.csv\n")
cat("  v2_growth_df.csv\n")
cat("  v2_occupation_classification_lookup.csv\n")

# APPENDIX: robustness comparison with Def 3 and Def 4
# (Def 2 is primary; these are for appendix tables only)
# =============================================================================

cat("\n--- Loading Def 3 (O*NET GWA) for robustness ---\n")

gwa_class <- tribble(
  ~element_id, ~task_class,
  "4.A.1.a.1","AMBIGUOUS","4.A.1.a.2","OVERHEAD","4.A.1.b.1","OVERHEAD",
  "4.A.1.b.2","OVERHEAD","4.A.1.b.3","AMBIGUOUS","4.A.2.a.1","AMBIGUOUS",
  "4.A.2.a.2","AMBIGUOUS","4.A.2.a.3","OVERHEAD","4.A.2.a.4","AMBIGUOUS",
  "4.A.2.b.1","AMBIGUOUS","4.A.2.b.2","PRODUCTION","4.A.2.b.3","AMBIGUOUS",
  "4.A.2.b.4","OVERHEAD","4.A.2.b.5","OVERHEAD","4.A.2.b.6","OVERHEAD",
  "4.A.2.b.7","OVERHEAD","4.A.3.a.1","PRODUCTION","4.A.3.a.2","AMBIGUOUS",
  "4.A.3.a.3","OVERHEAD","4.A.3.a.4","PRODUCTION","4.A.3.b.1","PRODUCTION",
  "4.A.3.b.2","PRODUCTION","4.A.3.b.3","PRODUCTION","4.A.3.b.4","PRODUCTION",
  "4.A.3.b.5","PRODUCTION","4.A.3.b.6","PRODUCTION","4.A.4.a.1","AMBIGUOUS",
  "4.A.4.a.2","AMBIGUOUS","4.A.4.a.3","OVERHEAD","4.A.4.a.4","OVERHEAD",
  "4.A.4.a.5","PRODUCTION","4.A.4.a.6","AMBIGUOUS","4.A.4.a.7","OVERHEAD",
  "4.A.4.a.8","PRODUCTION","4.A.4.b.1","OVERHEAD","4.A.4.b.2","OVERHEAD",
  "4.A.4.b.3","AMBIGUOUS","4.A.4.b.4","OVERHEAD","4.A.4.b.5","OVERHEAD",
  "4.A.4.b.6","AMBIGUOUS","4.A.4.c.1","OVERHEAD","4.A.4.c.2","OVERHEAD",
  "4.A.4.c.3","OVERHEAD"
)

onet_raw <- tryCatch(
  read_xlsx(ONET_FILE) %>%
    rename(occ_code_onet = `O*NET-SOC Code`,
           element_id    = `Element ID`,
           scale_id      = `Scale ID`,
           data_value    = `Data Value`),
  error = function(e) { cat("  O*NET file not found â€” Def 3 skipped.\n"); NULL }
)

if (!is.null(onet_raw)) {
  onet_primary <- onet_raw %>%
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
    select(OCC_CODE, overhead_def3) %>%
    distinct()

  oews_panel <- oews_panel %>%
    left_join(onet_primary, by = "OCC_CODE")

  # Agreement table (Def 2 vs Def 3)
  cat("\nDef 2 vs Def 3 agreement (occupation level):\n")
  oews_panel %>%
    filter(!is.na(overhead_def2), !is.na(overhead_def3)) %>%
    count(overhead_def2, overhead_def3) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    print()
}

def4_lookup <- tryCatch({
  read_xlsx(TASK_CLASS_FILE, sheet = "Occupation Summary") %>%
    rename(occ_code_onet = `O*NET-SOC Code`,
           occ_class_d4  = occ_class,
           oh_share_d4   = occ_oh_share) %>%
    mutate(
      OCC_CODE      = str_remove(occ_code_onet, "\\.\\d+$"),
      overhead_def4 = if_else(occ_class_d4 == "OVERHEAD", 1L, 0L),
      overhead_share_d4 = suppressWarnings(as.numeric(oh_share_d4))
    ) %>%
    select(OCC_CODE, overhead_def4, overhead_share_d4) %>%
    group_by(OCC_CODE) %>%
    slice_max(order_by = overhead_share_d4, n = 1, with_ties = FALSE) %>%
    ungroup()
}, error = function(e) {
  cat("  Def 4 file not found â€” skipped.\n")
  tibble(OCC_CODE = character(), overhead_def4 = integer(),
         overhead_share_d4 = double())
})

if (nrow(def4_lookup) > 0) {
  oews_panel <- oews_panel %>%
    left_join(def4_lookup, by = "OCC_CODE")

  cat("\nDef 2 vs Def 4 agreement (occupation level):\n")
  oews_panel %>%
    filter(!is.na(overhead_def2), !is.na(overhead_def4)) %>%
    count(overhead_def2, overhead_def4) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    print()
}

cat("\nAppendix robustness section complete.\n")
