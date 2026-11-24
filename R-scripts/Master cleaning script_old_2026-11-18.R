# =================================================================================================
# REACH MSNA 2026 Mozambique - Master cleaning script
# -------------------------------------------------------------------------------------------------
# Adapted from the MSNA 2026 Uganda pipeline. Open MSNA_cleaning_MOZ.Rproj (working directory =
# project root) and run this script top to bottom.
#
# What changed for Mozambique (see README_MOZ.md for details):
#   - Step 0 converts the labelled data export (question labels as headers, choice labels as values)
#     back to Kobo XML names/codes using the MOZ tool. Everything after step 0 uses XML names.
#   - Logical checks are MOZ-specific (inputs/REACH_MOZ_MSNA2026_logical_checks.xlsx), built by 1b.
#   - Datasets: Submissions (main), roster, edu_ind, health_ind, nutrition_ind (nut_ind), health_wra,
#     prot_sep_child (prot_separated_children_breakdown), prot_hosted_child
#     (prot_hosted_separated_children_breakdown). There is no child_protection_ind loop in MOZ.
#   - Context columns in the cleaning logs: admin1/2/3 (p-codes) + admin names, setting,
#     population_group (idp / returnees / non_displaced). point_number = Kobo submission _id.
#   - The export has no start/end/submission time, enumerator code or GPS: duration checks, daily
#     date-based trackers and enumerator-level checks cannot run on this file (see README_MOZ.md).
# =================================================================================================

rm(list = ls())
Sys.setlocale("LC_TIME", "English")
options(scipen = 999)

# ---- Packages ----
required_packages <- c("tidyverse", "readxl", "openxlsx", "writexl", "lubridate", "fs", "crayon", "janitor",
                       "stringr", "purrr", "rlang", "tidyselect", "cleaningtools")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Please install required package: ", pkg, " (see R-scripts/00_install_packages.R)", call. = FALSE)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

source("R-scripts/Source scripts/my_functions.R")
source("R-scripts/Source scripts/Data cleaning - My functions.R")

# ---- Project folders ----
folders <- c("inputs", "inputs/feedback", "outputs", "R-scripts", "R-scripts/Source scripts", "archive")
for (folder in folders) if (!fs::dir_exists(folder)) fs::dir_create(folder)

path_cleaning_folder_today <- file.path("outputs", paste0("MSNA2026 Mozambique data cleaning ", format(Sys.Date(), format = "%Y-%b-%d")))
if (!fs::dir_exists(path_cleaning_folder_today)) fs::dir_create(path_cleaning_folder_today)

# ---- Input paths: edit only these if your file names differ ----
path_tool <- "inputs/REACH_MOZ_MSNA2026_Kobo_tool.xlsx"

# Labelled export received from the data team (.xlsx) - or a folder with one CSV per sheet if the
# .xlsx is too large to open in R on your computer (see script 0).
path_data_labelled <- "inputs/REACH_MOZ_Data_MSNA_01102026.xlsx"

# Outputs of step 0 (converted to XML names). path_data is what the rest of the pipeline reads.
path_data_xml       <- "inputs/REACH_MOZ_MSNA2026_data_xml.xlsx"
path_data_xml_rds   <- "inputs/REACH_MOZ_MSNA2026_data_xml.rds"
path_conversion_log <- "inputs/REACH_MOZ_MSNA2026_data_xml_conversion_log.xlsx"
path_data <- path_data_xml_rds

path_checks <- "inputs/REACH_MOZ_MSNA2026_logical_checks.xlsx"
rebuild_logical_checks <- FALSE   # TRUE after editing checks in script 1b

# Optional: _uuid of pilot/test submissions to remove from all sheets.
pilot_submission_uuids <- character(0)
test_submission_user_pattern <- NULL

# =================================================================================================
# DATA CHECKS AND CLEANING LOGS
# =================================================================================================

# 0. Convert the labelled export to XML names/codes (skipped when the converted .rds is up to date)
source("R-scripts/Source scripts/0.Convert_labelled_export_to_xml_MOZ.R")

# 1b. Build the MOZ logical-checks workbook (only if missing or rebuild requested)
if (rebuild_logical_checks || !file.exists(path_checks)) {
  source("R-scripts/Source scripts/1b.Build_MOZ_logical_checks_workbook.R")
}

# 1. Load the converted export, tool and logical checks
source("R-scripts/Source scripts/1.Load and process the data.R")

# 2. Soft duplicate checks on household submissions
source("R-scripts/Source scripts/2.Soft duplicate checks.R")

# 3. Run checks (missingness, others, special values, outliers, logical checks) on all sheets
source("R-scripts/Source scripts/3.Do the checks.R")

# 4. Process cleaning logs (PII removal, previous flags, deletion log)
source("R-scripts/Source scripts/4.Process the CLs and DFs.R")

# 5. Export one cleaning-log workbook per district (admin2) for review
source("R-scripts/Source scripts/5. Export the logbooks.R")

# 6. Specify-other review log
source("R-scripts/Source scripts/6.Specify_other_checks.R")

# 7. Data collection progress by province/district/population group (MOZ)
source("R-scripts/Source scripts/7.Progress_tracker_MOZ.R")

# =================================================================================================
# REVIEW OF THE CLEANING (after the field team has filled the logbooks)
# =================================================================================================
# Copy the reviewed district logbooks and specify-other logs (any sub-folder structure) into
# inputs/feedback/. Steps 0-1 must have been run in the same session (raw_data_list).

# path_feedback <- "inputs/feedback"
# source("R-scripts/Source scripts/8.Load_feedback_and_create_clean_data_MOZ.R")
# source("R-scripts/Source scripts/computing_weights.R")      # needs inputs/REACH_MOZ_MSNA2026_population_frame.xlsx
# source("R-scripts/Source scripts/9.Finalise_and_export_logbook_MOZ.R")
