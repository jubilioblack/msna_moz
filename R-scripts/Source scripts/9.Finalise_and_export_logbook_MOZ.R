# =================================================================================================
# REACH MSNA 2026 Mozambique - 9. Remove PII / metadata and export the logbook for submission
# -------------------------------------------------------------------------------------------------
# Replaces, for MOZ, the Uganda scripts "9. Remove the unnecessary variables.R" and
# "10.Export the logbook.R" (those used Uganda dataset names and Uganda FSL variable names).
#
# Required objects: raw_data_list (script 1), clean_* datasets and cleaning_log_reviewed /
# deletion_log (script 8; weights optional), df_survey, df_choices
# Output: outputs/Logbook for submission/MSNA2026_MOZ_logbook_<date>.xlsx
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(stringr); library(writexl); library(tibble) })
if (exists("print_step", mode = "function")) print_step("9. Removing PII / metadata and exporting the logbook (MOZ)")

# Variables that identify people, enumerators, devices or exact locations. Most are not in the
# current MOZ export, but are listed so the script stays safe if a fuller Kobo export is used.
pii_cols <- c("enum_code", "enumerator_name", "enum_phone_number", "deviceid", "audit", "username",
              "household_geopoint", "_household_geopoint_latitude", "_household_geopoint_longitude",
              "_household_geopoint_altitude", "_household_geopoint_precision",
              "current_lat", "current_long", "first_lat", "first_long", "first_gps", "geotrace", "distance",
              "latitude", "longitude", "altitude", "accuracy", "geofence_result")
# Technical columns created during cleaning (not part of the dataset to share)
technical_cols <- c("hh_uuid", ".row_key", "percentage_missing", "percentage_missing_msna", "log_created_at",
                    "submission_uuid", "submission_id", "repeat_row_uuid", "check_binding")
# Notes are never data
note_cols <- df_survey |> filter(str_detect(type, "^note")) |> pull(name)

drop_cols <- function(df) {
  if (!is.data.frame(df)) return(df)
  df |> select(-any_of(c(pii_cols, technical_cols, note_cols)))
}

datasets_out <- c(Submissions = "clean_main", roster = "clean_roster", edu_ind = "clean_edu_ind",
                  health_ind = "clean_health_ind", nutrition_ind = "clean_nutrition_ind", health_wra = "clean_health_wra",
                  prot_sep_child = "clean_prot_sep_child", prot_hosted_child = "clean_prot_hosted_child")

variable_tracker <- imap_dfr(datasets_out, function(obj, ds) {
  if (!exists(obj)) return(NULL)
  before <- names(get(obj))
  after <- names(drop_cols(get(obj)))
  removed <- setdiff(before, after)
  if (!length(removed)) return(NULL)
  tibble(dataset = ds, variable = removed,
         reason = case_when(removed %in% pii_cols ~ "PII / exact location / enumerator identifier",
                            removed %in% note_cols ~ "Kobo note (no data)",
                            TRUE ~ "Technical column created during cleaning"))
})

raw_out <- map(raw_data_list, drop_cols)
clean_out <- map(datasets_out, ~ if (exists(.x)) drop_cols(get(.x)) else NULL) |> compact()

dir.create("outputs/Logbook for submission", showWarnings = FALSE, recursive = TRUE)
logbook_path <- file.path("outputs/Logbook for submission", paste0("MSNA2026_MOZ_logbook_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))

sheets <- c(
  set_names(raw_out, paste0("raw_", substr(names(raw_out), 1, 27))),
  set_names(clean_out, paste0("clean_", substr(names(clean_out), 1, 25))),
  list(
    cleaning_log = if (exists("cleaning_log_reviewed")) cleaning_log_reviewed |> filter(change_type != "remove_survey") |> select(-any_of(c(technical_cols, "source_file"))) else tibble(),
    deletion_log = if (exists("deletion_log")) deletion_log else tibble(),
    variable_tracker = if (nrow(variable_tracker)) variable_tracker else tibble(note = "no variables removed"),
    survey = df_survey, choices = df_choices
  )
)
write_xlsx(sheets, logbook_path)
cat("Logbook for submission exported:", logbook_path, "\n")
