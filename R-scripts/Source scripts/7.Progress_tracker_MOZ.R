# =================================================================================================
# REACH MSNA 2026 Mozambique - 7. Data collection progress tracker
# -------------------------------------------------------------------------------------------------
# Replaces the Uganda "7.Target_progress_and_daily_rate_tracker.R" (refugee settlements / host
# districts, 150/106 targets, 3 interviews per enumerator per day). Those rules do not apply to MOZ,
# and the MOZ export has no interview date, so daily rates cannot be computed.
#
# What it does
#   - Counts completed interviews by province, district and administrative post x population group.
#   - If inputs/REACH_MOZ_MSNA2026_sampling_targets.xlsx exists (columns: admin2, population_group,
#     target), adds target, remaining interviews and % achieved per district x population group.
#     admin2 must be the district p-code used in the tool (e.g. MZ0101); population_group one of
#     idp / returnees / non_displaced.
#   - Adds flags-per-interview from the cleaning log (script 3) to spot districts needing follow-up.
#
# Output: <daily output folder>/MSNA2026_MOZ_progress_tracker_<date>.xlsx
# Required objects: data_hh_main, path_cleaning_folder_today; optional: combined_log_df
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(openxlsx) })

if (exists("print_step", mode = "function")) print_step("7. MOZ progress tracker") else cat("\n7. MOZ progress tracker\n")

path_targets <- if (exists("path_targets")) path_targets else "inputs/REACH_MOZ_MSNA2026_sampling_targets.xlsx"

hh <- data_hh_main |>
  mutate(across(any_of(c("admin1", "admin2", "admin3", "population_group")), as.character))

count_by <- function(df, ...) {
  df |>
    count(..., population_group, name = "n") |>
    pivot_wider(names_from = population_group, values_from = n, values_fill = 0) |>
    mutate(total = rowSums(across(any_of(c("idp", "returnees", "non_displaced"))))) |>
    relocate(any_of(c("idp", "returnees", "non_displaced")), .before = total)
}

by_province <- count_by(hh, admin1, admin1_name) |> arrange(admin1)
by_district <- count_by(hh, admin1_name, admin2, admin2_name) |> arrange(admin1_name, admin2_name)
by_posto <- count_by(hh, admin1_name, admin2_name, admin3, admin3_name) |> arrange(admin1_name, admin2_name, admin3_name)
by_setting <- count_by(hh, admin2_name, setting) |> arrange(admin2_name, setting)

# Flags per interview by district (from script 3)
if (exists("combined_log_df") && is.data.frame(combined_log_df) && nrow(combined_log_df) > 0) {
  flags_district <- combined_log_df |>
    filter(dataset == "Submissions" | !is.na(uuid)) |>
    distinct(uuid, check_id) |>
    left_join(select(hh, uuid, admin2), by = "uuid") |>
    count(admin2, name = "n_flags")
  by_district <- by_district |>
    left_join(flags_district, by = "admin2") |>
    mutate(n_flags = coalesce(n_flags, 0L), flags_per_interview = round(n_flags / pmax(total, 1), 2))
}

# Targets (optional)
progress_vs_target <- NULL
if (file.exists(path_targets)) {
  targets <- readxl::read_excel(path_targets) |>
    janitor::clean_names() |>
    mutate(admin2 = as.character(admin2), population_group = as.character(population_group), target = as.numeric(target))
  achieved <- hh |> count(admin2, population_group, name = "achieved")
  progress_vs_target <- targets |>
    full_join(achieved, by = c("admin2", "population_group")) |>
    left_join(distinct(hh, admin2, admin2_name, admin1_name), by = "admin2") |>
    mutate(achieved = coalesce(achieved, 0L), remaining = pmax(target - achieved, 0),
           pct_achieved = round(100 * achieved / target, 1)) |>
    relocate(admin1_name, admin2, admin2_name) |>
    arrange(admin1_name, admin2_name, population_group)
} else {
  message("   No targets file at ", path_targets, " - the tracker shows completed interviews only.")
}

readme <- tibble(
  sheet = c("by_province", "by_district", "by_posto", "by_setting", "vs_target"),
  content = c(
    "Completed interviews by province and population group.",
    "Completed interviews by district and population group; n_flags = distinct checks flagged (script 3).",
    "Completed interviews by administrative post and population group.",
    "Completed interviews by district and setting (rural / urban / peri-urban / camp).",
    paste0("Only when ", path_targets, " exists: target, achieved, remaining and % achieved by district x population group.")
  )
)

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#EE5859", fontColour = "white")
for (nm in c("README", "by_province", "by_district", "by_posto", "by_setting", if (!is.null(progress_vs_target)) "vs_target")) {
  df <- switch(nm, README = readme, by_province = by_province, by_district = by_district, by_posto = by_posto,
               by_setting = by_setting, vs_target = progress_vs_target)
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = hs)
  freezePane(wb, nm, firstRow = TRUE)
  setColWidths(wb, nm, cols = seq_along(df), widths = "auto")
}
progress_tracker_path <- file.path(path_cleaning_folder_today, paste0("MSNA2026_MOZ_progress_tracker_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
saveWorkbook(wb, progress_tracker_path, overwrite = TRUE)
cat("Progress tracker written:", progress_tracker_path, "\n")
