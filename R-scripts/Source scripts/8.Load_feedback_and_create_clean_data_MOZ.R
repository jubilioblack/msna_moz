# =================================================================================================
# REACH MSNA 2026 Mozambique - 8. Load reviewed cleaning logs and create the clean datasets
# -------------------------------------------------------------------------------------------------
# Replaces, for MOZ, the Uganda scripts "Loading cleaning log data.R", "7. Review the cleaning log.R"
# and "8.Generate the clean df and review the cleaning.R" (those depend on Uganda folder names,
# the child_protection_ind loop, Kampala and date/duration fields that the MOZ export does not have).
#
# Steps
#   1. Read every reviewed workbook under path_feedback (recursively): the "cleaning_log" sheet of the
#      district logbooks (script 5) and the "other_responses" sheet of the reviewed other-responses
#      workbook (script 6), converted to cleaning-log rows (TRUE / EXISTING / INVALID decisions).
#   2. Keep rows with a valid change_type, harmonise columns, remove exact duplicates and report
#      conflicts (same uuid/dataset/repeat/question with different new values).
#   3. remove_survey on the household sheet removes the household and all its repeat rows.
#   4. For each dataset: cleaningtools::create_clean_data(), recreate_parent_column() for
#      select_multiple questions, review_cleaning() to confirm every change was applied.
#   5. Export clean data, the combined cleaning log, deletion log and review in one workbook.
#
# Required objects: raw_data_list (script 1), df_survey, df_choices, path_cleaning_folder_today
# Optional: path_feedback (default "inputs/feedback")
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(stringr); library(readxl); library(writexl) })

if (exists("print_step", mode = "function")) print_step("8. Loading reviewed cleaning logs and creating clean data (MOZ)")

path_feedback <- if (exists("path_feedback")) path_feedback else "inputs/feedback"
valid_change_types <- c("change_response", "blank_response", "remove_survey", "no_action")
dataset_names <- c("Submissions", "roster", "edu_ind", "health_ind", "nutrition_ind", "health_wra", "prot_sep_child", "prot_hosted_child")

# -------------------------------------------------------------------------------------------------
# 1. Read reviewed logs
# -------------------------------------------------------------------------------------------------
feedback_files <- list.files(path_feedback, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
feedback_files <- feedback_files[!str_detect(basename(feedback_files), "^~\\$")]
if (length(feedback_files) == 0) stop("No reviewed cleaning logs (.xlsx) found in ", path_feedback, call. = FALSE)

read_reviewed_log <- function(f) {
  sh <- excel_sheets(f)
  sh <- sh[tolower(trimws(sh)) == "cleaning_log"]
  if (!length(sh)) return(NULL)
  df <- read_excel(f, sheet = sh[1], col_types = "text", .name_repair = "minimal")
  names(df) <- trimws(names(df))
  df <- df[, names(df) != "" & !duplicated(names(df)), drop = FALSE]
  if (!"uuid" %in% names(df) || nrow(df) == 0) return(NULL)
  df |> filter(!is.na(uuid), trimws(uuid) != "") |> mutate(source_file = basename(f), .before = 1)
}

feedback_raw <- map(feedback_files, read_reviewed_log) |> compact() |> bind_rows()
cat("Reviewed logs read:", length(feedback_files), "file(s),", nrow(feedback_raw), "row(s)\n")

# Reviewed "other, please specify" workbooks (script 6, sheet other_responses) -> cleaning-log rows
source("R-scripts/Source scripts/Other_responses_functions_MOZ.R")
read_other_responses <- function(f) {
  sh <- excel_sheets(f)
  sh <- sh[tolower(trimws(sh)) == "other_responses"]
  if (!length(sh)) return(NULL)
  read_excel(f, sheet = sh[1], col_types = "text") |> mutate(source_file = basename(f))
}
other_reviewed <- map(feedback_files, read_other_responses) |> compact() |> bind_rows()
other_responses_issues <- tibble()
if (nrow(other_reviewed) > 0) {
  # Logbooks made before 2026-11-18 also contain "recode other" rows (step 3, check_others). The same
  # answers are in the other_responses workbook, which is the one applied: the logbook rows are dropped
  # so that a text is never changed twice.
  if ("issue" %in% names(feedback_raw)) {
    recode_other_rows <- feedback_raw |> filter(str_to_lower(str_trim(issue)) == "recode other")
    if (nrow(recode_other_rows) > 0) {
      cat("Logbook rows 'recode other' ignored (handled in other_responses):", nrow(recode_other_rows), "\n")
      feedback_raw <- feedback_raw |> filter(is.na(issue) | str_to_lower(str_trim(issue)) != "recode other")
    }
  }
  other_tool <- other_tool_tables(path_tool)
  other_conv <- other_responses_to_cleaning_log(other_reviewed, raw_data_list, other_tool$choices)
  other_responses_issues <- other_conv$issues
  if (nrow(other_conv$log) > 0) {
    feedback_raw <- bind_rows(feedback_raw, other_conv$log |> mutate(source_file = "other_responses"))
  }
  cat("other_responses rows read:", nrow(other_reviewed), "| cleaning-log rows created:", nrow(other_conv$log),
      "| rows not applied (no decision / follow-up / errors):", nrow(other_responses_issues), "\n")
  if (nrow(other_responses_issues)) print(count(other_responses_issues, problem, sort = TRUE))
}
if (nrow(feedback_raw) == 0) stop("No reviewed cleaning-log rows found in ", path_feedback, call. = FALSE)

for (cc in c("dataset", "question", "old_value", "new_value", "change_type", "repeat_index", "issue", "check_id", "notes")) {
  if (!cc %in% names(feedback_raw)) feedback_raw[[cc]] <- NA_character_
}

cleaning_log_reviewed <- feedback_raw |>
  mutate(
    change_type = str_trim(str_to_lower(change_type)),
    dataset = case_when(
      dataset %in% c("Submissions", "household_level", "main") | is.na(dataset) ~ "Submissions",
      dataset %in% c("nut_ind") ~ "nutrition_ind",
      dataset %in% c("prot_separated_children_breakdown") ~ "prot_sep_child",
      dataset %in% c("prot_hosted_separated_children_breakdown") ~ "prot_hosted_child",
      TRUE ~ dataset
    ),
    repeat_index = na_if(str_trim(repeat_index), "")
  )

# -------------------------------------------------------------------------------------------------
# 2. Validate
# -------------------------------------------------------------------------------------------------
feedback_not_reviewed <- cleaning_log_reviewed |> filter(is.na(change_type) | change_type == "")
feedback_invalid_type <- cleaning_log_reviewed |> filter(!is.na(change_type), change_type != "", !change_type %in% valid_change_types)
feedback_change_without_value <- cleaning_log_reviewed |> filter(change_type == "change_response", is.na(new_value) | trimws(new_value) == "")

cleaning_log_reviewed <- cleaning_log_reviewed |>
  filter(change_type %in% valid_change_types) |>
  filter(!(change_type == "change_response" & (is.na(new_value) | trimws(new_value) == ""))) |>
  distinct(dataset, uuid, repeat_index, question, change_type, new_value, .keep_all = TRUE)

feedback_conflicts <- cleaning_log_reviewed |>
  filter(change_type %in% c("change_response", "blank_response")) |>
  group_by(dataset, uuid, repeat_index, question) |>
  filter(n_distinct(paste(change_type, new_value)) > 1) |>
  ungroup() |>
  arrange(dataset, uuid, repeat_index, question)

cat("Valid reviewed rows:", nrow(cleaning_log_reviewed),
    "| not reviewed:", nrow(feedback_not_reviewed),
    "| invalid change_type:", nrow(feedback_invalid_type),
    "| change_response without new_value:", nrow(feedback_change_without_value),
    "| conflicting changes:", nrow(feedback_conflicts), "\n")
if (nrow(feedback_conflicts) > 0) warning("Conflicting changes for the same question - the last row read is applied. See feedback_conflicts.", call. = FALSE)
cleaning_log_reviewed <- cleaning_log_reviewed |> group_by(dataset, uuid, repeat_index, question, change_type) |> slice_tail(n = 1) |> ungroup()

# -------------------------------------------------------------------------------------------------
# 3. Deletions (whole households)
# -------------------------------------------------------------------------------------------------
deleted_uuids <- cleaning_log_reviewed |> filter(change_type == "remove_survey") |> pull(uuid) |> unique()
deletion_log <- raw_data_list$Submissions |>
  filter(uuid %in% deleted_uuids) |>
  select(any_of(c("uuid", "point_number", "admin1_name", "admin2_name", "admin3_name", "population_group"))) |>
  left_join(cleaning_log_reviewed |> filter(change_type == "remove_survey") |>
              group_by(uuid) |> summarise(reason = paste(unique(na.omit(c(issue, notes))), collapse = " | "), .groups = "drop"),
            by = "uuid")
cat("Households removed:", length(deleted_uuids), "\n")

# -------------------------------------------------------------------------------------------------
# 4. Clean each dataset
# -------------------------------------------------------------------------------------------------
# Repeats: the log uuid is the household uuid, so build a row key uuid + repeat_index (_index).
make_row_key <- function(df, dataset) {
  if (dataset == "Submissions") return(df |> mutate(.row_key = as.character(uuid)))
  df |> mutate(.row_key = paste0(as.character(uuid), "_", as.character(repeat_index)))
}

clean_one_dataset <- function(dataset) {
  raw <- raw_data_list[[dataset]]
  if (!is.data.frame(raw) || nrow(raw) == 0) return(NULL)
  raw <- make_row_key(raw, dataset) |> mutate(across(everything(), as.character))

  log_ds <- cleaning_log_reviewed |>
    filter(dataset == !!dataset, change_type %in% c("change_response", "blank_response", "no_action"), question %in% names(raw))
  log_ds <- if (dataset == "Submissions") mutate(log_ds, .row_key = uuid) else mutate(log_ds, .row_key = paste0(uuid, "_", repeat_index))
  log_ds <- log_ds |> filter(.row_key %in% raw$.row_key)
  not_applied <- cleaning_log_reviewed |>
    filter(dataset == !!dataset, change_type %in% c("change_response", "blank_response"), !question %in% names(raw))

  clean <- if (nrow(log_ds) == 0) raw else cleaningtools::create_clean_data(
    raw_dataset = raw, raw_data_uuid_column = ".row_key", cleaning_log = log_ds,
    cleaning_log_change_type_column = "change_type", change_response_value = "change_response",
    NA_response_value = "blank_response", no_change_value = "no_action", remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question", cleaning_log_uuid_column = ".row_key", cleaning_log_new_value_column = "new_value"
  )

  # create_clean_data() re-guesses column types: keep everything as text, like the raw data
  clean <- clean |> mutate(across(everything(), as.character))

  # Remove deleted households (also from repeats)
  clean <- clean |> filter(!as.character(uuid) %in% deleted_uuids)

  # Re-create select_multiple parent columns after dummy changes (and the reverse). Only the rows
  # that received a select_multiple change are processed (the full datasets are too large for
  # cleaningtools to process in reasonable time: 98,000 roster rows x 800 columns).
  sm_parents <- df_survey |> filter(str_detect(type, "^select_multiple")) |> pull(name)
  sm_log_keys <- log_ds |>
    filter(change_type %in% c("change_response", "blank_response"),
           question %in% sm_parents | str_remove(question, "/.*$") %in% sm_parents) |>
    pull(.row_key) |> unique()
  if (length(sm_log_keys) > 0) {
    sm_rows <- clean |> filter(.row_key %in% sm_log_keys)
    pc <- tryCatch(
      cleaningtools::recreate_parent_column(dataset = sm_rows, uuid_column = ".row_key", sm_separator = "/",
                                            kobo_survey = df_survey, kobo_choices = df_choices),
      error = function(e) { warning(dataset, ": recreate_parent_column failed - ", conditionMessage(e), call. = FALSE); NULL }
    )
    if (!is.null(pc)) {
      clean <- bind_rows(clean |> filter(!.row_key %in% sm_log_keys),
                         pc$data_with_fix_concat |> mutate(across(everything(), as.character)))
      if (is.data.frame(pc$correction_parent_sm_log) && nrow(pc$correction_parent_sm_log) > 0) {
        log_ds <- bind_rows(log_ds, pc$correction_parent_sm_log |> mutate(across(everything(), as.character), dataset = !!dataset))
      }
    }
  }

  # Review: compare raw vs clean only for the rows that appear in the cleaning log (all other rows
  # are untouched by construction); this keeps review_cleaning fast on 20,000+ households.
  # review_cleaning() renames the key column to "uuid", so the original uuid column is dropped here.
  review_keys <- unique(log_ds$.row_key[log_ds$change_type != "no_action"])
  review <- if (!length(review_keys)) tibble() else tryCatch(
    suppressWarnings(cleaningtools::review_cleaning(
      raw_dataset = raw |> filter(.row_key %in% review_keys, !as.character(uuid) %in% deleted_uuids) |> select(-uuid), raw_dataset_uuid_column = ".row_key",
      clean_dataset = clean |> filter(.row_key %in% review_keys) |> select(-uuid), clean_dataset_uuid_column = ".row_key",
      cleaning_log = log_ds |> filter(change_type != "no_action") |> select(-uuid), cleaning_log_uuid_column = ".row_key",
      cleaning_log_question_column = "question", cleaning_log_new_value_column = "new_value",
      cleaning_log_change_type_column = "change_type", cleaning_log_old_value_column = "old_value",
      deletion_log = NULL, check_for_deletion_log = FALSE
    )),
    error = function(e) tibble(review_error = conditionMessage(e))
  )

  list(clean = clean |> select(-.row_key), log = log_ds, review = review, not_applied = not_applied,
       n_raw = nrow(raw), n_clean = nrow(clean))
}

clean_results <- set_names(dataset_names) |> map(clean_one_dataset) |> compact()

clean_main <- clean_results$Submissions$clean
clean_roster <- clean_results$roster$clean
clean_edu_ind <- clean_results$edu_ind$clean
clean_health_ind <- clean_results$health_ind$clean
clean_nutrition_ind <- clean_results$nutrition_ind$clean
clean_health_wra <- clean_results$health_wra$clean
clean_prot_sep_child <- clean_results$prot_sep_child$clean
clean_prot_hosted_child <- clean_results$prot_hosted_child$clean

clean_summary <- imap_dfr(clean_results, ~ tibble(
  dataset = .y, rows_raw = .x$n_raw, rows_clean = .x$n_clean,
  changes_applied = sum(.x$log$change_type %in% c("change_response", "blank_response")),
  changes_not_applied_question_missing = nrow(.x$not_applied),
  review_issues = if (is.data.frame(.x$review) && !"review_error" %in% names(.x$review)) nrow(.x$review) else NA_integer_
))
print(clean_summary)

# -------------------------------------------------------------------------------------------------
# 5. Export
# -------------------------------------------------------------------------------------------------
review_all <- imap_dfr(clean_results, ~ if (is.data.frame(.x$review) && nrow(.x$review)) mutate(.x$review, across(everything(), as.character), dataset = .y, .before = 1) else NULL)
clean_output_path <- file.path(path_cleaning_folder_today, paste0("MSNA2026_MOZ_clean_data_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
write_xlsx(c(
  list(summary = clean_summary),
  set_names(map(clean_results, "clean"), paste0("clean_", substr(names(clean_results), 1, 25))),
  list(cleaning_log = cleaning_log_reviewed, deletion_log = deletion_log,
       review_cleaning = if (nrow(review_all)) review_all else tibble(note = "all changes applied as logged"),
       not_reviewed = feedback_not_reviewed, invalid_change_type = feedback_invalid_type, conflicts = feedback_conflicts,
       other_responses_not_applied = if (nrow(other_responses_issues)) other_responses_issues else tibble(note = "none"))
), clean_output_path)
cat("Clean data written:", clean_output_path, "\n")
