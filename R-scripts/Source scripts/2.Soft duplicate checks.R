cat("\014")

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

print_step("2. Running soft duplicate checks")

# --------------------------------------------------------------------------------------------------
# 1. Prepare main household data
# --------------------------------------------------------------------------------------------------

data <- data_hh_main

soft_duplicate_log <- tibble::tibble(
  dataset = character(),
  uuid = character(),
  parent_uuid = character(),
  repeat_index = character(),
  question = character(),
  old_value = character(),
  new_value = character(),
  change_type = character(),
  issue = character(),
  check_id = character(),
  check_to_perform = character(),
  columns_to_clean = character(),
  notes = character()
)

# --------------------------------------------------------------------------------------------------
# 2. Helper functions
# --------------------------------------------------------------------------------------------------

get_uuid_col <- function(df) {
  
  uuid_candidates <- c("uuid", "_uuid", "instance_name", "_id")
  uuid_col <- uuid_candidates[uuid_candidates %in% names(df)]
  
  if (length(uuid_col) == 0) {
    return(NA_character_)
  }
  
  uuid_col[[1]]
}

make_duplicate_key <- function(df, duplicate_cols) {
  
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(duplicate_cols),
        ~ dplyr::if_else(
          is.na(as.character(.x)) | trimws(as.character(.x)) == "",
          "<missing>",
          trimws(as.character(.x))
        )
      )
    ) |>
    dplyr::transmute(
      .dup_key = purrr::pmap_chr(
        dplyr::across(dplyr::all_of(duplicate_cols)),
        ~ paste(c(...), collapse = " | ")
      )
    ) |>
    dplyr::pull(.dup_key)
}

# --------------------------------------------------------------------------------------------------
# 3. Run household-level soft duplicate check
# --------------------------------------------------------------------------------------------------

if (is.data.frame(data) && nrow(data) > 0) {
  
  uuid_col <- get_uuid_col(data)
  
  if (is.na(uuid_col)) {
    stop(
      "No UUID column found in data_hh_main. Expected one of: uuid, _uuid, instance_name, _id.",
      call. = FALSE
    )
  }
  
  # Final-tool-aligned variables.
  # These names are based on the final Kobo tool and should not be replaced by older draft names such as
  # district_name, sub_county_division, refugee_settlement, or enumerator_id.
  # MOZ: point_number is the unique Kobo _id and there is no enumerator code, so an exact match on
  # location + respondent profile + all FCS/rCSI answers is used for the "exact profile" duplicate.
  duplicate_cols_priority <- c(
    "admin3",
    "setting",
    "population_group",
    "resp_gender",
    "resp_age",
    "resp_hoh_yn",
    "hoh_gender",
    "hoh_age",
    "hh_size",
    "snfi_shelter_type",
    "wash_drinking_water_source",
    "m3269_hh_fcs_staples", "m3270_hh_fcs_pulses", "m3271_hh_fcs_dairy", "m3272_hh_fcs_protein",
    "m3273_hh_fcs_veg", "m3274_hh_fcs_fruits", "m3275_hh_fcs_fat", "m3276_hh_fcs_sugar",
    "fsl_rcsi_rely_on_less_expensive_food", "fsl_rcsi_borrow", "fsl_rcsi_mealsize",
    "fsl_rcsi_mealadult", "fsl_rcsi_mealnb"
  )
  
  duplicate_cols <- intersect(duplicate_cols_priority, names(data))
  
  if (length(duplicate_cols) >= 3) {
    
    data_soft_dup <- data |>
      dplyr::mutate(
        .uuid_for_soft_dup = as.character(.data[[uuid_col]]),
        .dup_key = make_duplicate_key(data, duplicate_cols)
      )
    
    soft_duplicate_log_manual <- data_soft_dup |>
      dplyr::group_by(.dup_key) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        dataset = "Submissions",
        uuid = .uuid_for_soft_dup,
        parent_uuid = NA_character_,
        repeat_index = NA_character_,
        question = paste(duplicate_cols, collapse = ", "),
        old_value = .dup_key,
        new_value = NA_character_,
        change_type = NA_character_,
        issue = "Potential soft duplicate based on matching household/location/respondent profile.",
        check_id = "SUB_SOFT_DUPLICATE",
        check_to_perform = paste0(
          "duplicated profile across: ",
          paste(duplicate_cols, collapse = ", ")
        ),
        columns_to_clean = paste(duplicate_cols, collapse = ", "),
        notes = "Review; this is a soft duplicate flag and does not necessarily mean the submission is a true duplicate."
      ) |>
      dplyr::distinct(
        dataset,
        uuid,
        question,
        old_value,
        check_id,
        .keep_all = TRUE
      )
    
    soft_duplicate_log <- dplyr::bind_rows(
      soft_duplicate_log,
      soft_duplicate_log_manual
    )
    
  } else {
    
    message(
      "Soft duplicate check skipped: fewer than 3 final-tool duplicate-profile columns were found. Columns found: ",
      paste(duplicate_cols, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------------------------------------------
  # 4. Optional cleaningtools soft duplicate check
  # ------------------------------------------------------------------------------------------------
  
  if (
    requireNamespace("cleaningtools", quietly = TRUE) &&
    exists("df_survey_clean") &&
    is.data.frame(df_survey_clean) &&
    nrow(df_survey_clean) > 0
  ) {
    
    try({
      
      # MOZ: the pairwise comparison needs n x n memory (21,000+ households would need tens of GB),
      # so it is run within each administrative post (admin3). Duplicates are expected between
      # interviews done in the same area. Groups with fewer than 2 interviews are skipped.
      soft_dup_group_col <- if ("admin3" %in% names(data)) "admin3" else NA_character_
      soft_dup_groups <- if (is.na(soft_dup_group_col)) list(all = data) else split(data, data[[soft_dup_group_col]])
      soft_dup_groups <- soft_dup_groups[vapply(soft_dup_groups, nrow, integer(1)) >= 2]
      clog_sf <- list(soft_duplicate = purrr::map_dfr(soft_dup_groups, function(gdf) {
        res <- tryCatch(
          cleaningtools::check_soft_duplicates(
            dataset = gdf,
            kobo_survey = df_survey_clean,
            uuid_column = uuid_col,
            sm_separator = "/",
            log_name = "soft_duplicate",
            idnk_value = "dnk",
            threshold = 3
          ),
          error = function(e) NULL
        )
        if (is.list(res) && "soft_duplicate" %in% names(res)) {
          dplyr::mutate(res$soft_duplicate, dplyr::across(dplyr::everything(), as.character))
        } else {
          tibble::tibble()
        }
      }))
      
      if (is.list(clog_sf) && "soft_duplicate" %in% names(clog_sf)) {
        
        soft_duplicate_log_extra <- clog_sf$soft_duplicate |>
          janitor::clean_names() |>
          dplyr::mutate(
            dataset = "Submissions",
            check_id = "SUB_SOFT_DUPLICATE_CLEANINGTOOLS"
          )
        
        if (!"uuid" %in% names(soft_duplicate_log_extra) && uuid_col %in% names(soft_duplicate_log_extra)) {
          soft_duplicate_log_extra <- soft_duplicate_log_extra |>
            dplyr::rename(uuid = dplyr::all_of(uuid_col))
        }
        
        soft_duplicate_log_extra <- soft_duplicate_log_extra |>
          dplyr::mutate(
            uuid = if ("uuid" %in% names(.)) as.character(uuid) else NA_character_,
            parent_uuid = if ("parent_uuid" %in% names(.)) as.character(parent_uuid) else NA_character_,
            repeat_index = if ("repeat_index" %in% names(.)) as.character(repeat_index) else NA_character_,
            question = if ("question" %in% names(.)) as.character(question) else NA_character_,
            old_value = if ("old_value" %in% names(.)) as.character(old_value) else NA_character_,
            new_value = if ("new_value" %in% names(.)) as.character(new_value) else NA_character_,
            change_type = if ("change_type" %in% names(.)) as.character(change_type) else NA_character_,
            issue = if ("issue" %in% names(.)) {
              as.character(issue)
            } else {
              "Potential soft duplicate from cleaningtools."
            },
            check_to_perform = if ("check_to_perform" %in% names(.)) {
              as.character(check_to_perform)
            } else {
              "cleaningtools::check_soft_duplicates"
            },
            columns_to_clean = if ("columns_to_clean" %in% names(.)) {
              as.character(columns_to_clean)
            } else {
              NA_character_
            },
            notes = if ("notes" %in% names(.)) {
              as.character(notes)
            } else {
              "Review; generated by cleaningtools soft duplicate check."
            }
          ) |>
          dplyr::select(
            dplyr::any_of(c(
              "dataset",
              "uuid",
              "parent_uuid",
              "repeat_index",
              "question",
              "old_value",
              "new_value",
              "change_type",
              "issue",
              "check_id",
              "check_to_perform",
              "columns_to_clean",
              "notes"
            ))
          )
        
        soft_duplicate_log <- dplyr::bind_rows(
          soft_duplicate_log,
          soft_duplicate_log_extra
        ) |>
          dplyr::distinct(
            dataset,
            uuid,
            question,
            old_value,
            check_id,
            .keep_all = TRUE
          )
      }
      
    }, silent = TRUE)
  }
}

# --------------------------------------------------------------------------------------------------
# 5. Output summary
# --------------------------------------------------------------------------------------------------

cat("Soft duplicate records flagged: ", nrow(soft_duplicate_log), "\n", sep = "")

if (nrow(soft_duplicate_log) > 0) {
  print(
    soft_duplicate_log |>
      dplyr::count(check_id, issue, name = "n_flags") |>
      dplyr::arrange(dplyr::desc(n_flags))
  )
}

print_step("Soft duplicate checks completed")

# --------------------------------------------------------------------------------------------------
# 6. Report new objects created
# --------------------------------------------------------------------------------------------------

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0) {
  cat(crayon::blue("New dataframes created:\n"))
  cat(paste(setdiff(all_dfs_names_after, all_dfs_names_before), collapse = "\n"), "\n")
}

if (length(setdiff(all_lists_names_after, all_lists_names_before)) > 0) {
  cat(crayon::blue("New lists created:\n"))
  cat(paste(setdiff(all_lists_names_after, all_lists_names_before), collapse = "\n"), "\n")
}


