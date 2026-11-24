# =================================================================================================
# REACH MSNA 2026 Mozambique - helper functions for the "other responses" workflow
# -------------------------------------------------------------------------------------------------
# Used by:
#   6.Other_responses_MOZ.R                      -> builds the other_responses review workbook
#   8.Load_feedback_and_create_clean_data_MOZ.R  -> turns the reviewed workbook into cleaning-log rows
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr); library(tibble) })

# Key used to match a typed answer with the translation dictionary: lower case, no accents,
# single spaces, no trailing punctuation or quotes. "Rede mosquiteira." == "rede  mosquiteira".
other_norm_key <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[\"“”]", "")
  x <- str_replace_all(x, "[[:punct:]]+$", "")
  str_squish(x)
}

other_is_numeric_text <- function(x) str_detect(other_norm_key(x), "^[-_ 0-9.,%]*$")

# Survey / choices with label::english and label::portuguese, whatever the exact column names.
other_tool_tables <- function(path_tool) {
  sv <- readxl::read_excel(path_tool, "survey", col_types = "text")
  ch <- readxl::read_excel(path_tool, "choices", col_types = "text")
  other_label_col <- function(df, lang) {
    hit <- names(df)[str_detect(str_to_lower(names(df)), paste0("^label::", lang))]
    if (length(hit)) df[[hit[1]]] else rep(NA_character_, nrow(df))
  }
  sv <- sv |> mutate(type = str_squish(type), name = str_trim(name),
                     label_en = other_label_col(sv, "english"), label_pt = other_label_col(sv, "portuguese"),
                     q_type = word(type, 1), list_name = if_else(q_type %in% c("select_one", "select_multiple"), word(type, 2), NA_character_))
  ch <- ch |> filter(!is.na(list_name)) |>
    mutate(list_name = str_trim(list_name), name = str_trim(name),
           label_en = coalesce(other_label_col(ch, "english"), name), label_pt = coalesce(other_label_col(ch, "portuguese"), label_en),
           dropdown = paste0(label_en, " | ", label_pt))
  list(survey = sv, choices = ch)
}

# One row per "other - please specify" text question: the select question it belongs to and the
# choice that opens it (read from the relevant: selected(${parent}, 'other')).
other_question_map <- function(tool) {
  sv <- tool$survey
  txt <- sv |>
    filter(q_type == "text", !is.na(relevant), str_detect(relevant, "selected\\s*\\(")) |>
    mutate(
      parent = str_match(relevant, "selected\\s*\\(\\s*\\$\\{([^}]+)\\}")[, 2],
      other_code = str_match(relevant, "selected\\s*\\(\\s*\\$\\{[^}]+\\}\\s*,\\s*['\"]([^'\"]+)['\"]")[, 2]
    ) |>
    select(question_name = name, parent, other_code, text_label_en = label_en)
  txt |>
    left_join(sv |> select(parent = name, parent_type = q_type, list_name, full_label = label_en, full_label_pt = label_pt), by = "parent") |>
    filter(parent_type %in% c("select_one", "select_multiple"))
}

# Dropdown text -> choice code for one list (accepts "EN | PT", EN alone or PT alone).
other_choice_code <- function(value, list_name, choices) {
  value <- str_squish(as.character(value))
  ch <- choices[choices$list_name == list_name, , drop = FALSE]
  if (is.na(value) || value == "" || nrow(ch) == 0) return(NA_character_)
  for (col in c("dropdown", "label_en", "label_pt", "name")) {
    hit <- ch$name[str_squish(ch[[col]]) == value]
    if (length(hit)) return(hit[1])
  }
  key <- other_norm_key(value)
  for (col in c("label_en", "label_pt")) {
    hit <- ch$name[other_norm_key(ch[[col]]) == key]
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

# Suggest an existing choice when the typed answer matches a choice label (PT or EN): exact match
# of the normalised text, or a small spelling slip in Portuguese (<= 10% of the letters). Suggestion only.
other_suggest_choice <- function(text, list_name, other_code, choices, fuzzy = TRUE) {
  ch <- choices[choices$list_name == list_name & choices$name != other_code &
                  !choices$name %in% c("dnk", "pnta", "dk", "none"), , drop = FALSE]
  if (!nrow(ch)) return(NA_character_)
  key <- other_norm_key(text)
  if (is.na(key) || key == "") return(NA_character_)
  labs <- c(other_norm_key(ch$label_pt), other_norm_key(ch$label_en))
  drop <- c(ch$dropdown, ch$dropdown)
  exact <- which(labs == key)
  if (length(exact)) return(drop[exact[1]])
  # Spelling slips only ("fanta de informacao"): Portuguese labels, long answers, <= 10% of letters.
  # Larger tolerances gave wrong matches (latrina SEM descarga -> COM descarga, rent -> tent).
  if (!fuzzy || nchar(key) < 8) return(NA_character_)
  labs_pt <- other_norm_key(ch$label_pt)
  d <- as.numeric(adist(key, labs_pt))
  best <- which.min(d)
  if (length(best) && d[best] <= max(1, floor(0.1 * nchar(key)))) return(ch$dropdown[best])
  NA_character_
}

# -------------------------------------------------------------------------------------------------
# Reviewed other_responses workbook -> cleaning-log rows (used by script 8)
# -------------------------------------------------------------------------------------------------
# Decisions per row (exactly one of them):
#   TRUE other     : text field(s) replaced by the English text; 'other' stays selected.
#   EXISTING 1-3   : select_one -> parent = choice; select_multiple -> parent/other = 0, parent/choice = 1,
#                    parent rebuilt; text field(s) blanked. Percentage questions <parent>_<code>_prop
#                    (cm_income_source): the 'other' percentage is added to EXISTING other 1.
#   INVALID other  : select_one -> parent blanked; select_multiple -> 'other' removed (if it was the only
#                    choice, the question and its dummies are blanked); text field(s) blanked;
#                    <parent>_other_prop blanked.
# Rows with no decision, more than one decision, a dropdown value that is not a choice of the list,
# or several EXISTING choices on a select_one are not applied and are returned in $issues.
other_responses_to_cleaning_log <- function(reviewed, raw_data_list, choices) {
  col_like <- function(pattern) {
    hit <- names(reviewed)[str_detect(str_to_lower(names(reviewed)), pattern)]
    if (length(hit)) reviewed[[hit[1]]] else rep(NA_character_, nrow(reviewed))
  }
  blank <- function(x) is.na(x) | str_trim(x) == ""
  rv <- reviewed |>
    mutate(across(everything(), as.character),
           .true = col_like("^true other"), .ex1 = col_like("^existing other 1"), .ex2 = col_like("^existing other 2"),
           .ex3 = col_like("^existing other 3"), .invalid = col_like("^invalid other"), .follow = col_like("^follow-up"),
           .n_dec = (!blank(.true)) + (!blank(.ex1) | !blank(.ex2) | !blank(.ex3)) + (str_to_lower(str_trim(coalesce(.invalid, ""))) %in% c("yes", "sim", "y")),
           repeat_index = na_if(str_trim(repeat_index), ""), dataset = coalesce(na_if(dataset, ""), "Submissions"))

  get_row <- function(ds, uuid, ri) {
    df <- raw_data_list[[ds]]
    if (!is.data.frame(df)) return(NULL)
    i <- if (is.na(ri)) which(as.character(df$uuid) == uuid) else which(as.character(df$uuid) == uuid & as.character(df$repeat_index) == ri)
    if (length(i) != 1) return(NULL)
    df[i, , drop = FALSE]
  }
  val <- function(row, col) if (col %in% names(row)) as.character(row[[col]][1]) else NA_character_

  logs <- list(); issues <- list()
  add_issue <- function(r, why) issues[[length(issues) + 1]] <<- tibble(dataset = r$dataset, uuid = r$uuid, repeat_index = r$repeat_index,
                                                                        question_name = r$question_name, response_pt = r$response_pt, problem = why)
  for (k in seq_len(nrow(rv))) {
    r <- rv[k, ]
    if (r$.n_dec == 0) { add_issue(r, if (!blank(r$.follow)) "follow-up pending (not applied)" else "no decision"); next }
    if (r$.n_dec > 1) { add_issue(r, "more than one decision (TRUE / EXISTING / INVALID) - not applied"); next }
    row <- get_row(r$dataset, r$uuid, r$repeat_index)
    if (is.null(row)) { add_issue(r, "uuid / repeat_index not found in the raw data"); next }
    text_vars <- str_split(coalesce(r$other_text_vars, r$question_name), ";")[[1]]
    parent <- r$parent; ptype <- r$parent_type; oc <- r$other_code
    cur <- str_split(str_squish(coalesce(val(row, parent), "")), " ")[[1]]; cur <- cur[cur != ""]
    out <- list()
    add <- function(q, new, type, issue) {
      if (!q %in% names(row)) return(invisible())
      old <- val(row, q)
      if (type == "change_response" && identical(old, new)) return(invisible())
      if (type == "blank_response" && blank(old)) return(invisible())
      out[[length(out) + 1]] <<- tibble(dataset = r$dataset, uuid = r$uuid, repeat_index = r$repeat_index, question = q,
                                        old_value = old, new_value = if (type == "blank_response") NA_character_ else new,
                                        change_type = type, issue = issue, check_id = "other_responses")
    }
    if (!blank(r$.true)) {
      for (tv in text_vars) add(tv, str_squish(r$.true), "change_response", "TRUE other: translated")
    } else if (str_to_lower(str_trim(coalesce(r$.invalid, ""))) %in% c("yes", "sim", "y")) {
      if (ptype == "select_one") {
        add(parent, NA, "blank_response", "INVALID other")
      } else {
        rest <- setdiff(cur, oc)
        if (length(rest)) {
          add(parent, paste(rest, collapse = " "), "change_response", "INVALID other: 'other' removed")
          add(paste0(parent, "/", oc), "0", "change_response", "INVALID other: 'other' removed")
        } else {
          add(parent, NA, "blank_response", "INVALID other: only choice, question blanked")
          for (dv in names(row)[startsWith(names(row), paste0(parent, "/"))]) add(dv, NA, "blank_response", "INVALID other: only choice, question blanked")
        }
      }
      add(paste0(parent, "_", oc, "_prop"), NA, "blank_response", "INVALID other")
      for (tv in text_vars) add(tv, NA, "blank_response", "INVALID other")
    } else {
      picks <- c(r$.ex1, r$.ex2, r$.ex3); picks <- picks[!blank(picks)]
      codes <- vapply(picks, other_choice_code, character(1), list_name = r$list_name, choices = choices, USE.NAMES = FALSE)
      if (any(is.na(codes))) { add_issue(r, paste0("EXISTING value not in list ", r$list_name, ": ", paste(picks[is.na(codes)], collapse = "; "))); next }
      codes <- unique(codes)
      if (ptype == "select_one") {
        if (length(codes) > 1) { add_issue(r, "select_one question: only EXISTING other 1 can be used - not applied"); next }
        add(parent, codes, "change_response", "EXISTING other: recoded")
      } else {
        order_ref <- choices$name[choices$list_name == r$list_name]
        new_sel <- unique(c(setdiff(cur, oc), codes)); new_sel <- new_sel[order(match(new_sel, order_ref))]
        add(parent, paste(new_sel, collapse = " "), "change_response", "EXISTING other: recoded")
        add(paste0(parent, "/", oc), "0", "change_response", "EXISTING other: recoded")
        for (cd in codes) add(paste0(parent, "/", cd), "1", "change_response", "EXISTING other: recoded")
      }
      prop_other <- paste0(parent, "_", oc, "_prop")
      if (prop_other %in% names(row) && !blank(val(row, prop_other))) {
        prop_to <- paste0(parent, "_", codes[1], "_prop")
        p_old <- suppressWarnings(as.numeric(val(row, prop_to))); p_oth <- suppressWarnings(as.numeric(val(row, prop_other)))
        add(prop_to, as.character(sum(c(p_old, p_oth), na.rm = TRUE)), "change_response",
            paste0("EXISTING other: 'other' percentage added to ", codes[1], if (length(codes) > 1) " (several choices selected - check the split)" else ""))
        add(prop_other, NA, "blank_response", "EXISTING other: recoded")
      }
      for (tv in text_vars) add(tv, NA, "blank_response", "EXISTING other: recoded")
    }
    if (length(out)) logs[[length(logs) + 1]] <- bind_rows(out)
  }
  list(log = bind_rows(logs), issues = bind_rows(issues))
}
