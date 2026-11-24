# =================================================================================================
# REACH MSNA 2026 Mozambique - 6. "Other, please specify" review workbook (other_responses)
# -------------------------------------------------------------------------------------------------
# Replaces "6.Specify_other_checks.R" (one cleaning-log row per select_multiple dummy, ~74,000 rows)
# with the IMPACT other_responses format: ONE row per typed answer, with
#   - response_pt   : the answer as typed (Portuguese)
#   - response_en   : English translation from the translation dictionary (empty if not known)
#   - suggested_choice : automatic match of the answer with a choice of the same question (check it)
#   - selected_choices : what the respondent selected for that question
#   - decision columns (to fill): TRUE other / EXISTING other 1-3 / INVALID other / FOLLOW-UP
# EXISTING other columns have a dropdown with the choices of that question as "English | Português",
# so the answer in Portuguese can be compared with the form options in both languages.
#
# Inputs : raw_data_list (script 1), path_tool, path_cleaning_folder_today,
#          path_other_translations (dictionary, optional: inputs/REACH_MOZ_MSNA2026_other_translations.xlsx)
# Output : <daily folder>/Other responses/MSNA2026_MOZ_other_responses_<date>.xlsx
#          The reviewed copy goes to inputs/feedback/ and is applied by script 8.
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr); library(purrr); library(openxlsx) })
source("R-scripts/Source scripts/Other_responses_functions_MOZ.R")

if (exists("print_step", mode = "function")) print_step("6. Creating the other_responses review workbook")

path_other_translations <- get0("path_other_translations", ifnotfound = "inputs/REACH_MOZ_MSNA2026_other_translations.xlsx")
path_other_recode_rules <- get0("path_other_recode_rules", ifnotfound = "inputs/REACH_MOZ_MSNA2026_other_recode_rules.xlsx")

tool <- other_tool_tables(path_tool)
oq_map <- other_question_map(tool)

# -------------------------------------------------------------------------------------------------
# 1. Collect every typed "other" answer (all sheets)
# -------------------------------------------------------------------------------------------------
dataset_of_sheet <- c(Submissions = "Submissions", roster = "roster", edu_ind = "edu_ind", health_ind = "health_ind",
                      nutrition_ind = "nutrition_ind", health_wra = "health_wra",
                      prot_sep_child = "prot_sep_child", prot_hosted_child = "prot_hosted_child")

context_cols <- c("point_number", "admin1_name", "admin2", "admin2_name", "admin3", "admin3_name", "population_group")
hh_context <- raw_data_list$Submissions |>
  transmute(uuid = as.character(uuid), across(any_of(context_cols), as.character))

collect_other <- function(ds) {
  df <- raw_data_list[[ds]]
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  qs <- oq_map |> filter(question_name %in% names(df))
  if (!nrow(qs)) return(NULL)
  base <- df |>
    transmute(uuid = as.character(uuid),
              repeat_index = if (ds == "Submissions") NA_character_ else as.character(repeat_index),
              .row = row_number())
  map_dfr(seq_len(nrow(qs)), function(i) {
    q <- qs[i, ]
    txt <- as.character(df[[q$question_name]])
    # numbers typed in the text field (e.g. "100" in other_cm_income_source_other_prop) are kept too:
    # they are not a description of the "other" answer and must be reviewed like any other text
    keep <- !is.na(txt) & str_trim(txt) != ""
    if (!any(keep)) return(NULL)
    tibble(
      dataset = ds,
      uuid = base$uuid[keep], repeat_index = base$repeat_index[keep],
      question_name = q$question_name, parent = q$parent, parent_type = q$parent_type,
      list_name = q$list_name, other_code = q$other_code,
      full_label = q$full_label, full_label_pt = q$full_label_pt,
      parent_value = if (q$parent %in% names(df)) as.character(df[[q$parent]][keep]) else NA_character_,
      response_pt = str_squish(txt[keep])
    )
  })
}

other_raw <- map_dfr(names(dataset_of_sheet), collect_other)

# A question can have two text fields (MOZ: other_cm_income_source and
# other_cm_income_source_other_prop). Keep ONE row per answered question and record both fields,
# so the reviewer decides once and script 8 updates both.
other_rows <- other_raw |>
  group_by(dataset, uuid, repeat_index, parent) |>
  summarise(
    question_name = first(question_name),
    other_text_vars = paste(unique(question_name), collapse = ";"),
    across(c(parent_type, list_name, other_code, full_label, full_label_pt, parent_value), first),
    response_pt = paste(unique(response_pt), collapse = " || "),
    .groups = "drop"
  )

# -------------------------------------------------------------------------------------------------
# 2. Labels of the current selection, translation, suggestion
# -------------------------------------------------------------------------------------------------
choices <- tool$choices
label_of <- function(codes, ln) {
  if (is.na(codes) || codes == "") return(NA_character_)
  cc <- str_split(codes, "\\s+")[[1]]
  lab <- choices$label_en[match(paste(ln, cc), paste(choices$list_name, choices$name))]
  paste(coalesce(lab, cc), collapse = ";\n")
}

translations <- if (file.exists(path_other_translations)) {
  readxl::read_excel(path_other_translations, col_types = "text") |>
    filter(!is.na(key), !is.na(response_en), str_trim(response_en) != "") |>
    distinct(key, .keep_all = TRUE) |>
    select(key, response_en)
} else {
  message("   No translation dictionary at ", path_other_translations, " - response_en left empty.")
  tibble(key = character(), response_en = character())
}

# Recode rules (list_name, regex on the normalised answer, suggested choice code or INVALID). They
# only fill suggested_choice: nothing is changed until a reviewer copies the decision.
recode_rules <- if (file.exists(path_other_recode_rules)) {
  readxl::read_excel(path_other_recode_rules, col_types = "text") |> filter(!is.na(list_name), !is.na(pattern), !is.na(suggestion))
} else tibble(list_name = character(), pattern = character(), suggestion = character(), note = character())
bad_rules <- recode_rules |> filter(suggestion != "INVALID", !paste(list_name, suggestion) %in% paste(choices$list_name, choices$name))
if (nrow(bad_rules)) warning("Recode rules with a choice code that is not in the tool (ignored): ", paste(bad_rules$list_name, bad_rules$suggestion, collapse = "; "), call. = FALSE)
recode_rules <- recode_rules |> anti_join(bad_rules, by = c("list_name", "pattern", "suggestion"))
rule_suggestion <- function(key, ln) {
  rr <- recode_rules[recode_rules$list_name == ln, , drop = FALSE]
  if (!nrow(rr) || is.na(key)) return(NA_character_)
  hit <- which(vapply(rr$pattern, function(p) isTRUE(str_detect(key, regex(p))), logical(1)))
  if (!length(hit)) return(NA_character_)
  sg <- rr$suggestion[hit[1]]
  if (sg == "INVALID") return("INVALID")
  choices$dropdown[choices$list_name == ln & choices$name == sg][1]
}

other_rows <- other_rows |>
  mutate(
    selected_choices = map2_chr(parent_value, list_name, label_of),
    key = map_chr(response_pt, ~ other_norm_key(str_split(.x, " \\|\\| ")[[1]][1]))
  ) |>
  mutate(response_en = map_chr(response_pt, function(t) {
    # each part of a combined answer ("a || b") is translated on its own; numbers are kept as typed
    parts <- str_split(t, " \\|\\| ")[[1]]
    en <- translations$response_en[match(other_norm_key(parts), translations$key)]
    en <- ifelse(is.na(en) & other_is_numeric_text(parts), parts, en)
    if (all(is.na(en))) NA_character_ else paste(coalesce(en, paste0("[", parts, "]")), collapse = " || ")
  })) |>
  mutate(suggested_choice = pmap_chr(list(key, response_pt, response_en, list_name, other_code), function(k, t, e, l, o) {
    hit <- rule_suggestion(k, l)
    if (is.na(hit)) hit <- other_suggest_choice(str_split(t, " \\|\\| ")[[1]][1], l, o, choices)
    if (is.na(hit) && !is.na(e)) hit <- other_suggest_choice(str_remove(e, "\\s*\\(.*\\)$"), l, o, choices, fuzzy = FALSE)
    hit
  })) |>
  left_join(hh_context, by = "uuid") |>
  arrange(question_name, key, admin2_name)

# -------------------------------------------------------------------------------------------------
# 3. Workbook
# -------------------------------------------------------------------------------------------------
decision_cols <- c(
  "TRUE other (copy response_en or provide a better translation)",
  "EXISTING other 1 (select the most appropriate choice)",
  "EXISTING other 2 (select the most appropriate choice)",
  "EXISTING other 3 (select the most appropriate choice)",
  "INVALID other (select yes or leave blank)",
  "FOLLOW-UP message (what is unclear about this response?)",
  "Explanation"
)

out <- other_rows |>
  transmute(uuid, dataset, repeat_index, point_number, admin1_name, admin2_name, admin3, admin3_name, population_group,
            question_name, other_text_vars, parent, parent_type, list_name, other_code,
            full_label, full_label_pt, selected_choices, response_pt, response_en, suggested_choice)
for (cc in decision_cols) out[[cc]] <- NA_character_

# Dropdown lists: one column per list_name, "English | Português", without the "other" choice itself
lists_used <- other_rows |> distinct(list_name, other_code)
dropdown_tbl <- map(seq_len(nrow(lists_used)), function(i) {
  ch <- choices |> filter(list_name == lists_used$list_name[i], name != lists_used$other_code[i])
  ch$dropdown
})
names(dropdown_tbl) <- lists_used$list_name
max_len <- max(lengths(dropdown_tbl), 1)
dropdown_df <- as.data.frame(map(dropdown_tbl, ~ c(.x, rep(NA_character_, max_len - length(.x)))), check.names = FALSE)
dropdown_df[["yes_no"]] <- c("Yes", rep(NA_character_, max_len - 1))

options_ref <- choices |>
  filter(list_name %in% lists_used$list_name) |>
  left_join(oq_map |> distinct(list_name, parent) |> group_by(list_name) |> summarise(questions = paste(parent, collapse = ", ")), by = "list_name") |>
  select(list_name, questions, choice_code = name, label_en, label_pt)

readme <- tibble::tibble(
  `Column / decision` = c(
    "One row", "response_pt / response_en", "suggested_choice", "selected_choices",
    "TRUE other", "EXISTING other 1-3", "INVALID other", "FOLLOW-UP / Explanation", "Only one decision per row",
    "Two text fields", "Income percentages", "When done"
  ),
  `What to do` = c(
    "Each row is one 'other, please specify' answer of one household (or one household member for loops: see dataset and repeat_index).",
    "response_pt is the answer as typed. response_en comes from the translation dictionary (inputs/REACH_MOZ_MSNA2026_other_translations.xlsx; first version is a machine draft). Empty when the answer is new. Check it before copying it into TRUE other.",
    "Automatic suggestion: a recode rule (inputs/REACH_MOZ_MSNA2026_other_recode_rules.xlsx) or the same text as a choice label (PT or EN). 'INVALID' = suggested invalid. It is only a suggestion: copy it into EXISTING other 1 (or INVALID) if it is right.",
    "What the respondent selected for this question (English labels). For single-choice questions it is 'Other'.",
    "The answer is a real new category: copy response_en (or write a better English translation). The text is replaced by this English text; 'other' stays selected.",
    "The answer matches choice(s) of the form: select up to 3 in the dropdown ('English | Português'). 'Other' is unselected and the chosen choice(s) selected. Single-choice questions: use only EXISTING other 1.",
    "The answer is not an answer to the question (e.g. 'não sei', '...', unrelated text): select Yes. 'Other' is removed; if it was the only choice, the question becomes empty.",
    "Something is unclear and must be asked to the field team: write the question in FOLLOW-UP; nothing is changed until it is answered. Use Explanation for notes.",
    "Fill TRUE other OR EXISTING OR INVALID (not more than one). Rows with more than one, or with nothing filled, are left unchanged and listed by script 8.",
    "other_text_vars lists every text field of the question (cm_income_source has two). Both are updated together.",
    "cm_income_source: when 'other' is recoded to ONE existing source, its percentage is moved to that source by script 8. With several sources the percentage cannot be split: the row is listed for follow-up.",
    "Save as .xlsx (keep the sheet name other_responses) and put it in inputs/feedback/. Script 8 applies it."
  )
)

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", wrapText = TRUE, valign = "top", border = "Bottom")
addWorksheet(wb, "README"); writeData(wb, "README", readme, headerStyle = hs); setColWidths(wb, "README", 1:2, c(28, 130))
addStyle(wb, "README", createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(readme) + 1), cols = 1:2, gridExpand = TRUE)

sh <- "other_responses"
addWorksheet(wb, sh)
writeData(wb, sh, out, headerStyle = hs, withFilter = TRUE)
freezePane(wb, sh, firstActiveRow = 2, firstActiveCol = which(names(out) == "response_pt"))
nc <- ncol(out); nr <- nrow(out)
col_of <- function(n) which(names(out) == n)
fill_true <- createStyle(fgFill = "#E5FFCC", textDecoration = "bold", wrapText = TRUE, valign = "top", border = "Bottom")
fill_exist <- createStyle(fgFill = "#E5FFEC", textDecoration = "bold", wrapText = TRUE, valign = "top", border = "Bottom")
fill_follow <- createStyle(fgFill = "#CCE5FF", textDecoration = "bold", wrapText = TRUE, valign = "top", border = "Bottom")
fill_info <- createStyle(fgFill = "#FFF2CC", textDecoration = "bold", wrapText = TRUE, valign = "top", border = "Bottom")
addStyle(wb, sh, fill_true, rows = 1, cols = col_of(decision_cols[1]))
addStyle(wb, sh, fill_exist, rows = 1, cols = col_of(decision_cols[2]):col_of(decision_cols[4]))
addStyle(wb, sh, fill_true, rows = 1, cols = col_of(decision_cols[5]))
addStyle(wb, sh, fill_follow, rows = 1, cols = col_of(decision_cols[6]):col_of(decision_cols[7]))
addStyle(wb, sh, fill_info, rows = 1, cols = col_of("response_pt"):col_of("suggested_choice"))
addStyle(wb, sh, createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nr + 1), cols = col_of("full_label"):col_of("suggested_choice"), gridExpand = TRUE)
widths <- rep(14, nc)
widths[col_of("uuid")] <- 12
widths[match(c("full_label", "full_label_pt", "selected_choices", "response_pt", "response_en", "suggested_choice"), names(out))] <- c(35, 35, 28, 35, 35, 32)
widths[match(decision_cols, names(out))] <- c(30, 30, 30, 30, 16, 30, 25)
setColWidths(wb, sh, cols = seq_len(nc), widths = widths)
setRowHeights(wb, sh, rows = 1, heights = 60)

addWorksheet(wb, "Dropdown_values")
writeData(wb, "Dropdown_values", dropdown_df)
addWorksheet(wb, "Options_PT_EN")
writeData(wb, "Options_PT_EN", options_ref, headerStyle = hs, withFilter = TRUE)
setColWidths(wb, "Options_PT_EN", 1:5, c(30, 40, 22, 50, 50))

# Data validation: EXISTING columns -> the list of that question; INVALID -> Yes
excel_col <- function(i) openxlsx::int2col(i)
dd_names <- names(dropdown_df)
ex_cols <- col_of(decision_cols[2]):col_of(decision_cols[4])
for (ln in unique(out$list_name)) {
  rows <- which(out$list_name == ln) + 1
  j <- match(ln, dd_names)
  n_opt <- sum(!is.na(dropdown_df[[ln]]))
  ref <- sprintf("'Dropdown_values'!$%s$2:$%s$%d", excel_col(j), excel_col(j), n_opt + 1)
  # contiguous blocks keep the number of validation rules small
  blocks <- split(rows, cumsum(c(1, diff(rows) != 1)))
  # (openxlsx prints a harmless "one argument not used by format" warning for sheet-referenced lists)
  for (b in blocks) suppressWarnings(dataValidation(wb, sh, cols = ex_cols, rows = b, type = "list", value = ref, allowBlank = TRUE, showErrorMsg = TRUE))
}
suppressWarnings(dataValidation(wb, sh, cols = col_of(decision_cols[5]), rows = 2:(nr + 1), type = "list",
               value = sprintf("'Dropdown_values'!$%s$2:$%s$2", excel_col(ncol(dropdown_df)), excel_col(ncol(dropdown_df))),
               allowBlank = TRUE, showErrorMsg = TRUE))
sheetVisibility(wb)[match("Dropdown_values", names(wb))] <- "hidden"

dir_other <- file.path(path_cleaning_folder_today, "Other responses")
dir.create(dir_other, showWarnings = FALSE, recursive = TRUE)
other_responses_path <- file.path(dir_other, paste0("MSNA2026_MOZ_other_responses_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"))
saveWorkbook(wb, other_responses_path, overwrite = TRUE)

# -------------------------------------------------------------------------------------------------
# 4. Keep the translation dictionary up to date: new texts are added with an empty response_en
#    (existing rows are never changed). Fill response_en there once and every later run uses it.
# -------------------------------------------------------------------------------------------------
dict_cols <- c("key", "response_pt_example", "response_en", "n_answers", "questions", "translation_source", "checked_by")
dict_old <- if (file.exists(path_other_translations)) readxl::read_excel(path_other_translations, col_types = "text") else tibble()
for (cc in dict_cols) if (!cc %in% names(dict_old)) dict_old[[cc]] <- NA_character_
dict_new <- other_rows |>
  select(-key) |>
  tidyr::separate_rows(response_pt, sep = " \\|\\| ") |>
  mutate(key = other_norm_key(response_pt)) |>
  filter(!key %in% dict_old$key, !is.na(key), key != "", !other_is_numeric_text(key)) |>
  group_by(key) |>
  summarise(response_pt_example = first(response_pt), n_answers = as.character(n()),
            questions = paste(unique(question_name), collapse = "; "), .groups = "drop") |>
  mutate(response_en = NA_character_, translation_source = "new - to translate", checked_by = NA_character_)
if (nrow(dict_new) > 0) {
  writexl::write_xlsx(bind_rows(dict_old[, c(dict_cols, setdiff(names(dict_old), dict_cols))], dict_new), path_other_translations)
  cat("   Translation dictionary:", nrow(dict_new), "new text(s) added without translation ->", path_other_translations, "\n")
}

other_responses_summary <- out |>
  count(question_name, name = "answers") |>
  left_join(out |> group_by(question_name) |> summarise(translated = sum(!is.na(response_en)), suggested = sum(!is.na(suggested_choice))), by = "question_name") |>
  arrange(desc(answers))

cat("\nother_responses rows:", nr, "| translated:", sum(!is.na(out$response_en)), "(", round(100 * mean(!is.na(out$response_en))), "%)",
    "| with suggested choice:", sum(!is.na(out$suggested_choice)), "\n")
cat("Workbook:", other_responses_path, "\n")
