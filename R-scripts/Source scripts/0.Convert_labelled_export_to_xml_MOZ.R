# =================================================================================================
# REACH MSNA 2026 Mozambique - 0. Convert the labelled Kobo export to XML names / XML codes
# -------------------------------------------------------------------------------------------------
# Why this script exists
#   The Mozambique data file (e.g. REACH_MOZ_Data_MSNA_01102026.xlsx) is NOT a standard Kobo
#   "XML values and headers" export. Column headers are the English question labels and the
#   cell values are English choice labels ("Yes", "Married", ...). The whole cleaning pipeline
#   (logical checks, cleaningtools, specify-other, cleaning logs) works on XML names/codes, so
#   this script translates the file back to XML using the Kobo tool (survey + choices sheets).
#
# What it does, sheet by sheet
#   1. Rebuilds the list of columns the tool produces in each sheet (main + each begin_repeat),
#      in tool order, using label::english (or the name when the row has no label).
#   2. Matches the export headers to that list in order (labels repeat, e.g. "If other, please
#      specify:2", so the match is sequential, not a simple lookup).
#   3. Renames select_multiple dummy columns to parent/choice_name, matching the choice label.
#   4. Recodes select_one values from label to choice name (list taken from the tool).
#   5. Rebuilds each select_multiple parent column from its 0/1 dummies (the labelled parent text
#      cannot be split reliably because labels contain spaces).
#   6. Renames the Kobo ids to the standard server names:
#        main sheet : uuid -> _uuid, Id -> _id, _index stays
#        repeats    : uuid -> _submission__uuid, Id -> _submission__id, _index / _parent_index stay
#   7. Writes an XML-style workbook and a mapping log for review.
#
# Outputs (inputs/ folder)
#   - path_data_xml_rds : converted export as an R list (read by script 1 - fast, low memory)
#   - path_data_xml  : same data as xlsx, one sheet per Kobo sheet, XML names (for review)
#   - path_conversion_log : header mapping, unmatched columns, unmapped values (REVIEW THIS)
#
# Run position: first, before "1.Load and process the data.R".
# Required objects: path_data_labelled, path_tool, path_data_xml, path_conversion_log
# =================================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(readxl)
  library(writexl)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

stopifnot(exists("path_data_labelled"), exists("path_tool"), exists("path_data_xml"))
if (!exists("path_conversion_log")) path_conversion_log <- sub("\\.xlsx$", "_conversion_log.xlsx", path_data_xml)
if (!file.exists(path_data_labelled)) stop("Labelled MOZ export not found: ", path_data_labelled, call. = FALSE)
if (!file.exists(path_tool)) stop("Kobo tool not found: ", path_tool, call. = FALSE)

# Re-use the converted file when it is newer than both inputs (reading 120 MB takes minutes).
force_reconvert <- if (exists("force_reconvert")) isTRUE(force_reconvert) else FALSE
if (!exists("path_data_xml_rds")) path_data_xml_rds <- sub("\\.xlsx$", ".rds", path_data_xml)
if (!force_reconvert && file.exists(path_data_xml_rds) &&
    file.mtime(path_data_xml_rds) > max(file.mtime(path_data_labelled), file.mtime(path_tool))) {
  message("0. Converted XML export is up to date - skipping conversion: ", path_data_xml_rds)
} else {

message("0. Converting labelled MOZ export to XML names/codes ...")

# -------------------------------------------------------------------------------------------------
# 1. Helpers
# -------------------------------------------------------------------------------------------------

norm_label <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "<[^>]+>", "")          # html tags in labels
  x <- str_replace_all(x, "\\*\\*", "")           # markdown bold
  x <- str_replace_all(x, "[\u2018\u2019`]", "'")
  x <- str_replace_all(x, "[\u201c\u201d]", "\"")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace_all(x, "\\s+/", "/")
  x <- str_replace_all(x, "/\\s+", "/")
  str_to_lower(str_trim(x))
}

# Kobo/Excel truncate very long headers; compare on the first n characters when long.
labels_match <- function(a, b, n = 150, check_choice = FALSE) {
  if (is.na(a) || is.na(b)) return(FALSE)
  if (a == b) return(TRUE)
  if (nchar(a) >= n && nchar(b) >= n && substr(a, 1, n) == substr(b, 1, n)) {
    # For select_multiple dummies the parent label can be long: the choice part (after the last
    # "/") must also be identical, otherwise every dummy would match the first choice.
    if (!check_choice) return(TRUE)
    return(sub(".*/", "", a) == sub(".*/", "", b))
  }
  FALSE
}

pick_label_col <- function(df) {
  cands <- names(df)[str_detect(names(df), regex("^label::english", ignore_case = TRUE))]
  if (length(cands) == 0) cands <- names(df)[str_detect(names(df), regex("^label", ignore_case = TRUE))]
  cands[1]
}

# -------------------------------------------------------------------------------------------------
# 2. Read tool
# -------------------------------------------------------------------------------------------------

tool_survey <- read_excel(path_tool, sheet = "survey", col_types = "text") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_trim(name))
tool_choices <- read_excel(path_tool, sheet = "choices", col_types = "text") |>
  filter(!is.na(list_name)) |>
  mutate(list_name = str_trim(list_name), name = str_trim(name))

survey_label_col <- pick_label_col(tool_survey)
choice_label_col <- pick_label_col(tool_choices)
tool_survey$label_en <- tool_survey[[survey_label_col]]
tool_choices$label_en <- tool_choices[[choice_label_col]]

tool_survey <- tool_survey |>
  mutate(
    q_type = word(type, 1),
    list_name = if_else(q_type %in% c("select_one", "select_multiple"), word(type, 2), NA_character_)
  )

# Assign every question to its sheet (main or the innermost begin_repeat).
scope <- character(nrow(tool_survey))
stack <- character(0)
for (i in seq_len(nrow(tool_survey))) {
  t <- tool_survey$q_type[i]
  if (t %in% c("begin_repeat", "begin repeat")) { stack <- c(stack, tool_survey$name[i]); scope[i] <- NA; next }
  if (t %in% c("end_repeat", "end repeat")) { stack <- head(stack, -1); scope[i] <- NA; next }
  scope[i] <- if (length(stack) == 0) "main" else tail(stack, 1)
}
tool_survey$scope <- scope

skip_types <- c("begin_group", "end_group", "begin", "end group", "begin group",
                "start", "end", "today", "deviceid", "audit", "username", "phonenumber",
                "simserial", "subscriberid", "geotrace", "geoshape")

expected_columns <- function(scope_name) {
  q <- tool_survey |> filter(scope == scope_name, !q_type %in% skip_types)
  out <- list()
  for (i in seq_len(nrow(q))) {
    lab <- q$label_en[i]
    if (is.na(lab) || str_trim(lab) == "") lab <- q$name[i]
    out[[length(out) + 1]] <- tibble(xml_name = q$name[i], label = lab, q_type = q$q_type[i],
                                     list_name = q$list_name[i], parent = NA_character_)
    if (q$q_type[i] == "select_multiple") {
      ch <- tool_choices |> filter(list_name == q$list_name[i])
      if (nrow(ch) > 0) {
        out[[length(out) + 1]] <- tibble(
          xml_name = paste0(q$name[i], "/", ch$name),
          label = paste0(lab, "/", coalesce(ch$label_en, ch$name)),
          q_type = "sm_choice", list_name = q$list_name[i], parent = q$name[i]
        )
      }
    }
  }
  bind_rows(out) |> mutate(label_norm = norm_label(label))
}

# Columns produced by the MOZ export that are not tool labels (indicator calculates renamed by
# the data team, admin p-codes, ids). Extend this list if a new export adds other columns.
manual_header_map <- c(
  "_index" = "_index", "_parent_index" = "_parent_index", "_parent_table_name" = "_parent_table_name",
  "ADM1_PCODE" = "admin1_pcode", "ADM2_PCODE" = "admin2_pcode", "ADM3_PCODE" = "admin3_pcode",
  "FCS weighted component 1" = "fcs_weight1", "FCS weighted component 2" = "fcs_weight2",
  "FCS weighted component 3" = "fcs_weight3", "FCS weighted component 4" = "fcs_weight4",
  "FCS weighted component 5" = "fcs_weight5", "FCS weighted component 6" = "fcs_weight6",
  "FCS weighted component 7" = "fcs_weight7", "FCS weighted component 8" = "fcs_weight8",
  "Food Consumption Score (FCS)" = "fcs_score", "Food consumption category" = "fcs_cat",
  "Reduced Coping Strategies Index (rCSI)" = "rcsi_score",
  "Household Hunger Scale (HHS)" = "hhs_score", "Household hunger category" = "hhs_cat",
  "Livelihood stress coping flag" = "lcs_stress_coping", "Livelihood crisis coping flag" = "lcs_crisis_coping",
  "Livelihood emergency coping flag" = "lcs_emergency_coping",
  "Livelihood coping severity score" = "lcs_score", "Livelihood coping category" = "lcs_cat",
  "Food Insecurity Experience Scale raw score (FIES)" = "fies_score",
  "meta/rootUuid" = "meta_rootuuid"
)

# -------------------------------------------------------------------------------------------------
# 3. Header matching
# -------------------------------------------------------------------------------------------------

match_headers <- function(headers, exp, sheet_label) {
  xml <- rep(NA_character_, length(headers))
  how <- rep(NA_character_, length(headers))
  j <- 1L
  current_sm <- NULL      # last select_multiple parent matched (for dummy columns)
  exp_norm <- exp$label_norm
  for (k in seq_along(headers)) {
    h <- headers[k]
    if (h %in% names(manual_header_map)) { xml[k] <- manual_header_map[[h]]; how[k] <- "manual"; next }
    hn <- norm_label(h)
    cands <- unique(c(hn, norm_label(str_remove(h, "\\d+$"))))
    hit <- NA_integer_
    window <- seq.int(j, min(j + 80L, nrow(exp)))
    if (j <= nrow(exp)) {
      for (w in window) {
        is_choice <- exp$q_type[w] == "sm_choice"
        if (any(vapply(cands, labels_match, logical(1), b = exp_norm[w], check_choice = is_choice))) { hit <- w; break }
      }
    }
    if (!is.na(hit)) {
      xml[k] <- exp$xml_name[hit]; how[k] <- "label_sequence"; j <- hit + 1L
      if (exp$q_type[hit] == "select_multiple") current_sm <- exp[hit, ]
      if (exp$q_type[hit] == "sm_choice") current_sm <- exp[exp$xml_name == exp$parent[hit], ][1, ]
      next
    }
    # Dummy of the current select_multiple whose choice label changed between tool versions.
    if (!is.null(current_sm) && str_detect(h, "/")) {
      parent_lab <- norm_label(current_sm$label)
      prefix <- norm_label(str_replace(h, "/[^/]*$", ""))
      if (labels_match(prefix, parent_lab) || startsWith(parent_lab, prefix) || startsWith(prefix, substr(parent_lab, 1, 150))) {
        ch_lab <- norm_label(str_extract(h, "[^/]*$"))
        ch <- tool_choices |> filter(list_name == current_sm$list_name)
        ch_norm <- norm_label(ch$label_en)
        pos <- which(ch_norm == ch_lab)
        if (length(pos) == 0) {
          d <- adist(ch_lab, ch_norm)[1, ]
          if (length(d) && min(d) <= max(3, 0.25 * nchar(ch_lab))) pos <- which.min(d)
        }
        if (length(pos) > 0) {
          xml[k] <- paste0(current_sm$xml_name, "/", ch$name[pos[1]]); how[k] <- "sm_choice_label"
        } else {
          xml[k] <- paste0(current_sm$xml_name, "/", janitor::make_clean_names(str_extract(h, "[^/]*$")))
          how[k] <- "sm_choice_not_in_tool"
        }
        next
      }
    }
    xml[k] <- janitor::make_clean_names(h); how[k] <- "not_in_tool_clean_name"
  }
  # Final safety: unique names
  xml <- make.unique(xml, sep = "_dup")
  tibble(sheet = sheet_label, export_header = headers, xml_name = xml, match_method = how)
}

# -------------------------------------------------------------------------------------------------
# 4. Value recoding
# -------------------------------------------------------------------------------------------------

recode_select_one <- function(values, list_name) {
  ch <- tool_choices |> filter(list_name == !!list_name)
  if (nrow(ch) == 0) return(list(values = values, unmapped = character(0)))
  lut_label <- setNames(ch$name, norm_label(ch$label_en))
  v_norm <- norm_label(values)
  out <- values
  is_label <- !is.na(values) & v_norm %in% names(lut_label)
  out[is_label] <- unname(lut_label[v_norm[is_label]])
  already_code <- !is.na(values) & values %in% ch$name
  unmapped <- unique(values[!is.na(values) & !is_label & !already_code & str_trim(values) != ""])
  list(values = out, unmapped = unmapped)
}

convert_sheet <- function(df, scope_name, sheet_label) {
  exp <- expected_columns(scope_name)
  map <- match_headers(names(df), exp, sheet_label)
  names(df) <- map$xml_name

  # Kobo ids -> standard server names
  if (scope_name == "main") {
    df <- df |> rename(any_of(c(`_uuid` = "uuid", `_id` = "id")))
  } else {
    df <- df |> rename(any_of(c(`_submission__uuid` = "uuid", `_submission__id` = "id")))
  }

  unmapped_log <- list()
  q <- tool_survey |> filter(scope == scope_name)

  # select_one
  so <- q |> filter(q_type == "select_one", name %in% names(df))
  for (i in seq_len(nrow(so))) {
    r <- recode_select_one(df[[so$name[i]]], so$list_name[i])
    df[[so$name[i]]] <- r$values
    if (length(r$unmapped)) unmapped_log[[length(unmapped_log) + 1]] <-
      tibble(sheet = sheet_label, variable = so$name[i], list_name = so$list_name[i], unmapped_value = r$unmapped)
  }

  # select_multiple: rebuild parent from dummies
  sm <- q |> filter(q_type == "select_multiple", name %in% names(df))
  for (i in seq_len(nrow(sm))) {
    p <- sm$name[i]
    dcols <- names(df)[startsWith(names(df), paste0(p, "/"))]
    if (length(dcols) == 0) next
    mat <- as.matrix(df[dcols])
    sel <- !is.na(mat) & trimws(mat) %in% c("1", "1.0", "TRUE", "true")
    choice_names <- substring(dcols, nchar(p) + 2)
    parent_val <- apply(sel, 1, function(r) if (any(r)) paste(choice_names[r], collapse = " ") else NA_character_)
    answered <- !is.na(df[[p]]) & str_trim(df[[p]]) != ""
    df[[p]] <- ifelse(answered, parent_val, NA_character_)
    # Keep dummies empty when the question was not asked (Kobo XML convention).
    for (dc in dcols) df[[dc]][!answered] <- NA_character_
  }

  # Enumerator gender in the MOZ export mixes M/F and male/female
  if ("enum_gender" %in% names(df)) {
    df$enum_gender <- dplyr::case_when(
      str_to_lower(df$enum_gender) %in% c("m", "male", "masculino") ~ "male",
      str_to_lower(df$enum_gender) %in% c("f", "female", "feminino") ~ "female",
      TRUE ~ df$enum_gender
    )
  }

  # Dates exported as "YYYY-MM-DD 00:00:00"
  date_vars <- q |> filter(q_type == "date", name %in% names(df)) |> pull(name)
  for (dv in date_vars) {
    v <- str_remove(df[[dv]], "[ T]00:00:00(\\.0+)?$")
    is_serial <- !is.na(v) & str_detect(v, "^\\d{4,5}(\\.\\d+)?$")   # readxl returns Excel serials as text
    v[is_serial] <- as.character(as.Date(floor(as.numeric(v[is_serial])), origin = "1899-12-30"))
    df[[dv]] <- v
  }

  list(data = df, map = map, unmapped = bind_rows(unmapped_log))
}

# -------------------------------------------------------------------------------------------------
# 5. Run on every sheet
# -------------------------------------------------------------------------------------------------

# path_data_labelled can be the .xlsx export or a folder with one CSV per sheet
# (file name = sheet name). readxl loads a whole sheet into memory: the MOZ main sheet
# (~490 MB of XML) needs ~7-8 GB of RAM. If R crashes or freezes here, open the export in
# Excel, save each sheet as CSV UTF-8 into one folder and point path_data_labelled to it.
input_is_csv_folder <- dir.exists(path_data_labelled)
if (input_is_csv_folder) {
  csv_files <- list.files(path_data_labelled, pattern = "\\.csv$", full.names = TRUE)
  export_sheets <- sub("\\.csv$", "", basename(csv_files), ignore.case = TRUE)
  main_first <- order(!str_detect(export_sheets, regex("kobo|tool|main|form", ignore_case = TRUE)))
  csv_files <- csv_files[main_first]; export_sheets <- export_sheets[main_first]
} else {
  export_sheets <- excel_sheets(path_data_labelled)
}

read_export_sheet <- function(sh) {
  if (input_is_csv_folder) {
    f <- csv_files[match(sh, export_sheets)]
    df <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                          na = c("", "NA"), name_repair = "minimal", progress = FALSE)
  } else {
    df <- read_excel(path_data_labelled, sheet = sh, col_types = "text", .name_repair = "minimal")
  }
  tibble::as_tibble(df, .name_repair = "minimal")
}
repeat_names <- tool_survey |> filter(q_type %in% c("begin_repeat", "begin repeat")) |> pull(name)

# Sheet names longer than 31 characters are truncated by Excel ("prot_separated_children_brea...").
resolve_scope <- function(sheet) {
  if (sheet == export_sheets[1]) return("main")
  s <- str_remove(sheet, "\\.\\.\\.$")
  hit <- repeat_names[repeat_names == s]
  if (!length(hit)) hit <- repeat_names[startsWith(repeat_names, s)]
  if (length(hit)) hit[1] else NA_character_
}

converted <- list(); maps <- list(); unmapped <- list()
for (sh in export_sheets) {
  sc <- resolve_scope(sh)
  if (is.na(sc)) { message("   - skipping sheet not in tool: ", sh); next }
  message("   - reading ", sh, " (", sc, ")")
  df <- read_export_sheet(sh)
  res <- convert_sheet(df, sc, sh)
  out_name <- if (sc == "main") "main" else sc
  converted[[out_name]] <- res$data
  maps[[out_name]] <- res$map
  unmapped[[out_name]] <- res$unmapped
}

conversion_map <- bind_rows(maps)
conversion_unmapped_values <- bind_rows(unmapped)
conversion_summary <- conversion_map |> count(sheet, match_method, name = "n_columns")

# Fast copy for script 1 (reading the 120 MB xlsx back needs ~7 GB of RAM).
if (!exists("path_data_xml_rds")) path_data_xml_rds <- sub("\\.xlsx$", ".rds", path_data_xml)
saveRDS(converted, path_data_xml_rds)
# Excel copy for human review (sheet names max 31 chars)
names(converted) <- substr(names(converted), 1, 31)
write_xlsx(converted, path_data_xml)
write_xlsx(list(
  summary = conversion_summary,
  header_map = conversion_map,
  not_matched_to_tool = conversion_map |> filter(match_method %in% c("not_in_tool_clean_name", "sm_choice_not_in_tool")),
  unmapped_values = if (nrow(conversion_unmapped_values)) conversion_unmapped_values else tibble(note = "all select_one values mapped")
), path_conversion_log)

message("0. Conversion done. Columns matched per method:")
print(conversion_summary, n = 50)
if (nrow(conversion_unmapped_values)) {
  message("   ", nrow(conversion_unmapped_values), " select_one value(s) not found in the tool choices - see 'unmapped_values' in ", path_conversion_log)
}
rm(converted)
invisible(gc())
}
