# =================================================================================================
# REACH MSNA 2026 Mozambique - Sampling weights
# -------------------------------------------------------------------------------------------------
# Strata: district (admin2 p-code) x population group (idp / returnees / non_displaced).
#
# Population frame: OCHA workbook inputs/REACH_MOZ_MSNA2026_population_frame.xlsx, sheet
# "Baseline data" (159 districts, p-codes, IDPs and returnees from IOM DTM round 22,
# Non-Displaced = Total - IDPs - Returnees). The other sheets of that workbook (Sheet1/Sheet4) are
# older Cabo Delgado pivots based on DTM R21 and are NOT used.
#
# Weight per stratum h:  weight_h = (N_h / N) / (n_h / n)
#   N_h = households in the stratum (individuals / mean household size of the stratum in the sample,
#         see convert_individuals_to_households), N = sum over strata kept
#   n_h = clean interviews in the stratum, n = interviews kept
#   -> weights sum to the number of interviews kept (mean weight = 1)
#   -> every repeat inherits the household weight.
#
# Interviews in a stratum with population 0 (or missing) in the frame - e.g. IDPs interviewed in
# Maputo City, returnees in Balama - are handled by strata_without_population:
#   "pool_district" (DEFAULT, decided 16/11/2026) the household is weighted in the non-displaced stratum of its district
#                   (keeps the interview in weighted results; its population group is unchanged).
#   "exclude"       weight = 0, in_weighted_frame = FALSE (kept in the data, out of weighted results)
#
# Required objects (script 8): clean_main and clean_* repeats; path_cleaning_folder_today
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readxl) })

path_population_frame <- if (exists("path_population_frame")) path_population_frame else "inputs/REACH_MOZ_MSNA2026_population_frame.xlsx"
population_frame_sheet <- if (exists("population_frame_sheet")) population_frame_sheet else "Baseline data"
strata_without_population <- if (exists("strata_without_population")) strata_without_population else "pool_district"  # decided 2026-11-16
convert_individuals_to_households <- if (exists("convert_individuals_to_households")) convert_individuals_to_households else TRUE

stopifnot(strata_without_population %in% c("exclude", "pool_district"))
if (!file.exists(path_population_frame)) stop("Population frame not found: ", path_population_frame, call. = FALSE)

num <- function(x) suppressWarnings(as.numeric(x))

# -------------------------------------------------------------------------------------------------
# 1. Read the OCHA baseline (header on row 3; row 1 holds national totals)
# -------------------------------------------------------------------------------------------------
pop_raw <- read_excel(path_population_frame, sheet = population_frame_sheet, skip = 2,
                      col_types = "text", .name_repair = ~ trimws(.x))
col_of <- function(pattern) {
  hit <- names(pop_raw)[grepl(pattern, names(pop_raw), ignore.case = TRUE)]
  if (!length(hit)) stop("Column matching '", pattern, "' not found in sheet ", population_frame_sheet, call. = FALSE)
  hit[1]
}
pop_district <- pop_raw |>
  filter(grepl("^MZ\\d{4}$", .data[[col_of("^ADM2 Pcode$")]])) |>
  transmute(
    admin2 = .data[[col_of("^ADM2 Pcode$")]],
    admin2_name_frame = .data[[col_of("^ADM2 PT$")]],
    total = num(.data[[col_of("^Total Population$")]]),
    idp = coalesce(num(.data[[col_of("^IDPs")]]), 0),
    returnees = coalesce(num(.data[[col_of("^Return")]]), 0),
    non_displaced = num(.data[[col_of("^Non-Displaced$")]])
  )
stopifnot(!anyDuplicated(pop_district$admin2))
bad_sum <- pop_district |> filter(abs(total - idp - returnees - non_displaced) > 1)
if (nrow(bad_sum)) warning(nrow(bad_sum), " district(s) where Total != IDPs + Returnees + Non-Displaced", call. = FALSE)

pop_frame <- pop_district |>
  pivot_longer(c(idp, returnees, non_displaced), names_to = "population_group", values_to = "population_ind") |>
  mutate(strata = paste(admin2, population_group, sep = "_"))

# -------------------------------------------------------------------------------------------------
# 2. Match interviews to strata
# -------------------------------------------------------------------------------------------------
main_w <- clean_main |>
  select(-any_of(c("weight", "strata", "in_weighted_frame"))) |>
  mutate(strata = paste(as.character(admin2), as.character(population_group), sep = "_"))

no_district <- setdiff(unique(main_w$admin2), pop_district$admin2)
if (length(no_district)) stop("District(s) in the data but not in the frame: ", paste(no_district, collapse = ", "), call. = FALSE)

strata_sample <- main_w |>
  group_by(strata, admin2, population_group) |>
  summarise(n_achieved = n(), mean_hh_size = mean(num(hh_size), na.rm = TRUE), .groups = "drop") |>
  left_join(select(pop_frame, strata, population_ind), by = "strata")

strata_zero <- strata_sample |> filter(is.na(population_ind) | population_ind <= 0)
if (nrow(strata_zero)) {
  message("Weights: ", sum(strata_zero$n_achieved), " interview(s) in ", nrow(strata_zero),
          " stratum/strata with no population in the frame -> '", strata_without_population, "'")
}

if (strata_without_population == "pool_district" && nrow(strata_zero)) {
  # The households of a group with no population in the frame are weighted as part of the
  # non-displaced stratum of their district (the frame counts that district's whole population there).
  main_w <- main_w |>
    mutate(strata = if_else(strata %in% strata_zero$strata, paste(admin2, "non_displaced", sep = "_"), strata))
  strata_sample <- main_w |>
    group_by(strata) |>
    summarise(admin2 = first(admin2), n_achieved = n(), mean_hh_size = mean(num(hh_size), na.rm = TRUE), .groups = "drop") |>
    mutate(population_group = sub("^MZ\\d{4}_", "", strata)) |>
    left_join(select(pop_frame, strata, population_ind), by = "strata")
}

# -------------------------------------------------------------------------------------------------
# 3. Weights
# -------------------------------------------------------------------------------------------------
weights_tbl <- strata_sample |>
  mutate(
    population_hh = if (convert_individuals_to_households) population_ind / mean_hh_size else population_ind,
    in_weighted_frame = !is.na(population_hh) & population_hh > 0
  )
kept <- weights_tbl$in_weighted_frame
weights_tbl <- weights_tbl |>
  mutate(
    pop_share = if_else(in_weighted_frame, population_hh / sum(population_hh[kept]), NA_real_),
    sample_share = if_else(in_weighted_frame, n_achieved / sum(n_achieved[kept]), NA_real_),
    weight = if_else(in_weighted_frame, pop_share / sample_share, 0)
  ) |>
  arrange(strata)

main_w <- main_w |> left_join(select(weights_tbl, strata, weight, in_weighted_frame), by = "strata")
n_kept <- sum(main_w$in_weighted_frame)
stopifnot(!anyNA(main_w$weight), isTRUE(all.equal(sum(main_w$weight), n_kept)))

# Strata with more interviews than estimated households: the frame probably under-counts them
more_interviews_than_hh <- weights_tbl |>
  filter(in_weighted_frame, n_achieved > population_hh) |>
  select(strata, n_achieved, population_ind, mean_hh_size, population_hh, weight)
if (nrow(more_interviews_than_hh)) {
  warning(nrow(more_interviews_than_hh), " stratum/strata have more interviews than estimated households in the frame ",
          "(check the population figures): ", paste(more_interviews_than_hh$strata, collapse = ", "), call. = FALSE)
}

frame_not_sampled <- pop_frame |> filter(population_ind > 0, !strata %in% weights_tbl$strata)
cat("Weights: ", n_kept, " of ", nrow(main_w), " interviews weighted; ",
    nrow(frame_not_sampled), " frame strata with population but no interview (not represented).\n", sep = "")

# Repeats inherit the household weight
add_hh_weight <- function(loop_df, name) {
  if (!exists(loop_df) || !is.data.frame(get(loop_df)) || nrow(get(loop_df)) == 0) return(invisible(NULL))
  df <- get(loop_df)
  out <- df |>
    select(-any_of(c("weight", "strata", "in_weighted_frame"))) |>
    left_join(select(main_w, `_uuid`, strata, weight, in_weighted_frame), by = c("_submission__uuid" = "_uuid"))
  if (anyNA(out$weight)) stop(name, ": ", sum(is.na(out$weight)), " row(s) without a household weight", call. = FALSE)
  assign(loop_df, out, envir = .GlobalEnv)
}

clean_main <- main_w
for (nm in c("clean_roster", "clean_edu_ind", "clean_health_ind", "clean_nutrition_ind",
             "clean_health_wra", "clean_prot_sep_child", "clean_prot_hosted_child")) add_hh_weight(nm, nm)

weights_path <- file.path(path_cleaning_folder_today, paste0("MOZ_MSNA_2026_weights_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
writexl::write_xlsx(list(
  weights_by_stratum = weights_tbl,
  strata_without_population = strata_zero,
  more_interviews_than_hh = more_interviews_than_hh,
  frame_strata_not_sampled = frame_not_sampled |> select(admin2, admin2_name_frame, population_group, population_ind),
  settings = tibble::tibble(setting = c("population_frame", "sheet", "strata_without_population", "convert_individuals_to_households"),
                            value = c(path_population_frame, population_frame_sheet, strata_without_population, convert_individuals_to_households))
), weights_path)
cat("All weight checks passed. Weights written:", weights_path, "\n")

# Using the weights (srvyr) - drop households outside the frame when strata_without_population = "exclude"
# design_hh <- srvyr::as_survey_design(dplyr::filter(clean_main, in_weighted_frame), strata = strata, weights = weight)
