# =================================================================================================
# REACH MSNA 2026 Mozambique - Sampling weights
# -------------------------------------------------------------------------------------------------
# Replaces the Uganda version (refugee settlements / host districts / Kampala).
#
# Strata: district (admin2 p-code) x population group (idp / returnees / non_displaced).
# If the ToR uses another stratification (e.g. province x population group), change strata_vars.
#
# Weight per stratum h:  weight_h = (N_h / N) / (n_h / n)
#   N_h = stratum population (from the population frame), N = total population of all strata
#   n_h = clean interviews in the stratum, n = total clean interviews
#   -> weights sum to the number of interviews (mean weight = 1)
#   -> every repeat (roster, edu_ind, health_ind, nutrition_ind, health_wra, separated-children
#      loops) inherits the household weight.
#
# POPULATION FRAME (required - not included in the pipeline):
#   inputs/REACH_MOZ_MSNA2026_population_frame.xlsx with columns
#     admin2            district p-code as in the tool (e.g. MZ0101)
#     population_group  idp / returnees / non_displaced
#     population        number of households (or people - be consistent) in the stratum
#   Typical sources: IOM DTM baseline for IDPs and returnees, INE projections for the
#   non-displaced population. Use the frame agreed in the MSNA ToR / sampling plan.
#
# Required objects (script 8): clean_main, clean_roster, clean_edu_ind, clean_health_ind,
# clean_nutrition_ind, clean_health_wra, clean_prot_sep_child, clean_prot_hosted_child
# =================================================================================================

suppressPackageStartupMessages({ library(dplyr); library(readxl) })

path_population_frame <- if (exists("path_population_frame")) path_population_frame else "inputs/REACH_MOZ_MSNA2026_population_frame.xlsx"
strata_vars <- c("admin2", "population_group")

if (!file.exists(path_population_frame)) {
  stop("Population frame not found: ", path_population_frame,
       "\nCreate it with columns admin2, population_group, population (see the header of this script).", call. = FALSE)
}

pop_frame <- read_excel(path_population_frame) |>
  janitor::clean_names() |>
  mutate(across(all_of(strata_vars), as.character), population = as.numeric(population)) |>
  filter(!is.na(population), population > 0) |>
  mutate(strata = do.call(paste, c(across(all_of(strata_vars)), sep = "_")))

stopifnot(!anyDuplicated(pop_frame$strata))

main_w <- clean_main |>
  select(-any_of(c("weight", "strata"))) |>
  mutate(strata = do.call(paste, c(across(all_of(strata_vars), as.character), sep = "_")))

unmatched <- main_w |> filter(!strata %in% pop_frame$strata)
if (nrow(unmatched) > 0) {
  print(count(unmatched, across(all_of(strata_vars))), n = Inf)
  stop(nrow(unmatched), " interview(s) fall in a stratum that is not in the population frame - add the stratum or remove the interviews before weighting.", call. = FALSE)
}

weights_tbl <- main_w |>
  count(strata, name = "n_achieved") |>
  inner_join(pop_frame, by = "strata") |>
  mutate(pop_share = population / sum(population),
         sample_share = n_achieved / sum(n_achieved),
         weight = pop_share / sample_share) |>
  arrange(strata)

missing_strata <- anti_join(pop_frame, weights_tbl, by = "strata")
if (nrow(missing_strata) > 0) warning("Strata in the frame with no clean interviews (not represented): ", paste(missing_strata$strata, collapse = ", "), call. = FALSE)

main_w <- main_w |> left_join(select(weights_tbl, strata, weight), by = "strata")

stopifnot(!anyNA(main_w$weight), all(main_w$weight > 0), isTRUE(all.equal(sum(main_w$weight), nrow(main_w))))

# Repeats inherit the household weight
add_hh_weight <- function(loop_df, name) {
  if (!is.data.frame(loop_df) || nrow(loop_df) == 0) return(loop_df)
  out <- loop_df |>
    select(-any_of(c("weight", "strata"))) |>
    inner_join(select(main_w, `_uuid`, strata, weight), by = c("_submission__uuid" = "_uuid"))
  cat(name, ":", nrow(out), "rows kept of", nrow(loop_df), "\n")
  stopifnot(!anyNA(out$weight))
  out
}

clean_main <- main_w
clean_roster <- add_hh_weight(clean_roster, "roster")
clean_edu_ind <- add_hh_weight(clean_edu_ind, "edu_ind")
clean_health_ind <- add_hh_weight(clean_health_ind, "health_ind")
clean_nutrition_ind <- add_hh_weight(clean_nutrition_ind, "nutrition_ind")
if (exists("clean_health_wra")) clean_health_wra <- add_hh_weight(clean_health_wra, "health_wra")
if (exists("clean_prot_sep_child")) clean_prot_sep_child <- add_hh_weight(clean_prot_sep_child, "prot_sep_child")
if (exists("clean_prot_hosted_child")) clean_prot_hosted_child <- add_hh_weight(clean_prot_hosted_child, "prot_hosted_child")

print(weights_tbl, n = Inf)
write.csv(weights_tbl, file.path(path_cleaning_folder_today, "MOZ_MSNA_2026_weights.csv"), row.names = FALSE)
cat("\nAll weight checks passed.\n")

# Using the weights (srvyr)
# design_hh <- srvyr::as_survey_design(clean_main, strata = strata, weights = weight)
# design_ind <- srvyr::as_survey_design(clean_roster, ids = `_submission__uuid`, strata = strata, weights = weight)
