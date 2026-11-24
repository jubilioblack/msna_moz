# =================================================================================================
# REACH MSNA 2026 Mozambique - 1b. Build the logical-checks workbook
# -------------------------------------------------------------------------------------------------
# Replaces the Uganda "1b.Add_new_logical_checks.R". All checks are written against the final
# MOZ Kobo tool (MOZ 2026 MSNA Kobo Tool FINAL) and use XML names produced by script 0.
#
# Design rules
#   - Only cross-question / plausibility checks. Kobo already enforces required/relevant/constraint,
#     so "field missing when relevant" checks from the Uganda workbook were not carried over.
#   - Every variable used in check_to_perform is validated against the tool when the workbook is
#     built (QA_summary sheet). A check whose variables are missing is kept but marked FAIL.
#   - Numbers are wrapped in suppressWarnings(as.numeric()) because the export stores text.
#   - -999 (don't know) is excluded from numeric thresholds.
#
# Output: path_checks (inputs/REACH_MOZ_MSNA2026_logical_checks.xlsx), sheets:
#   README, QA_summary, household_level, roster, edu_ind, health_ind, nutrition_ind, health_wra,
#   prot_sep_child, prot_hosted_child, checks_all
#
# Run: only when you change the checks (the master skips it when the workbook already exists,
#      unless rebuild_logical_checks <- TRUE).
# =================================================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(stringr); library(purrr); library(readxl); library(openxlsx)
})

stopifnot(exists("path_tool"), exists("path_checks"))

n <- function(x) paste0("suppressWarnings(as.numeric(", x, "))")             # numeric helper text
nk <- function(x) paste0("!is.na(", n(x), ") & ", n(x), " != -999")         # known numeric value
has <- function(var, choice) paste0("stringr::str_detect(dplyr::coalesce(as.character(", var, "), ''), '(^| )", choice, "( |$)')")
yes <- function(var, value = "yes") paste0("dplyr::coalesce(as.character(", var, "), '') == '", value, "'")

chk <- function(dataset, check_id, description, check_to_perform, columns_to_clean) {
  tibble(dataset = dataset, check_id = check_id, description = description,
         check_to_perform = check_to_perform, columns_to_clean = columns_to_clean)
}

fcs_vars <- c("m3269_hh_fcs_staples", "m3270_hh_fcs_pulses", "m3271_hh_fcs_dairy", "m3272_hh_fcs_protein",
              "m3273_hh_fcs_veg", "m3274_hh_fcs_fruits", "m3275_hh_fcs_fat", "m3276_hh_fcs_sugar")
rcsi_vars <- c("fsl_rcsi_rely_on_less_expensive_food", "fsl_rcsi_borrow", "fsl_rcsi_mealsize",
               "fsl_rcsi_mealadult", "fsl_rcsi_mealnb")
lcs_vars <- c("m3332_hh_lcsfs_domasset", "m3333_hh_lcsfs_saving", "m3335_hh_lcsfs_borrowcash",
              "fsl_lcsi_send_family_members_elsewhere", "fsl_lcsi_sell_more_reproductive_animals",
              "fsl_lcsi_reduce_expenses_on_health", "hh_lcsfs_sell_assets", "hh_lcsfs_consume_seed",
              "m3350_hh_lcsfs_drop_school", "hh_lcsfs_sold_house", "hh_lcsfs_sell_female_animal")
# Checks that involve the whole LCS module list only the 3 stress strategies in columns_to_clean,
# otherwise each flag would create 11 cleaning-log rows. Change other LCS answers with
# change_response rows added manually if needed.
lcs_anchor_vars <- "m3332_hh_lcsfs_domasset, m3333_hh_lcsfs_saving, m3335_hh_lcsfs_borrowcash"
adults <- paste0("(dplyr::coalesce(", n("ind_f_age_above18_n"), ", 0) + dplyr::coalesce(", n("ind_m_age_above18_n"), ", 0))")
children <- paste0("(", n("hh_size"), " - ", adults, ")")

# -------------------------------------------------------------------------------------------------
# 1. Household level (Submissions)
# -------------------------------------------------------------------------------------------------
hh <- bind_rows(
  # Demographics
  chk("Submissions", "MOZ_HH_001", "The head of household is reported to be 80 years old or older. Please confirm the age of the head of household.",
      paste0(yes("resp_hoh_yn", "no"), " & ", nk("hoh_age"), " & ", n("hoh_age"), " >= 80"), "hoh_age"),
  chk("Submissions", "MOZ_HH_002", "The respondent is reported to be 80 years old or older. Please confirm the age of the respondent.",
      paste0(nk("resp_age"), " & ", n("resp_age"), " >= 80"), "resp_age"),
  chk("Submissions", "MOZ_HH_003", "Household size is 15 members or more. Please confirm the household size and that all members eat from the same pot and share the same shelter.",
      paste0(nk("hh_size"), " & ", n("hh_size"), " >= 15"), "hh_size"),
  chk("Submissions", "MOZ_HH_004", "No adult (18+) member was listed in the roster. Please confirm the ages of the household members.",
      paste0("!is.na(", n("hh_size"), ") & ", adults, " == 0"), "hh_size, ind_f_age_above18_n, ind_m_age_above18_n"),
  chk("Submissions", "MOZ_HH_005", "The respondent is the son/daughter of the head of household, but the head of household is less than 12 years older than the respondent.",
      paste0(yes("hoh_relationship", "son_daughter"), " & ", nk("hoh_age"), " & ", nk("resp_age"), " & (", n("hoh_age"), " - ", n("resp_age"), ") < 12"),
      "hoh_relationship, hoh_age, resp_age"),
  chk("Submissions", "MOZ_HH_006", "The respondent is the father/mother of the head of household, but the respondent is less than 12 years older than the head of household.",
      paste0(yes("hoh_relationship", "father_mother"), " & ", nk("hoh_age"), " & ", nk("resp_age"), " & (", n("resp_age"), " - ", n("hoh_age"), ") < 12"),
      "hoh_relationship, hoh_age, resp_age"),
  chk("Submissions", "MOZ_HH_007", "The respondent is the grandchild of the head of household, but the head of household is less than 25 years older than the respondent.",
      paste0(yes("hoh_relationship", "grandchild"), " & ", nk("hoh_age"), " & ", nk("resp_age"), " & (", n("hoh_age"), " - ", n("resp_age"), ") < 25"),
      "hoh_relationship, hoh_age, resp_age"),
  chk("Submissions", "MOZ_HH_008", "The respondent is the spouse of the head of household, but the head of household is reported as single, divorced or widowed.",
      paste0(yes("hoh_relationship", "spouse"), " & dplyr::coalesce(as.character(hoh_civil_status), '') %in% c('single', 'divorced', 'widowed')"),
      "hoh_relationship, hoh_civil_status"),

  # Population group / displacement
  chk("Submissions", "MOZ_HH_009", "Population group selected at the start of the interview does not match the displacement status reported in the mobility section (IDP = still displaced; returnee = displaced but back; non-displaced = never forced to flee).",
      paste0("(", yes("population_group", "idp"), " & !", yes("dis_fd_status", "yes_still_displaced"), ") | (",
             yes("population_group", "returnees"), " & !", yes("dis_fd_status", "yes_but_back"), ") | (",
             yes("population_group", "non_displaced"), " & dplyr::coalesce(as.character(dis_fd_status), '') %in% c('yes_still_displaced', 'yes_but_back'))"),
      "population_group, dis_fd_status"),
  chk("Submissions", "MOZ_HH_010", "Date of arrival in the current location is earlier than the date the household was first forced to leave its home.",
      "!is.na(suppressWarnings(as.Date(dis_first_date))) & !is.na(suppressWarnings(as.Date(dis_arrival_date))) & suppressWarnings(as.Date(dis_arrival_date)) < suppressWarnings(as.Date(dis_first_date))",
      "dis_first_date, dis_arrival_date"),
  chk("Submissions", "MOZ_HH_011", "Household reports displacement due to armed conflict that started before October 2017 (start of the conflict in Cabo Delgado). Please confirm the date of first displacement and the reason.",
      paste0(has("dis_reasons", "armed_conflict"), " & !is.na(suppressWarnings(as.Date(dis_first_date))) & suppressWarnings(as.Date(dis_first_date)) < as.Date('2017-10-01')"),
      "dis_reasons, dis_first_date"),
  chk("Submissions", "MOZ_HH_012", "Household reports being displaced only once, but the time between first leaving home and arriving in the current location is more than 2 years.",
      "dplyr::coalesce(as.character(dis_no_times), '') == 'once' & !is.na(suppressWarnings(as.Date(dis_first_date))) & !is.na(suppressWarnings(as.Date(dis_arrival_date))) & as.numeric(suppressWarnings(as.Date(dis_arrival_date)) - suppressWarnings(as.Date(dis_first_date))) > 730",
      "dis_no_times, dis_first_date, dis_arrival_date"),
  chk("Submissions", "MOZ_HH_013", "A non-displaced household is reported to live in an accommodation centre or in a camp/site setting. Please confirm the population group and shelter situation.",
      paste0(yes("population_group", "non_displaced"), " & (", yes("snfi_shelter_type", "accommodation_center"),
             " | dplyr::coalesce(as.character(setting), '') %in% c('camp_formal', 'camp_informal'))"),
      "population_group, snfi_shelter_type, setting"),
  chk("Submissions", "MOZ_HH_014", "The household is itself hosted, but reports hosting displaced persons. Please confirm the shelter situation and the hosting question.",
      paste0(yes("snfi_shelter_type", "hosted"), " & dplyr::coalesce(as.character(dis_persons_hosted_type), '') %in% c('mozambican_idp', 'refugee')"),
      "snfi_shelter_type, dis_persons_hosted_type"),

  # Food security - FCS
  chk("Submissions", "MOZ_HH_015", "Food Consumption Score is 0: no food group was consumed in the last 7 days. Please confirm with the respondent.",
      paste0("!is.na(", n("fcs_score"), ") & ", n("fcs_score"), " == 0"), paste(fcs_vars, collapse = ", ")),
  chk("Submissions", "MOZ_HH_016", "All main food groups were consumed 7 days out of 7 (maximum FCS). Please confirm with the respondent.",
      paste(paste0(n(fcs_vars), " == 7"), collapse = " & "), paste(fcs_vars, collapse = ", ")),
  chk("Submissions", "MOZ_HH_017", "Only condiments were consumed in the last 7 days (all main food groups at 0). This profile almost always reflects a data-entry or comprehension error.",
      paste0(paste(paste0(n(fcs_vars), " == 0"), collapse = " & "), " & ", n("m3277_hh_fcs_condiments"), " > 0"),
      paste(c(fcs_vars, "m3277_hh_fcs_condiments"), collapse = ", ")),
  chk("Submissions", "MOZ_HH_018", "Cereals/tubers (staples) eaten on 2 days or fewer, while meat/fish/eggs were eaten on 5 days or more. Please confirm the food consumption responses.",
      paste0(n("m3269_hh_fcs_staples"), " <= 2 & ", n("m3272_hh_fcs_protein"), " >= 5"), "m3269_hh_fcs_staples, m3272_hh_fcs_protein"),

  # Food security - rCSI / HHS / LCS / FIES
  chk("Submissions", "MOZ_HH_019", "Adults restricted consumption so that children could eat, but no member under 18 was listed in the household.",
      paste0(n("fsl_rcsi_mealadult"), " > 0 & !is.na(", children, ") & ", children, " <= 0"),
      "fsl_rcsi_mealadult, hh_size"),
  chk("Submissions", "MOZ_HH_020", "Very high reduced coping strategy index (rCSI >= 43). Please confirm the frequency of the coping strategies.",
      paste0("!is.na(", n("rcsi_score"), ") & ", n("rcsi_score"), " >= 43"), paste(rcsi_vars, collapse = ", ")),
  chk("Submissions", "MOZ_HH_021", "Poor food consumption (FCS <= 21) but no use of reduced coping strategies in the last 7 days (rCSI = 0).",
      paste0("!is.na(", n("fcs_score"), ") & ", n("fcs_score"), " <= 21 & !is.na(", n("rcsi_score"), ") & ", n("rcsi_score"), " == 0"),
      paste(rcsi_vars, collapse = ", ")),
  chk("Submissions", "MOZ_HH_022", "Acceptable food consumption (FCS > 35) but severe hunger reported (HHS >= 4). Please confirm.",
      paste0("!is.na(", n("fcs_score"), ") & ", n("fcs_score"), " > 35 & !is.na(", n("hhs_score"), ") & ", n("hhs_score"), " >= 4"),
      "m2754_hh_hhs_no_food, m2756_hh_hhs_sleep_hungry, m2758_hh_hhs_no_day_night"),
  chk("Submissions", "MOZ_HH_023", "Household went to sleep hungry in the last 30 days but reported that there was never a lack of food at home (HHS).",
      paste0(yes("m2756_hh_hhs_sleep_hungry"), " & ", yes("m2754_hh_hhs_no_food", "no")),
      "m2754_hh_hhs_no_food, m2756_hh_hhs_sleep_hungry"),
  chk("Submissions", "MOZ_HH_024", "Household went a whole day and night without eating but did not report going to sleep hungry (HHS).",
      paste0(yes("m2758_hh_hhs_no_day_night"), " & ", yes("m2756_hh_hhs_sleep_hungry", "no")),
      "m2756_hh_hhs_sleep_hungry, m2758_hh_hhs_no_day_night"),
  # NOTE: HHS and FIES disagree for ~20% of households on 'no food' and 'slept hungry' (same 30 days).
  # That is a systemic wording/training issue to raise with the FSL team, not a record-level check,
  # so only the most severe item is checked here, in one direction.
  chk("Submissions", "MOZ_HH_027", "HHS reports that a household member went a whole day and night without eating, but the FIES question for the same 30 days says no.",
      paste0(yes("m2758_hh_hhs_no_day_night"), " & ", yes("fsl_fies_whole_day_without_eating", "no")),
      "m2758_hh_hhs_no_day_night, fsl_fies_whole_day_without_eating"),
  chk("Submissions", "MOZ_HH_028", "FIES: household went a whole day without eating but was never worried about not having enough food.",
      paste0(yes("fsl_fies_whole_day_without_eating"), " & ", yes("fsl_fies_worried_about_not_having_enough_food", "no")),
      "fsl_fies_worried_about_not_having_enough_food, fsl_fies_whole_day_without_eating"),
  chk("Submissions", "MOZ_HH_029", "Poor food consumption (FCS <= 21) but the household reported no need to use any livelihood coping strategy.",
      paste0("!is.na(", n("fcs_score"), ") & ", n("fcs_score"), " <= 21 & ", paste(paste0(yes(lcs_vars, "10")), collapse = " & ")),
      lcs_anchor_vars),
  chk("Submissions", "MOZ_HH_030", "All livelihood coping strategies were answered 'not applicable'. Please confirm that the household has none of these assets/options.",
      paste(paste0(yes(lcs_vars, "9999")), collapse = " & "), lcs_anchor_vars),
  chk("Submissions", "MOZ_HH_031", "Household withdrew children from school to cope with lack of food, but has no school-aged child.",
      paste0(yes("m3350_hh_lcsfs_drop_school", "30"), " & !is.na(", n("ind_age_schooling_n"), ") & ", n("ind_age_schooling_n"), " == 0"),
      "m3350_hh_lcsfs_drop_school, ind_age_schooling_n"),
  chk("Submissions", "MOZ_HH_032", "Household sold its last female productive animal but does not report own production (agriculture/livestock) as an income source.",
      paste0(yes("hh_lcsfs_sell_female_animal", "30"), " & !", has("cm_income_source", "own_production")),
      "hh_lcsfs_sell_female_animal, cm_income_source"),

  # Income
  chk("Submissions", "MOZ_HH_033", "Income source percentages do not add up to 100%.",
      paste0("!is.na(", n("cm_sum_income_source_prop"), ") & ", n("cm_sum_income_source_prop"), " != 100 & ",
             n("cm_sum_income_source_prop"), " > 0 & !stringr::str_detect(dplyr::coalesce(as.character(cm_income_source), ''), '(^| )(none|dnk|pnta)( |$)')"),
      "cm_income_source, cm_sum_income_source_prop"),
  chk("Submissions", "MOZ_HH_034", "Humanitarian assistance is reported as an income source, but the household reported not receiving aid in the last 12 months.",
      paste0(has("cm_income_source", "assistance"), " & ", yes("aap_received_assistance_12m", "no")),
      "cm_income_source, aap_received_assistance_12m"),

  # Shelter / HLP
  chk("Submissions", "MOZ_HH_035", "The household owns its shelter but reports a risk of eviction from the housing/shelter plot. Please confirm.",
      paste0(yes("hlp_occupancy", "ownership"), " & dplyr::coalesce(as.character(hlp_risk_eviction), '') %in% c('eviction_from_housing_shelter_plot', 'eviction_from_both_housing_and_agriculture_land')"),
      "hlp_occupancy, hlp_risk_eviction"),
  chk("Submissions", "MOZ_HH_036", "The household rents its shelter but lives in a tent or emergency shelter.",
      paste0(yes("hlp_occupancy", "rented"), " & (dplyr::coalesce(as.character(snfi_shelter_type), '') %in% c('tent', 'emergency_shelter') | dplyr::coalesce(as.character(snfi_shelter_type_individual_by_materials), '') %in% c('tent', 'emergency_shelter'))"),
      "hlp_occupancy, snfi_shelter_type, snfi_shelter_type_individual_by_materials"),
  chk("Submissions", "MOZ_HH_037", "The household is hosted by another household but reports owning the shelter.",
      paste0(yes("snfi_shelter_type", "hosted"), " & ", yes("hlp_occupancy", "ownership")),
      "snfi_shelter_type, hlp_occupancy"),
  chk("Submissions", "MOZ_HH_038", "The household cannot sleep properly because of a lack of materials, but no blankets, sleeping mats or mosquito nets are listed among missing essential items.",
      paste0(has("snfi_fds_sleeping_issue", "lack_materials"), " & !", has("snfi_fds_nfi_essential", "(blankets|sleeping_mats|mosquito_nets)")),
      "snfi_fds_sleeping_issue, snfi_fds_nfi_essential"),

  # WASH
  chk("Submissions", "MOZ_HH_040", "More than 20 households reported sharing the same toilet facility. Please confirm.",
      paste0(yes("wash_sanitation_facility_sharing_yn"), " & ", nk("wash_sanitation_facility_sharing_n"), " & ", n("wash_sanitation_facility_sharing_n"), " > 20"),
      "wash_sanitation_facility_sharing_n"),
  chk("Submissions", "MOZ_HH_041", "The household practises open defecation but reports sharing the toilet facility with other households.",
      paste0(yes("wash_sanitation_facility", "open_defecation"), " & ", yes("wash_sanitation_facility_sharing_yn")),
      "wash_sanitation_facility, wash_sanitation_facility_sharing_yn"),
  chk("Submissions", "MOZ_HH_042", "Main drinking water source is piped into the dwelling/compound, but collecting water takes more than 30 minutes.",
      paste0("dplyr::coalesce(as.character(wash_drinking_water_source), '') %in% c('piped_dwelling', 'piped_compound') & ", nk("wash_drinking_water_time_int"), " & ", n("wash_drinking_water_time_int"), " > 30"),
      "wash_drinking_water_source, wash_drinking_water_time_int"),
  chk("Submissions", "MOZ_HH_043", "A water container was reportedly filled more than 20 times yesterday. Please confirm the number of trips.",
      paste(paste0("dplyr::coalesce(", n(c("wash_container_size10_time_int", "wash_container_size15_time_int", "wash_container_size20_time_int", "wash_container_size25_time_int")), ", 0) > 20"), collapse = " | "),
      "wash_container_size10_time_int, wash_container_size15_time_int, wash_container_size20_time_int, wash_container_size25_time_int"),

  # Health / protection / AAP
  chk("Submissions", "MOZ_HH_044", "Nearest functional health facility is reported to be more than 3 hours away. Please confirm the time in minutes.",
      paste0(nk("health_facility_time"), " & ", n("health_facility_time"), " > 180"), "health_facility_time"),
  chk("Submissions", "MOZ_HH_045", "A household member needed healthcare in the last 3 months, but no member was recorded with a healthcare need in the individual health loop.",
      paste0(yes("health_hh_any_member_required_healthcare"), " & !is.na(", n("health_ind_healthcare_needed_n"), ") & ", n("health_ind_healthcare_needed_n"), " == 0"),
      "health_hh_any_member_required_healthcare, health_ind_healthcare_needed_n"),
  chk("Submissions", "MOZ_HH_046", "The household hosts a separated/unaccompanied child, but the number of hosted children is 0 or larger than the household size.",
      paste0(yes("prot_hosting_child_sep"), " & !is.na(", n("prot_num_child_sep_hosted"), ") & (", n("prot_num_child_sep_hosted"), " <= 0 | ", n("prot_num_child_sep_hosted"), " > ", n("hh_size"), ")"),
      "prot_hosting_child_sep, prot_num_child_sep_hosted, hh_size"),
  chk("Submissions", "MOZ_HH_047", "The household has a child living elsewhere, but the number of children not living with the household is 0 or 10 or more.",
      paste0(yes("prot_child_sep"), " & !is.na(", n("prot_num_child_sep"), ") & (", n("prot_num_child_sep"), " <= 0 | ", n("prot_num_child_sep"), " >= 10)"),
      "prot_child_sep, prot_num_child_sep")
)

# -------------------------------------------------------------------------------------------------
# 2. Repeat sheets
# -------------------------------------------------------------------------------------------------
ros <- bind_rows(
  chk("roster", "MOZ_ROS_001", "Household member is reported to be 100 years old or older. Please confirm the age.",
      paste0(nk("ind_age"), " & ", n("ind_age"), " >= 100"), "ind_age"),
  chk("roster", "MOZ_ROS_002", "The first member of the roster must be the respondent, but the age or gender differs from the respondent's age/gender reported at the start of the interview.",
      paste0(n("ind_pos"), " == 1 & ((", nk("ind_age"), " & !is.na(", n("resp_age"), ") & ", n("ind_age"), " != ", n("resp_age"), ") | (!is.na(ind_gender) & !is.na(resp_gender) & as.character(ind_gender) != as.character(resp_gender)))"),
      "ind_age, ind_gender"),
  chk("roster", "MOZ_ROS_003", "All six Washington Group questions were answered 'cannot do at all'. Please confirm.",
      paste(paste0(yes(c("wgq_vision", "wgq_hearing", "wgq_mobility", "wgq_cognition", "wgq_self_care", "wgq_communication"), "cannot_do_at_all")), collapse = " & "),
      "wgq_vision, wgq_hearing, wgq_mobility, wgq_cognition, wgq_self_care, wgq_communication"),
  chk("roster", "MOZ_ROS_004", "Age of the household member is unknown (-999). Please try to obtain an estimated age.",
      paste0(n("ind_age"), " == -999"), "ind_age")
)

edu <- bind_rows(
  chk("edu_ind", "MOZ_EDU_001", "The child attends a grade that is too high for the age (grade number > age - 4). Please confirm the age and grade.",
      paste0(yes("edu_access"), " & stringr::str_detect(dplyr::coalesce(edu_level_grade, ''), '^grade_') & ", nk("edu_ind_age"),
             " & suppressWarnings(as.numeric(stringr::str_remove(edu_level_grade, 'grade_'))) > (", n("edu_ind_age"), " - 4)"),
      "edu_ind_age, edu_level_grade"),
  chk("edu_ind", "MOZ_EDU_002", "The child is 8 years old or older but attends kindergarten. Please confirm the age and level.",
      paste0(yes("edu_access"), " & ", yes("edu_level_grade", "kindergarten"), " & ", nk("edu_ind_age"), " & ", n("edu_ind_age"), " >= 8"),
      "edu_ind_age, edu_level_grade"),
  chk("edu_ind", "MOZ_EDU_003", "The child is reported as having already graduated, but is younger than 16.",
      paste0(yes("edu_barrier", "child_graduated"), " & ", nk("edu_ind_age"), " & ", n("edu_ind_age"), " < 16"),
      "edu_ind_age, edu_barrier"),
  chk("edu_ind", "MOZ_EDU_004", "Child marriage or pregnancy is reported as the barrier to education for a child younger than 12. Please confirm.",
      paste0("dplyr::coalesce(as.character(edu_barrier), '') %in% c('child_marriage') & ", nk("edu_ind_age"), " & ", n("edu_ind_age"), " < 12"),
      "edu_ind_age, edu_barrier")
)

hlt <- bind_rows(
  chk("health_ind", "MOZ_HLT_001", "Healthcare was needed but not received, and the only barrier reported is 'none'.",
      paste0(yes("health_ind_healthcare_received", "no"), " & ", has("health_barriers_unmet", "none")),
      "health_ind_healthcare_received, health_barriers_unmet"),
  chk("health_ind", "MOZ_HLT_002", "Natal services or safe delivery were needed for a male member or a member outside 10-55 years.",
      paste0("(", has("health_ind_healthcare_needed_type", "(natal_services|safe_delivery)"), ") & (", yes("health_ind_gender", "male"),
             " | (", nk("health_ind_age"), " & (", n("health_ind_age"), " < 10 | ", n("health_ind_age"), " > 55)))"),
      "health_ind_healthcare_needed_type, health_ind_age, health_ind_gender"),
  chk("health_ind", "MOZ_HLT_003", "Healthcare was received but the location is 'did not seek care'.",
      paste0(yes("health_ind_healthcare_received"), " & ", has("health_ind_healthcare_received_location", "did_not_seek_care")),
      "health_ind_healthcare_received, health_ind_healthcare_received_location")
)

nut <- bind_rows(
  chk("nutrition_ind", "MOZ_NUT_001", "The child was reported as exclusively breastfed yesterday, but other liquids (water, milk, juice, porridge, etc.) were also reported.",
      paste0(yes("nut_ind_breastfed_yesterday"), " & ", has("nut_ind_child_liquids_taken",
             "(plain_water|water_with_sugar_salt_lemon|fruit_or_vegetable_juice|infant_formula_animal_powdered_milk|thin_porrdge_gruel|soup_clear_broth|sweetened_beverages|tea_or_coffee|other)")),
      "nut_ind_breastfed_yesterday, nut_ind_child_liquids_taken"),
  chk("nutrition_ind", "MOZ_NUT_002", "The child was reported as exclusively breastfed yesterday, but breast milk was not among the liquids taken.",
      paste0(yes("nut_ind_breastfed_yesterday"), " & !is.na(nut_ind_child_liquids_taken) & !", has("nut_ind_child_liquids_taken", "breast_milk")),
      "nut_ind_breastfed_yesterday, nut_ind_child_liquids_taken")
)

wra <- bind_rows(
  chk("health_wra", "MOZ_WRA_001", "First antenatal care visit reported after 42 weeks of pregnancy. Please confirm the number of weeks.",
      paste0(nk("health_antenatal_care_months"), " & ", n("health_antenatal_care_months"), " > 42"), "health_antenatal_care_months"),
  chk("health_wra", "MOZ_WRA_002", "More than 15 antenatal care visits during the pregnancy. Please confirm.",
      paste0(nk("health_antenatal_care_n"), " & ", n("health_antenatal_care_n"), " > 15"), "health_antenatal_care_n"),
  chk("health_wra", "MOZ_WRA_003", "Antenatal care was received, but the number of visits is 0.",
      paste0(yes("health_antenatal_care_yn"), " & !is.na(", n("health_antenatal_care_n"), ") & ", n("health_antenatal_care_n"), " == 0"),
      "health_antenatal_care_yn, health_antenatal_care_n"),
  chk("health_wra", "MOZ_WRA_004", "Birth took place in a health facility but was assisted only by a traditional birth attendant, a relative/friend or nobody.",
      paste0("stringr::str_detect(dplyr::coalesce(health_birth_location, ''), '^(government|private|ngo|other_government|other_private|other_ngo)') & dplyr::coalesce(health_birth_assistance, '') %in% c('traditional_birth_attendant', 'relative_friend', 'none')"),
      "health_birth_location, health_birth_assistance")
)

sep <- bind_rows(
  chk("prot_sep_child", "MOZ_SEP_001", "Child living elsewhere reportedly left to get married but is younger than 15. Please confirm (sensitive - handle with care).",
      paste0(yes("prot_child_sep_reason", "left_married"), " & ", nk("prot_child_sep_age"), " & ", n("prot_child_sep_age"), " < 15"),
      "prot_child_sep_age, prot_child_sep_reason"),
  chk("prot_sep_child", "MOZ_SEP_002", "Child living elsewhere reportedly left for employment but is younger than 12. Please confirm (possible child labour - handle with care).",
      paste0(yes("prot_child_sep_reason", "left_employment"), " & ", nk("prot_child_sep_age"), " & ", n("prot_child_sep_age"), " < 12"),
      "prot_child_sep_age, prot_child_sep_reason")
)

hos <- bind_rows(
  chk("prot_hosted_child", "MOZ_HOS_001", "Hosted unaccompanied/separated child is under 1 year old. Please confirm the age.",
      paste0(nk("prot_hosted_child_sep_age"), " & ", n("prot_hosted_child_sep_age"), " < 1"), "prot_hosted_child_sep_age")
)

all_checks <- list(household_level = hh, roster = ros, edu_ind = edu, health_ind = hlt, nutrition_ind = nut,
                   health_wra = wra, prot_sep_child = sep, prot_hosted_child = hos) |>
  map(~ mutate(.x, variables_to_clean = columns_to_clean))

# -------------------------------------------------------------------------------------------------
# 3. QA: every variable referenced must exist in the tool (or be a known exported calculate)
# -------------------------------------------------------------------------------------------------
tool_vars <- read_excel(path_tool, "survey", col_types = "text") |> pull(name) |> na.omit() |> as.character()
exported_calcs <- c("fcs_score", "rcsi_score", "hhs_score", "lcs_score", "fies_score")
# repeat-level copies of household variables added by script 1 (main context)
context_vars <- c("resp_age", "resp_gender")

r_words <- c("suppressWarnings", "as", "numeric", "is", "na", "dplyr", "coalesce", "character", "stringr", "str_detect",
             "str_remove", "Date", "c", "TRUE", "FALSE", "NA", "grade_", "")
qa <- imap_dfr(all_checks, function(df, sh) {
  map_dfr(seq_len(nrow(df)), function(i) {
    expr <- df$check_to_perform[i]
    expr_no_str <- str_remove_all(expr, "'[^']*'")
    toks <- unique(str_extract_all(expr_no_str, "[A-Za-z_][A-Za-z0-9_\\.]*")[[1]])
    toks <- setdiff(toks, c(r_words, "in", "as.numeric", "is.na", "as.character", "as.Date"))
    toks <- toks[!str_detect(toks, "^(dplyr|stringr|suppressWarnings|as|is)$")]
    missing <- setdiff(toks, c(tool_vars, exported_calcs, context_vars))
    parse_ok <- !inherits(try(parse(text = expr), silent = TRUE), "try-error")
    tibble(sheet = sh, check_id = df$check_id[i], variables = paste(toks, collapse = ", "),
           missing_in_tool = paste(missing, collapse = ", "),
           status = if (!parse_ok) "FAIL: R expression does not parse" else if (length(missing)) "FAIL: variable(s) not in tool" else "PASS")
  })
})

if (any(qa$status != "PASS")) {
  warning("Some MOZ logical checks failed QA - see QA_summary sheet:\n",
          paste(capture.output(print(filter(qa, status != "PASS"))), collapse = "\n"), call. = FALSE)
}

readme <- tibble(
  item = c("Workbook purpose", "Tool", "Execution", "Variables", "Rule for descriptions", "How to add a check", "Datasets"),
  detail = c(
    "Logical checks for REACH MSNA 2026 Mozambique, written against the final MOZ Kobo tool (XML names).",
    paste0("Validated against: ", basename(path_tool), " on ", format(Sys.Date())),
    "Read by script 1 and run by script 3 (cleaningtools::check_logical_with_list). check_to_perform is a vectorised R expression: TRUE = flag.",
    "Data are text after import: numbers are wrapped in suppressWarnings(as.numeric()); -999 (don't know) is excluded from numeric thresholds.",
    "Descriptions explain why a record is flagged; they do not mean the value is wrong. Enumerators/field team confirm or correct.",
    "Add a row in the right sheet with a unique check_id, then re-run R-scripts/Source scripts/1b.Build_MOZ_logical_checks_workbook.R (or edit this file and set rebuild_logical_checks <- FALSE).",
    "household_level = main sheet; roster, edu_ind, health_ind, nutrition_ind (nut_ind), health_wra, prot_sep_child (prot_separated_children_breakdown), prot_hosted_child (prot_hosted_separated_children_breakdown)."
  )
)

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#EE5859", fontColour = "white", wrapText = TRUE)
add_sheet <- function(name, df, widths = NULL) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hs)
  freezePane(wb, name, firstRow = TRUE)
  setColWidths(wb, name, cols = seq_along(df), widths = widths %||% rep(25, ncol(df)))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
add_sheet("README", readme, c(25, 120))
add_sheet("QA_summary", qa, c(18, 16, 70, 30, 30))
for (sh in names(all_checks)) add_sheet(sh, all_checks[[sh]], c(16, 14, 70, 90, 40, 40))
add_sheet("checks_all", bind_rows(all_checks), c(16, 14, 70, 90, 40, 40))
saveWorkbook(wb, path_checks, overwrite = TRUE)

message("1b. MOZ logical checks workbook written: ", path_checks, " (",
        nrow(bind_rows(all_checks)), " checks, ", sum(qa$status == "PASS"), " passed QA)")
