---
title: "MSNA 2026 Mozambique – Data and tool limitations and their impact on the analysis"
date: "16/11/2026"
---

## Summary

The data (21,445 households, 126 districts, export of 01/10/2026) are usable, but five limitations affect the analysis and must be stated in the methodology note:

1. **The export has no interview metadata** (time, date, enumerator, GPS). Data collection quality cannot be checked by duration, by day or by enumerator.
2. **The HHS and FIES modules disagree** for 15–21% of households over the same 30-day period. The severity of hunger depends on which indicator is used.
3. **The question on the first antenatal visit mixes weeks and months** (72% of answers are ≤ 9). The early antenatal care indicator is not reliable as it stands.
4. **The OCHA population frame does not cover 784 interviews** (21 strata with zero population) and has 8 strata with more interviews than households. Weights in those areas rest on a methodological decision (pooling with the non-displaced), not on official figures.
5. **33 of the 159 districts were not surveyed.** National and provincial results represent the 126 covered districts, not the whole country.

The figures in this document come from the test run of the cleaning pipeline on 16/11/2026. They are **preliminary** until the cleaning logs have been reviewed.

## Abbreviations

| Abbreviation | Meaning |
| --- | --- |
| admin1 / admin2 / admin3 | Province / district / administrative post (administrative levels 1, 2 and 3) |
| CI | Confidence interval |
| DTM | Displacement Tracking Matrix (IOM) |
| FCS | Food Consumption Score |
| FIES | Food Insecurity Experience Scale |
| FS | Food security |
| GPS | Global Positioning System (geographic coordinates) |
| HHS | Household Hunger Scale |
| IDP | Internally Displaced Person |
| INGD | Instituto Nacional de Gestão e Redução do Risco de Desastres (National Institute for Disaster Risk Management and Reduction) |
| IOM | International Organization for Migration |
| IPC | Integrated Food Security Phase Classification |
| LCS | Livelihood Coping Strategies |
| LSG | Living Standards Gap |
| MSNA | Multi-Sector Needs Assessment |
| OCHA | United Nations Office for the Coordination of Humanitarian Affairs |
| p-code | Standard place code of an administrative unit (e.g. MZ0101 = Ancuabe) |
| R21 / R22 | DTM round 21 / round 22 |
| rCSI | reduced Coping Strategies Index |
| XML | Format in which Kobo exports variable names and answer codes ("XML values and headers") |

## Data export limitations

The file `REACH_MOZ_Data_MSNA_01102026.xlsx` is not a standard Kobo export: it is a processed, labelled version without metadata.

| Limitation | Evidence | Consequence |
| --- | --- | --- |
| Headers and answers are English labels, not XML names/codes | 1,029 columns can only be identified through the order of the tool; repeated labels ("If other, please specify:2") | A conversion step was needed (script 0). A label or order change in a new export can misalign columns without a visible error; the conversion log must be reviewed for every export |
| Select multiple: the parent column is text of labels separated by spaces | "Insecurity Armed conflict" cannot be split reliably | The parent column was rebuilt from the 0/1 columns; the original text is not used |
| No `start`, `end`, `today`, `_submission_time` | No interview date/time column | No duration check; no daily tracker; no removal of the pilot by date; no analysis by data collection period |
| No `enum_code`, `deviceid`, `audit` | Only `enum_gender` exists | No enumerator-level checks (patterns, speed, soft duplicates by the same enumerator); `enum_id` empty in the logs |
| No GPS | No coordinates | No check of location against the reported administrative post; no point-level maps |
| `enum_gender` with two coding schemes | M 8,283, F 6,764, male 3,562, female 2,836 | Sign that the export combines different sources or versions; harmonised to male/female |
| No `point_number` | — | The Kobo `_id` is used as the identifier in the logs; field teams must locate the record by `_id` |

Overall risk: since we do not know what processing was done before the export (removals, recoding, computed scores), cleaning cannot tell a data collection error from a processing error.

## Kobo tool limitations

Three tool issues directly affect indicators; the others mainly affect the cleaning work.

| Limitation | Evidence | Consequence |
| --- | --- | --- |
| `health_antenatal_care_months` asks for **weeks** of pregnancy, but its name says months | 3,132 answers: 2,261 (72%) between 0 and 9, 868 between 10 and 42, 3 above 42 | Many enumerators likely recorded months. The "first antenatal visit in the 1st trimester" indicator is biased and cannot be recovered from the data alone |
| Column `health_antenatal_care_weeks_first_visit` in the export, not in the tool | Identical to `health_antenatal_care_months` | Duplicate created during processing; not a second measure |
| HHS and FIES ask the same thing for the same 30 days | Disagreement: no food at home 20.8%, slept hungry 16.0%, whole day and night without eating 15.2% | The prevalence of severe hunger changes with the module used. Likely a wording, translation or training effect, not isolated record errors |
| Choice labels changed between the version used for data collection and the final version | Some select-multiple choices have a different label in the export than in the final tool | Converted without loss, but confirms the tool changed during data collection |
| 52 "specify other" questions | 4,870 texts in the household sheet and loops; 76,215 rows in the review log | Heavy recoding workload; until it is done, the "other" category is inflated |
| Wide constraints on numeric questions | E.g. 0–50 for antenatal weeks and visits; ages up to 120 | Kobo did not block implausible values; they are left to the logical checks |
| No explicit identification of the respondent in the roster | The rule "the 1st member is the respondent" is only checked afterwards (`MOZ_ROS_002`) | Respondent-level indicators depend on that rule having been followed |

For the next MSNA: rename `health_antenatal_care_months`, fix the unit in the label and in the constraint (0–42 weeks), and review the translation of the HHS and FIES questions side by side.

## Population frame and weighting limitations

Weights use the "Baseline data" sheet of the OCHA file (159 districts; IDPs and returnees from DTM round 22; non-displaced = total − IDPs − returnees), with district × population group strata.

| Limitation | Evidence | Treatment / consequence |
| --- | --- | --- |
| Groups interviewed that OCHA reports with zero population | 784 interviews in 21 strata: IDPs in Maputo City (98), Bilene (93), Boane (90), Marracuene (85), Palma (38)…; returnees in Balama (99), Chókwè (41), Chigubo (30)… | Weighted with the non-displaced of the same district (decision of 16/11/2026). IDP/returnee totals in those districts come from the sample, not from an official figure |
| More interviews than estimated households | 8 strata, 222 interviews. E.g. returnees in Chemba: 40 interviews for 36 people (≈ 6 households) | The frame under-counts these groups; their weights are very low (minimum ≈ 0.0005) and these households barely count in aggregated results |
| Population in people, not households | OCHA columns are individuals | Converted by dividing by the mean household size of the stratum in the sample; errors in `hh_size` carry over to the weights |
| Districts without interviews | 33 of 159 districts; 37 strata with population and no interview | Results represent the 126 surveyed districts, not the country |
| Two population versions in the same file | Sheet1/Sheet4 (Cabo Delgado only, DTM R21): Ancuabe 199,392; Baseline (R22): 193,878 | Baseline used; confirm with OCHA which version is the reference |
| Widely dispersed weights | 0.0005 to 9.9; Kish design effect ≈ 2.3 | The effective national sample size is about 9,400 households, not 21,445; margins of error are larger than the sample suggests |

District × group results do not depend on the weights (the weight is constant within a stratum). The limitations above affect results that combine strata: provincial, national, total IDPs.

## Cleaning process limitations

Cleaning covers the consistency of the answers, but not how the interviews were conducted.

| Limitation | Evidence | Consequence |
| --- | --- | --- |
| Enumerator behaviour checks not possible | No duration, date, enumerator or GPS | Short, fabricated or off-site interviews cannot be detected; the main quality risk that remains uncontrolled |
| Soft duplicates only within the same administrative post | Running all 21,445 at once would need tens of GB of memory; 10 exact-profile duplicates found | Duplicates across different posts are not detected |
| Logical checks only cross-check questions | 64 checks; 24,461 records flagged in the test run | They catch inconsistencies, not false but consistent answers |
| HHS/FIES disagreement checked in one direction only | `MOZ_HH_027`: whole day and night without eating in HHS but not in FIES | The other disagreements (15–21%) remain in the data; handle them in the analysis, not in cleaning |
| "Missing ≥ 50%" counts conditional questions | 1,413 households (6.6%) flagged | Many flags are false positives; review with judgement |
| High review volume | 126 district logbooks; 76,215 "specify other" rows | Risk of rushed review (bulk `no_action`) and delays; recode frequent "other" answers by rule rather than row by row |

## Impact on the analysis

The limitations with the largest effect on results are the HHS/FIES disagreement, the antenatal question and the weights in areas outside the OCHA frame.

| Limitation | Indicators affected | Likely effect | How to report |
| --- | --- | --- | --- |
| HHS and FIES disagree (15–21%) | Severe hunger (HHS), FIES, food security composites and FS LSG | Prevalence changes with the module chosen | Choose a primary module (HHS, for alignment with IPC), report the other in an annex and explain the difference |
| Weeks/months in the 1st antenatal visit | Antenatal care in the 1st trimester | Biased, direction unknown | Do not publish this indicator; use only "received antenatal care" and number of visits |
| 784 interviews in strata without OCHA population | Totals and % of IDPs and returnees in Maputo, Gaza, Palma, Balama, etc.; provincial and national aggregates | These households weigh like the district's non-displaced | Unweighted district × group results; footnote on group aggregates |
| 8 strata with more interviews than households | National and provincial IDP aggregates | These households barely count (weights ≈ 0.0005–0.003) | Methodology note; sensitivity run with trimmed weights if the group is central |
| 33 districts not surveyed | All national and provincial results | They represent 126 districts, not the country | State coverage in every national chart and table |
| Dispersed weights (design effect ≈ 2.3) | All weighted results | Confidence intervals ≈ 1.5 times wider than in an unweighted sample | Always compute with the design (stratum + weight, e.g. srvyr); never plain weighted % without CI |
| No interview metadata | All | Fabrication or rushed interviews not detected | State the limitation; ask the data collection partner for supervision evidence |
| Scores already computed in the export (FCS, rCSI, HHS, LCS, FIES) | Food security categories | Method and thresholds unknown | Recompute from the questions (e.g. humind) and compare before use |
| "Specify other" not yet recoded | Income sources, displacement reasons, barriers | "Other" category inflated | Analyse only after applying the "specify other" log |

## Recommendations

The first three address most of the risk and should be done before the analysis.

- [ ] Ask the data collection partner for the Kobo export in "XML values and headers" with `start`, `end`, `_submission_time`, `enum_code` and GPS, to run the duration and enumerator-level checks
- [ ] Confirm with OCHA which population version is the reference (Baseline R22 vs Sheet1/Sheet4) and request figures for the 21 strata with zero population (INGD or another DTM round for Maputo and Gaza)
- [ ] Agree with the food security team which module (HHS or FIES) is the primary one and document the disagreement
- [ ] Drop the first-trimester antenatal visit indicator from the analysis and fix the question in the next tool
- [ ] Recompute FCS, rCSI, HHS, LCS and FIES from the questions and compare with the scores in the export
- [ ] Finish reviewing the cleaning logs and "specify other" before producing any table
- [ ] Include in the methodology note: coverage (126 of 159 districts), strata without population, design effect and missing metadata

Reference files in the `MSNA_cleaning_MOZ` project: `inputs/REACH_MOZ_MSNA2026_data_xml_conversion_log.xlsx` (conversion), `inputs/REACH_MOZ_MSNA2026_logical_checks.xlsx` (checks), `docs_test_run/flags_by_check_test_run_2026-11-16.xlsx` (flags) and the weights file produced by `computing_weights.R`.
