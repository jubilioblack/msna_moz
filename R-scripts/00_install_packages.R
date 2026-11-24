# Install required packages for the REACH MSNA 2026 Mozambique cleaning pipeline
required_packages <- c("tidyverse", "readxl", "openxlsx", "writexl", "lubridate", "fs", "crayon", "janitor",
                       "stringr", "purrr", "rlang", "tidyselect", "remotes")
missing_required <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required) > 0) install.packages(missing_required)

# cleaningtools (IMPACT) is required: logical checks, cleaning-log export, create_clean_data
if (!requireNamespace("cleaningtools", quietly = TRUE)) {
  remotes::install_github("impact-initiatives/cleaningtools", upgrade = "never")
}
