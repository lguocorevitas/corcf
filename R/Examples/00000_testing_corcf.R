# Example code for testing corcf function
library(data.table)
library(stringr)
library(haven)
library(lubridate)
library(glue)
library(tidyverse)
# for word output
library(officer)
library(flextable)
# conflicted::conflicts_prefer(flextable::width)
library(devtools)
library(roxygen2)
# roxygen2::roxygenise()
# install.packages("devtools")
# devtools::document()
detach("package:corcf", unload = TRUE, character.only = TRUE)
remotes::install_github("lguocorevitas/corcf", force = TRUE)
library(corcf)
ls("package:corcf")

# Identify location of two datasets to be compared
tdy_date <- Sys.Date()
tdy_year  <- sprintf("%04d", year(tdy_date))
tdy_month <- sprintf("%02d", month(tdy_date))

cut_date    <- as.Date(glue("{tdy_year}-{tdy_month}-01"))
sharepoint <- "~/../../Thermo Fisher Scientific/"
dir_ra_monthly  <- glue("{sharepoint}/Biostat Data Files - RA/monthly/")
analytic_data  <- glue("{dir_ra_monthly}/{tdy_year}/{cut_date}/")

# data name and ids
data_name <-"visits_calc"
test_data <- glue("{analytic_data}/rewrite/RA_{data_name}_{cut_date}.dta")
current_data  <- glue("{analytic_data}/2_3_keyvisitvars_{cut_date}.dta")
id_cols <- c("id", "visitdate")

master_df <- read_dta(test_data)
using_df  <- read_dta(current_data)
if (!"id" %in% names(using_df)) using_df <- using_df %>% mutate(id = subject_number)

# run corcf function
res <- corcf::corcf(
  master = master_df,
  using  = using_df,
  vars   = "_all",
  id     = id_cols
  # ,verbose1 = 200
)

# optional
# res$ecode
# # 9 = at least one variable had differing values
# # 106 means you hit at least one type mismatch
# names(res$label_conflicts)
# res$per_variable

# generate a word document for report

corcf::write_corcf_word(
  res,
  master_path = test_data,
  using_path  = current_data,
  id_cols     = id_cols,
  path = glue("{analytic_data}/rewrite/QC/corcf_results_{data_name}_{tdy_date}.docx"),
  report_date = Sys.Date()
)

# Another example, testing UAT/prod raw data: bv_subjects

data_name <-"bv_subjects"

prev_data <- glue("{dir_ra_monthly}/2026/2026-03-01/bv_raw/{data_name}.dta")
current_data  <- glue("{analytic_data}/bv_raw/{data_name}.dta")
id_cols <- c("subject_number")

master_df <- read_dta(current_data)
using_df  <- read_dta(prev_data)
# to compare specific variables
# var_list <- c("c_birth_year", "diagnosis_date", "diagnosis_year", "exit_form_date")

# run corcf function
res <- corcf::corcf(
  master = master_df,
  using = using_df,
  vars = "_all",
  id = id_cols,
  # verbose = TRUE,
  # verbose_vars = c("c_birth_year", "diagnosis_date")
)

names(res)
names(res$verbose_details)
res$verbose_details$c_birth_year

res <- corcf::corcf(
  master = master_df,
  using  = using_df,
  # vars = var_list,
  vars   = "_all",
  id     = id_cols,
  verbose = TRUE,
  verbose_vars = c(
    "c_birth_year",
    "diagnosis_date",
    "diagnosis_year",
    "exit_form_date"
  )
  # ,verbose1 = 200
)

# generate a word document for report

corcf::write_corcf_word(
  res,
  master_path = current_data,
  using_path  = prev_data,
  id_cols     = id_cols,
  path = glue("{analytic_data}/rewrite/QC/corcf_results_{data_name}_{tdy_date}_verbose.docx"),
  report_date = Sys.Date()
)
