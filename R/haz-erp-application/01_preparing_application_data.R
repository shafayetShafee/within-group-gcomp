# ==============================================================================
# Setups -----------------------------------------------------------------------
# ==============================================================================

library(dplyr)
library(tidyr)
library(forcats)
library(haven)
library(sjlabelled)


# ==============================================================================
# Reading the MICS Survey data -------------------------------------------------
# ==============================================================================

ch_data <- sjlabelled::read_spss(here::here("mics_raw_data/ch.sav"))
wm_data <- sjlabelled::read_spss(here::here("mics_raw_data/wm.sav"))


# ==============================================================================
# Processing data for joining --------------------------------------------------
# ==============================================================================

## UF1 => Cluster Number
## UF2 => Household Number
## UF4 => Mother Line Number
## CAGE => Child Age (in Months)

oldest_ch_df <- ch_data |>
  group_by(UF1, UF2, UF4) |>
  slice_max(order_by = CAGE, n = 1, with_ties = FALSE) |>
  ungroup()

joined <- wm_data |>
  inner_join(
    oldest_ch_df,
    by = join_by(HH1, HH2, WM3 == UF4),
    suffix = c("_wm", "_ch")
  )


# ==============================================================================
# Outcome, Treatment & Confounders ---------------------------------------------
# ==============================================================================

## `melevel` Mother's education
## 0	Pre-primary or none
## 1	Primary
## 2	Secondary
## 3	Higher secondary+
## 9	Missing/DK

haz_erp_df <- joined |>
  filter(melevel != 9) |>
  mutate(
    melevel = fct_recode(
      melevel,
      `0` = "0",
      `0` = "1",
      `1` = "2",
      `1` = "3"
    ),
    place_of_res = factor(
      HH6_wm,
      levels = c(1, 2),
      labels = c("Urban", "Rural")
    ),
    wealth_index = factor(
      windex5_wm,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Poorest", "Second", "Middle", "Fourth", "Richest")
    ),
    wealth_index = fct_recode(
      wealth_index,
      "poor" = "Poorest",
      "poor" = "Second",
      "middle" = "Middle",
      "rich" = "Fourth",
      "rich" = "Richest"
    ),
    # Age at first birth
    age_f_birth = WB4 - AN4,
    erp = case_when(
      age_f_birth <= 19 ~ 1, # Adolescent pregnancy
      age_f_birth >= 20 & age_f_birth <= 49 ~ 0, # Normal
      TRUE ~ NA_real_
    ),
    stunt = if_else(HAZ2 < 6 & HAZ2 > -6, HAZ2, NA_real_)
  ) |>
  select(
    stunt,
    erp,
    place_of_res,
    wealth_index,
    melevel,
    chweight,
    HH7A_ch, # District
    HH1 # Sampling Design Cluster
  ) |>
  drop_na()

skimr::skim(haz_erp_df)

saveRDS(
  haz_erp_df,
  file = here::here("R/haz-erp-application/application_data.rds")
)
