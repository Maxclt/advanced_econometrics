library(dplyr)
library(tidyr)
library(haven)
library(readr)

# Load initial dataset
countylist <- read_dta("minwage_dubeRESTAT/countylist.dta")
qcew <- read_dta("minwage_dubeRESTAT/QCEW_industrydata.dta")
mw_data <- read_dta("minwage_dubeRESTAT/MW_yr_qtr_84_07.dta")
county_pairs <- read_csv("minwage_dubeRESTAT/county-pair-list.txt") %>%
  rename(countyreal = COUNTY, pair_id = COUNTYPAIR_ID)

# Expand to 96 time periods per county
main_panel <- countylist %>%
  slice(rep(1:n(), each = 96)) %>%
  group_by(countyreal) %>%
  mutate(period = row_number(),
         year = 1984 + floor((period - 1) / 4),
         quarter = ((period - 1) %% 4) + 1) %>%
  ungroup()

# Merge with QCEW data
using_only_vars <- setdiff(names(qcew), names(main_panel))

if (length(using_only_vars) == 0) {
  stop("No unique variables found in using dataset to filter on.")
} else {
  filter_var <- using_only_vars[1]
}

main_panel <- main_panel %>%
  left_join(qcew, by = c("countyreal", "year", "quarter")) %>%
  filter(!is.na(.data[[filter_var]]))

# Handle missing state_fips
main_panel <- main_panel %>%
  group_by(countyreal) %>%
  mutate(state_fips = max(state_fips, na.rm = TRUE)) %>%
  ungroup()

# Merge with minimum wage data
main_panel <- main_panel %>%
  left_join(mw_data, by = c("state_fips", "year", "quarter"))

# Fix SF minimum wage
main_panel <- main_panel %>%
  mutate(CA = state_fips == 6) %>%
  group_by(year, quarter) %>%
  mutate(CAminwage = max(minwage[CA], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(minwage = ifelse(countyreal == 6075, CAminwage, minwage),
         minwage = case_when(
           countyreal == 6075 & year == 2004 ~ 8.50,
           countyreal == 6075 & year == 2005 ~ 8.62,
           countyreal == 6075 & year == 2006 ~ 8.82,
           countyreal == 6075 & year == 2007 ~ 9.15,
           TRUE ~ minwage
         ),
         lnpop = log(pop),
         lnMW = log(minwage)) %>%
  rename(stminwage = st_mw,
         federalmin = fed_mw)

# Save
write_dta(main_panel, "data/minwage/QCEWindustry_minwage_all.dta")

# Expand and time variables
county_pairs_expanded <- county_pairs %>%
  slice(rep(1:n(), each = 96)) %>%
  group_by(pair_id, countyreal) %>%
  mutate(period = row_number(),
         year = 1984 + floor((period - 1) / 4),
         quarter = ((period - 1) %% 4) + 1,
         countyreal = as.numeric(countyreal)) %>%
  ungroup()

# Merge QCEW
using_only_vars <- setdiff(names(qcew), names(county_pairs_expanded))

if (length(using_only_vars) == 0) {
  stop("No unique variables found in using dataset to filter on.")
} else {
  filter_var <- using_only_vars[1]
}

county_pairs_expanded <- county_pairs_expanded %>%
  left_join(qcew, by = c("countyreal", "year", "quarter")) %>%
  filter(!is.na(.data[[filter_var]]))

# Fix state_fips
county_pairs_expanded <- county_pairs_expanded %>%
  group_by(countyreal) %>%
  mutate(state_fips = max(state_fips, na.rm = TRUE)) %>%
  ungroup()

# Manual fixes
county_pairs_expanded <- county_pairs_expanded %>%
  mutate(state_fips = case_when(
    countyreal == 6075 ~ 99,
    countyreal == 30113 ~ 30,
    TRUE ~ state_fips
  ))

# Merge minimum wage
county_pairs_expanded <- county_pairs_expanded %>%
  left_join(mw_data, by = c("state_fips", "year", "quarter"))

# SF fix
county_pairs_expanded <- county_pairs_expanded %>%
  mutate(CA = state_fips == 6) %>%
  group_by(year, quarter) %>%
  mutate(CAminwage = max(minwage[CA], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(minwage = ifelse(state_fips == 99, CAminwage, minwage),
         minwage = case_when(
           state_fips == 99 & year == 2004 ~ 8.50,
           state_fips == 99 & year == 2005 ~ 8.62,
           state_fips == 99 & year == 2006 ~ 8.82,
           state_fips == 99 & year == 2007 ~ 9.15,
           TRUE ~ minwage
         ),
         lnMW = log(minwage),
         lnpop = log(pop),
         all = 1,
         event = event_type < 3) %>%
  rename(stminwage = st_mw,
         federalmin = fed_mw)

# Pair-period groups and wage differences
county_pairs_expanded <- county_pairs_expanded %>%
  mutate(pair_id_period = interaction(pair_id, period, drop = TRUE)) %>%
  group_by(pair_id) %>%
  mutate(nonmissing_both_pair = min(nonmissing_rest_both, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(pair_id_period) %>%
  mutate(lnMW_min_pairperiod = min(lnMW, na.rm = TRUE),
         lnMW_max_pairperiod = max(lnMW, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lnMW_dif_period = (lnMW_min_pairperiod != lnMW_max_pairperiod) & (period >= 25 & period <= 90)) %>%
  group_by(pair_id) %>%
  mutate(lnMW_dif = max(lnMW_dif_period, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lnMW_gap_pair = lnMW_max_pairperiod - lnMW_min_pairperiod)

# Rename non valid column name

names(county_pairs_expanded) <- gsub(" ", "_", names(county_pairs_expanded))

# Save
write_dta(county_pairs_expanded, "data/minwage/QCEWindustry_minwage_contig.dta")


