## loading the necessary packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(fpemlocal)
library(countrycode)

## Identifying the 85 focus countries of the FP2030 initiative

# The `countrycode` package contains the division_numeric_code of all countries across the world.
## Hence, we will use the `division_numeric_code` to extract the income groupings of these countries from the WDI.
## Since the focus countries are mainly the low and lower middle income countries.

# the division numeric code
wdi_countries <- WDI::WDI_data$country |>
  filter(region != "Aggregates")  |> # ignoring the aggregates
  mutate(division_numeric_code = countrycode(iso3c, "iso3c", "un")) |>
  select(division_numeric_code, income)


## Other countries who joined the survey that are not part of the low and lower middle income countries of the WDI are:
## Algeria, Belize, Botswana, El Salvador, Indonesia, Iran, Mongolia, Namibia, Ukraine.

other_fcountries <- c("Algeria", "Belize", "Botswana", "El Salvador", "Indonesia",
                      "Iran (Islamic Republic of)", "Mongolia", "Namibia", "Ukraine")


# Let's join these to extract the grouping information of these countries and other low and lower middle income countries
focus_countries <- divisions |>
  select(division_numeric_code, country = name_country, region = name_region, sub_region = name_sub_region) |>
  left_join(wdi_countries, by = "division_numeric_code") |>
  filter(income %in% c("Low income", "Lower middle income")|
           country %in% other_fcountries) |>
  ungroup() |>
  mutate(country = country |>
           str_replace("\\s*\\(.*\\)", "") |>
           str_replace("^Democratic People's Republic of Korea*", "DPR Korea") |>
           str_replace("^Democratic Republic of the Congo*", "DR Congo") |>
           str_replace("^Lao People's Democratic Republic*", "Lao PDR") |>
           str_replace("^United Republic of\\s+", "") |>
           str_replace("'", "’")
  ) |>
  filter(country != "Micronesia") # Micronesia is regarded as a sub_region, but WDI has it as a country.

## `focus_countries` is a data frame containing the 85 FP2030 focus countries with their respective grouping information.


### SURVEY DATA of the United Nations Population Division Data.

## Filtering survey contraceptive use data for married women category alone of the UNPD data.

contraceptive_use_data <- contraceptive_use |>
  mutate(year = floor(start_date)) |>
  filter(is_in_union == "Y", year >= 1990) |>
  select(division_numeric_code,
         year,
         data_series_type,
         contraceptive_use_modern, # priority
         contraceptive_use_traditional,
         se_modern,
         se_traditional)

## `contraceptive_use_data` above contains modern and traditional contraceptive use data across all the survey types.

## `contraceptive_use` data frame have 197 country levels, filtering year >= 1990, reduced the available data to across 185 countries

## ranking the survey types to prioritise DHS above others for country-year pair with more than one survey type

survey_contraceptive_use_data <- contraceptive_use_data |>
  dplyr::mutate(priority = dplyr::case_when(
    data_series_type == "DHS" ~ 1,
    data_series_type == "MICS" ~ 2,
    data_series_type == "PMA" ~ 3,
    data_series_type == "National survey" ~ 4,
    data_series_type == "Other" ~ 5
  )) |>
  dplyr::group_by(division_numeric_code, year) |>
  dplyr::slice_min(order_by = priority) |>
  ungroup() |>
  select(-priority) |>
  dplyr::group_by(division_numeric_code, year, data_series_type) |>  # if the country-year have multiple values from the same survey take the average.
  summarise_if(is.numeric, mean, na.rm = TRUE) |>
  left_join(divisions |>
              select(division_numeric_code, country = name_country,
                     sub_region = name_sub_region,region = name_region),
            join_by("division_numeric_code")) |>
  mutate(country = country |>
           str_replace("\\s*\\(.*\\)", "") |>
           str_replace("^Democratic People's Republic of Korea*", "DPR Korea") |>
           str_replace("^Democratic Republic of the Congo*", "DR Congo") |>
           str_replace("^Lao People's Democratic Republic*", "Lao PDR") |>
           str_replace("^United Republic of\\s+", "") |>
           str_replace("'", "’")
  ) |>
  ungroup()

# survey_contraceptive_use_data contains one country-year pair for all the family planning data.


## The interest for this analysis is the family planning focus countries as specified by the track20.

## The survey data for the 85 focus countries only

survey_fc_contraceptive_use_data <- survey_contraceptive_use_data |>
  filter(division_numeric_code %in% focus_countries$division_numeric_code)

## Hence,`survey_fc_contraceptive_use_data` above contains modern and traditional contraceptive use data
## with one data point per country-year combination and priority on DHS over other survey types.


### MODEL DATA of the United Nations Population Division Family Planning Estimation Model Outputs.

FPEM_model_output <- readr::read_csv("Data/Data_FamilyPlanningIndicators_2024.csv")

model_data <- FPEM_model_output |> filter(
  Time >= 1990 & Time <= 2025, # year range
  Variant == "Median", # Model-based Median estimates
  Category == "Married or in a union women",
  IndicatorShortName %in% c("CPModP", "CPTrad") # modern and traditional
) |>
  mutate(
    proportion = Value / 100,
    indicator = dplyr::recode( # renaming the indicator codes
      IndicatorShortName,
      "CPModP" = "contraceptive_use_modern",
      "CPTrad" = "contraceptive_use_traditional"
    )
  ) |>
  select(LocationId, year = Time, indicator, EstimateMethod, proportion)


## joining the country grouping information to the `model_contraceptive_use_data`.

model_contraceptive_use_data <- model_data |>
  left_join(divisions |>
              select(division_numeric_code, country = name_country,
                     sub_region = name_sub_region,region = name_region),
            join_by("LocationId" == "division_numeric_code")) |>
  mutate(country = country |>
           str_replace("\\s*\\(.*\\)", "") |>
           str_replace("^Democratic People's Republic of Korea*", "DPR Korea") |>
           str_replace("^Democratic Republic of the Congo*", "DR Congo") |>
           str_replace("^Lao People's Democratic Republic*", "Lao PDR") |>
           str_replace("^United Republic of\\s+", "") |>
           str_replace("'", "’")
  ) |>
  ungroup() |>
  pivot_wider(id_cols = c(LocationId, country, region, sub_region, year),
              values_from = proportion,
              names_from = indicator) |>
  filter(!is.na(country))


## The model-based estimates for the 85 focus countries only

model_fc_contraceptive_use_data <- model_contraceptive_use_data |>
  filter(LocationId %in% focus_countries$division_numeric_code)


#### For reference
#`survey_contraceptive_use_data` is the survey data of the proportion of modern
# and traditional contraceptive uses.

#`model_contraceptive_use_data` is the model data of the proportion of modern
# and traditional contraceptive uses.

#`survey_fc_contraceptive_use_data` is the survey data of the proportion of modern
# and traditional contraceptive uses for the FP2030 focus countries.

#`model_fc_contraceptive_use_data` is the model data of the proportion of modern
# and traditional contraceptive uses for the FP2030 focus countries.

# `focus_countries` is the list of the 85 FP2030 initiative focus countries.


