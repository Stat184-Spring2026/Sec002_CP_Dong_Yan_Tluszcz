#Primary Author: Ethan Tluszcz
#Read and clean Census age income data 

library(dplyr)
library(tidyr)
library(readr)

# Step 1: Read the raw CSV 
ageRaw <- read.csv("age_income_raw.csv")

# Step 2: Wrangle in a piped sequence
ageIncome <- ageRaw |>
  # Step 2a: update column names
  rename(         
    age_group = characteristic,
    households_2023 = Population_2023,
    median_2023 = Income_2023,
    moe_2023 = MOE_2023,
    households_2024 = Population_2024,
    median_2024 = Income_2024,
    moe_2024 = MOE_2024,
    pct_change = pct_change_Income,
    pct_change_moe = pct_change_MOE
  ) |>
  # Step 2b: remove under 65 total row
  filter(age_group != "Under 65 years") |>  
  # Step 2c: flag significance and parse pct_change
  mutate(                                    
    significant_change = if_else(
      condition = (age_group %in% c(
        "15 to 24 years",
        "35 to 44 years",
        "45 to 54 years"
      )),
      true = TRUE,
      false = FALSE
    ),
    pct_change = parse_number(pct_change)
  )
ageIncome <- ageRaw
rownames(ageIncome)


