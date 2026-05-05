# Primary Author: Ethan Tluszcz
# tidy and wrangled data

# Step 1: Read the raw CSV
ageRaw <- read.csv("age_income_raw.csv", stringsAsFactors = FALSE)

# Step 2: Drop the "Under 65 years" Row so we don't double count
ageRaw <- ageRaw[ageRaw$characteristic != "Under 65 years", ]

# Step 3: Capture the Census significance flag (asterisk) into a logical column
ageRaw$Significant <- grepl("\\*", ageRaw$pct_change_Income)

# Step 4: Strip asterisks from pct_change_Income
ageRaw$pct_change_Income <- gsub("\\*", "", ageRaw$pct_change_Income)
ageRaw$pct_change_Income <- as.numeric(ageRaw$pct_change_Income)

# Step 5: Order the age groups along the life cycle for plotting
ageRaw$characteristic <- factor(
  ageRaw$characteristic,
  levels = c(
    "15 to 24 years",
    "25 to 34 years",
    "35 to 44 years",
    "45 to 54 years",
    "55 to 64 years",
    "65 years and older"
  )
)

# Step 6: Rename to friendlier names matching team conventions
names(ageRaw) <- c(
  "age_group", "households_2023", "median_2023", "moe_2023",
  "households_2024", "median_2024", "moe_2024",
  "pct_change", "pct_change_moe", "significant_change"
)

ageIncome <- ageRaw
rownames(ageIncome)


