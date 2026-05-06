
library(tidyverse)
library(scales)

edu_summary <- data.frame(
  education = c("No high school diploma", "High school graduate", "Some college", "Bachelor's degree or higher"),
  income_2023 = c(37570, 57250, 75510, 130100),
  income_2024 = c(36900, 58410, 76520, 132700),
  percent_change = c(-1.8, 2.0, 1.3, 2.0)
)

#  (Earnings Premium)
# High school graduate as Baseline 
hs_2023 <- edu_summary$income_2023[2]
hs_2024 <- edu_summary$income_2024[2]

premium_analysis <- edu_summary %>%
  mutate(
    premium_2023 = (income_2023 / hs_2023) - 1,
    premium_2024 = (income_2024 / hs_2024) - 1,
    premium_delta = premium_2024 - premium_2023
  )


cat("--- premium associated with higher education diminishing ---\n")
print(premium_analysis %>% select(education, premium_2023, premium_2024, premium_delta))

# 4. 
plot_data <- premium_analysis %>%
  filter(education != "High school graduate") %>%
  pivot_longer(cols = c(premium_2023, premium_2024), names_to = "Year", values_to = "Premium") %>%
  mutate(Year = ifelse(Year == "premium_2023", "2023", "2024"))

ggplot(plot_data, aes(x = reorder(education, Premium), y = Premium, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Higher Education Earnings Premium (2023 vs 2024)",
    subtitle = "Relative to High School Graduates",
    x = "Education Level",
    y = "Premium Percentage (%)",
    caption = "Source: U.S. Census Bureau, CPS ASEC [cite: 11]"
  ) +
  theme_minimal()

#case2: gender

library(tidyverse)
library(tidyverse)
library(scales)
gender_df <- data.frame(
  group = c("Male Householder", "Female Householder"),
  income_2023 = c(58680, 43230), 
  income_2024 = c(58000, 44870), 
  moe_2024 = c(1866, 1256)      
)
gender_analysis <- gender_df %>%
  summarise(
    ratio_2023 = gender_df$income_2023[2] / gender_df$income_2023[1],
    ratio_2024 = gender_df$income_2024[2] / gender_df$income_2024[1]
  ) %>%
  mutate(
    improvement = ratio_2024 - ratio_2023,
    status = ifelse(improvement > 0, "Gap Narrowing", "Gap Widening")
  )


cat("--- 性别收入差距分析 (2023 vs 2024) ---\n")
print(gender_analysis)

plot_gender <- gender_df %>%
  pivot_longer(cols = starts_with("income"), names_to = "Year", values_to = "Income") %>%
  mutate(Year = ifelse(Year == "income_2023", "2023", "2024"))

ggplot(plot_gender, aes(x = group, y = Income, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = dollar(Income)), position = position_dodge(0.9), vjust = -0.5) +
  labs(
    title = "Median Income by Gender (Nonfamily Households)",
    subtitle = "Comparing Real Median Income Growth 2023-2024",
    x = "Gender Role",
    y = "Income (2024 Dollars)"
  ) +
  theme_minimal()