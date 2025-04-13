install.packages("tidyverse")  # Includes tidyr
install.packages("tidycensus")
install.packages("tigris") #package for the function tracs to map dallas 

library(dplyr)
library(readr)
library(tidycensus)
library(ggplot2)
library(tidyr)
library(tigris)
# Set Census API Key
census_api_key("4a1ebc849fc0b8c6eb495412532ce5918841de8b", overwrite=TRUE, install = TRUE)

# Define Census variables
census_vars <- c(
  renters = "B25003_003", 
  homeowners = "B25003_002", 
  total_population = "B01003_001", 
  seniors_male = "B01001_020", 
  seniors_female = "B01001_021",
  seniors_other = "B01001_022",
  children_young = "B01001_003",
  children_middle = "B01001_004",
  children_oldest = "B01001_005",
  white = "B02001_002", 
  black = "B02001_003", 
  asian = "B02001_005", 
  hispanic = "B03003_003",
  rent_burden = "B25070_007"
)

# Fetch Census data for Dallas County
census_data <- get_acs(
  geography = "tract", 
  variables = census_vars, 
  state = "TX", 
  county = "Dallas", 
  year = 2021, 
  survey = "acs5"
)


census_data <- census_data %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    # Compute total seniors and children
    seniors = seniors_male + seniors_female + seniors_other,
    children = children_young + children_middle + children_oldest,

    # Compute within-group proportions
    senior_male_prop = seniors_male / seniors,
    senior_female_prop = seniors_female / seniors,
    senior_other_prop = seniors_other / seniors,

    child_young_prop = children_young / children,
    child_middle_prop = children_middle / children,
    child_oldest_prop = children_oldest / children,

    # Compute key housing & economic indicators
    homeownership_rate = homeowners / total_population,
    homeownership_risk_factor = 1 - (homeowners / total_population),
    rent_burden = rent_burden / 100,  # Convert to proportion

    # Compute racial majority classification
    racial_majority = case_when(
      white >= black & white >= asian & white >= hispanic ~ "White",
      black >= white & black >= asian & black >= hispanic ~ "Black",
      asian >= white & asian >= black & asian >= hispanic ~ "Asian",
      hispanic >= white & hispanic >= black & hispanic >= asian ~ "Hispanic",
      TRUE ~ "Other"
    )
  ) %>%
  select(GEOID, renters, homeowners, total_population, seniors, seniors_male, seniors_female, seniors_other,
         children, children_young, children_middle, children_oldest,
         senior_male_prop, senior_female_prop, senior_other_prop,
         child_young_prop, child_middle_prop, child_oldest_prop,
         homeownership_rate, homeownership_risk_factor, rent_burden, racial_majority)

# Load external dataset
displacement_data <- read_csv("dallas_monthly_2020_2021.csv")

# Filter for 2021 data only
displacement_data <- displacement_data %>%
  filter(grepl("2021", month)) %>%
  group_by(GEOID) %>%
  summarise(
    total_filings_2021 = sum(filings_2020, na.rm = TRUE),
    avg_filings_2021 = mean(filings_avg, na.rm = TRUE)
  )

# Merge datasets
merged_data <- census_data %>% 
  left_join(displacement_data, by = "GEOID")
# Compute eviction rate
merged_data <- merged_data %>%
  mutate(eviction_rate = total_filings_2021 / total_population)
# Compute displacement risk measure with additional homeownership risk factor
# Compute updated displacement risk with rent burden factor
merged_data <- merged_data %>%
  mutate(
    renter_rate = 1 - homeownership_rate,  # Get the renter percentage
    displacement_risk = eviction_rate + (rent_burden * renter_rate)
  )

print(merged_data)

# get Dallas County Census Tract geography data 
dallas_tracts <- tracts(state = "TX", county = "Dallas", year = 2021, class = "sf")
# cahnge merged_data GEOID to character to match 
merged_data$GEOID <- as.character(merged_data$GEOID)

# merge eviction rate 
dallas_map_data <- dallas_tracts %>%
  left_join(merged_data, by = c("GEOID" = "GEOID"))

# map
ggplot(data = dallas_map_data) +
  geom_sf(aes(fill = eviction_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "Eviction Rate") +
  labs(title = "Eviction Rate by Census Tract in Dallas County (2021)",
       subtitle = "Higher eviction rates concentrated in renter-heavy areas",
       caption = "Data Source: ACS 2021 & Eviction Filings") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Get Dallas County Census Tracts boundary data 
dallas_tracts <- tracts(state = "TX", county = "Dallas", year = 2021, class = "sf")

# # cahnge merged_data GEOID to character to match 
merged_data$GEOID <- as.character(merged_data$GEOID)

# Merge renter rate 
dallas_map_renter <- dallas_tracts %>%
  left_join(merged_data, by = "GEOID")

#map
ggplot(data = dallas_map_renter) +
  geom_sf(aes(fill = renter_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "Renter Rate") +
  labs(title = "Renter Rate by Census Tract in Dallas County (2021)",
       subtitle = "Higher renter concentration in certain neighborhoods",
       caption = "Data Source: ACS 2021 & Eviction Filings") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Create summary table comparing eviction rate, rent burden, and renter rate
risk_comparison_table <- merged_data %>%
  summarise(
    avg_eviction_rate = mean(eviction_rate, na.rm = TRUE),
    avg_rent_burden = mean(rent_burden, na.rm = TRUE),
    avg_renter_rate = mean(renter_rate, na.rm = TRUE)
  )

# Print the table
print(risk_comparison_table)


ggplot(merged_data, aes(x = renter_rate, y = eviction_rate)) +
  geom_jitter(alpha = 0.3, width = 0.01, height = 0.001) + # Adjust width and height as needed
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Comparison of renter_rate and Eviction Rate",
       x = "Renter Rate",
       y = "Eviction Rate") +
  theme_minimal()


library(ggplot2)

# Assuming 'rent_burden' and 'eviction_rate' exist in your merged_data

# Calculate correlation
correlation <- cor(merged_data$rent_burden, merged_data$eviction_rate, use = "complete.obs")

# Create the scatter plot
comparison_plot_rent_burden <- ggplot(merged_data, aes(x = rent_burden, y = eviction_rate)) +
  geom_point(alpha = 0.3, position = "jitter") +  # Transparency and jitter
  geom_smooth(method = "lm", color = "blue") +  # Regression line

  # Add correlation text
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Correlation:", round(correlation, 2))) +

  labs(title = "Relationship Between Rent Burden and Eviction Rate",
       x = "Rent Burden (Proportion)",  # Updated x-axis label
       y = "Eviction Rate (Units)") +  # Updated y-axis label
  theme_minimal()

print(comparison_plot_rent_burden)


#High Rent Burden & Homeownership Risk Suggest Vulnerability
#With 81.7% of the population being renters and 64.2% of renters cost-burdened, many households are economically vulnerable to displacement.
#Even though the formal eviction rate is low (0.97%), financial stress could be forcing renters to leave before eviction filings occur.
#Eviction Rate May Not Fully Capture Displacement
#The low eviction rate might not reflect the full scale of displacement, since many renters leave voluntarily due to rent increases or informal pressure before an eviction is formally recorded.
#Rent burden is a better indicator of financial stress that could lead to displacement.

# Define the threshold for high-risk tracts (top 25% displacement risk)
risk_threshold <- quantile(merged_data$displacement_risk, 0.75, na.rm = TRUE)

# Filter for high-risk areas
high_risk_data <- merged_data %>% 
  filter(displacement_risk >= risk_threshold)

# Compute proportions of renters and homeowners in high-risk areas
housing_risk_analysis <- high_risk_data %>%
  summarise(
    proportion_renters = sum(renters, na.rm = TRUE) / sum(total_population, na.rm = TRUE),
    proportion_homeowners = sum(homeowners, na.rm = TRUE) / sum(total_population, na.rm = TRUE)
  )

# Save and print the results
write_csv(housing_risk_analysis, "risk_analysis_renters_homeowners.csv")
print(housing_risk_analysis)

# Filter for high-risk areas
high_risk_data <- merged_data %>% 
  filter(displacement_risk >= risk_threshold)

# Compute proportions of renters and homeowners in high-risk areas
housing_risk_analysis <- high_risk_data %>%
  summarise(
    proportion_renters = sum(renters, na.rm = TRUE) / sum(total_population, na.rm = TRUE),
    proportion_homeowners = sum(homeowners, na.rm = TRUE) / sum(total_population, na.rm = TRUE)
  )

# Convert to long format for visualization
housing_risk_analysis_long <- housing_risk_analysis %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Proportion")

# ---- Bar Plot ----
housing_bar_plot <- ggplot(housing_risk_analysis_long, aes(x = Category, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Proportion of Renters and Homeowners in High-Risk Areas",
       x = "Housing Type",
       y = "Proportion of Population") +
  scale_fill_manual(values = c("proportion_renters" = "red", "proportion_homeowners" = "blue"),
                    labels = c("Renters", "Homeowners")) +
  theme_minimal()
print(housing_bar_plot)

#Renters are at Higher Displacement Risk:
#In the top 25% most at-risk areas, a larger share of the population are renters (27.4%) compared to homeowners (11.5%).
#This aligns with expectations, as renters typically face higher displacement risks due to eviction.
#Homeowners Are Less Affected:
#The homeownership rate in high-risk areas is lower, indicating that homeowners are less vulnerable to immediate displacement risks like eviction.
#However, some homeowners (11.5%) still face risks, which could be due to foreclosure, housing instability, or economic stress.

# Define the threshold for high-risk tracts (top 25% displacement risk)
risk_threshold <- quantile(merged_data$displacement_risk, 0.75, na.rm = TRUE)

# Filter for high-risk areas
high_risk_data <- merged_data %>% 
  filter(displacement_risk >= risk_threshold)

# Compute proportions of racial and ethnic groups in high-risk areas
racial_risk_analysis <- high_risk_data %>%
  group_by(racial_majority) %>%
  summarise(
    total_population_at_risk = sum(total_population, na.rm = TRUE),
    proportion_at_risk = total_population_at_risk / sum(high_risk_data$total_population, na.rm = TRUE)
  )
# Print racial risk analysis data
print(racial_risk_analysis)


# Bar Plot for Racial and Ethnic Groups
ggplot(racial_risk_analysis, aes(x = reorder(racial_majority, -proportion_at_risk), y = proportion_at_risk, fill = racial_majority)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Proportion of Racial and Ethnic Groups in High-Risk Areas",
       x = "Racial/Ethnic Group",
       y = "Proportion of Population at Risk") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

merged_data <- merged_data %>%
  mutate(
    # Calculate total seniors and children
    total_seniors = seniors_male + seniors_female + seniors_other,
    total_children = children_young + children_middle + children_oldest,

    # Compute within-group proportions for seniors
    senior_male_prop = seniors_male / total_seniors,
    senior_female_prop = seniors_female / total_seniors,
    senior_other_prop = seniors_other / total_seniors,

    # Compute within-group proportions for children
    child_young_prop = children_young / total_children,
    child_middle_prop = children_middle / total_children,
    child_oldest_prop = children_oldest / total_children
  )

print(merged_data)

senior_analysis <- merged_data %>%
  group_by(displacement_risk >= quantile(displacement_risk, 0.75, na.rm = TRUE)) %>%
  summarise(
    avg_senior_male = mean(senior_male_prop, na.rm = TRUE),
    avg_senior_female = mean(senior_female_prop, na.rm = TRUE),
    avg_senior_other = mean(senior_other_prop, na.rm = TRUE)
  )

print(senior_analysis)

# Create bar chart for within-group senior differences
senior_proportions <- merged_data %>%
  summarise(
    avg_senior_male = mean(senior_male_prop, na.rm = TRUE),
    avg_senior_female = mean(senior_female_prop, na.rm = TRUE),
    avg_senior_other = mean(senior_other_prop, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Proportion")

senior_bar_chart <- ggplot(senior_proportions, aes(x = Category, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), vjust = -0.5, size = 4) +
  labs(title = "Proportion of Seniors by Subgroup",
       x = "Senior Category",
       y = "Proportion of Total Seniors",
       fill = "Category") +
  theme_minimal()
print(senior_bar_chart)

child_analysis <- merged_data %>%
  group_by(displacement_risk >= quantile(displacement_risk, 0.75, na.rm = TRUE)) %>%
  summarise(
    avg_child_young = mean(child_young_prop, na.rm = TRUE),
    avg_child_middle = mean(child_middle_prop, na.rm = TRUE),
    avg_child_oldest = mean(child_oldest_prop, na.rm = TRUE)
  )

print(child_analysis)

# Create bar chart for within-group child differences
child_proportions <- merged_data %>%
  summarise(
    avg_child_young = mean(child_young_prop, na.rm = TRUE),
    avg_child_middle = mean(child_middle_prop, na.rm = TRUE),
    avg_child_oldest = mean(child_oldest_prop, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Proportion")

child_bar_chart <- ggplot(child_proportions, aes(x = Category, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), vjust = -0.5, size = 4) +
  labs(title = "Proportion of Children by Subgroup",
       x = "Child Age Group",
       y = "Proportion of Total Children",
       fill = "Category") +
  theme_minimal()

print(child_bar_chart)



# Define the threshold for high-risk tracts (top 25% displacement risk)
risk_threshold <- quantile(merged_data$displacement_risk, 0.75, na.rm = TRUE)

# Filter for high-risk areas
high_risk_data <- merged_data %>% 
  filter(displacement_risk >= risk_threshold)

# Compute proportions of renters and homeowners in high-risk areas
housing_risk_analysis <- high_risk_data %>%
  summarise(
    proportion_renters = sum(renters, na.rm = TRUE) / sum(total_population, na.rm = TRUE),
    proportion_homeowners = sum(homeowners, na.rm = TRUE) / sum(total_population, na.rm = TRUE)
  )

# Compute proportions of racial and ethnic groups in high-risk areas
racial_risk_analysis <- high_risk_data %>%
  group_by(racial_majority) %>%
  summarise(
    total_population_at_risk = sum(total_population, na.rm = TRUE),
    proportion_at_risk = total_population_at_risk / sum(high_risk_data$total_population, na.rm = TRUE)
  )

# Compute proportions of seniors in high-risk areas
seniors_risk_analysis <- high_risk_data %>%
  summarise(
    total_seniors_at_risk = sum(seniors, na.rm = TRUE),
    proportion_seniors_at_risk = total_seniors_at_risk / sum(high_risk_data$total_population, na.rm = TRUE)
  )

# Compute proportions of children in high-risk areas
children_risk_analysis <- high_risk_data %>%
  summarise(
    total_children_at_risk = sum(children, na.rm = TRUE),
    proportion_children_at_risk = total_children_at_risk / sum(high_risk_data$total_population, na.rm = TRUE)
  )

# Combine all results for visualization
summary_risk_analysis <- bind_rows(
  housing_risk_analysis %>% pivot_longer(cols = everything(), names_to = "Category", values_to = "Proportion"),
  racial_risk_analysis %>% select(racial_majority, proportion_at_risk) %>% rename(Category = racial_majority, Proportion = proportion_at_risk),
  seniors_risk_analysis %>% pivot_longer(cols = -total_seniors_at_risk, names_to = "Category", values_to = "Proportion") %>% select(-total_seniors_at_risk),
  children_risk_analysis %>% pivot_longer(cols = -total_children_at_risk, names_to = "Category", values_to = "Proportion") %>% select(-total_children_at_risk)
)



# Rename categories for clarity:
summary_risk_analysis <- summary_risk_analysis %>%
  mutate(Category = recode(Category,
                           "proportion_renters" = "Renters",
                           "proportion_homeowners" = "Homeowners",
                           "proportion_children_at_risk" = "Children at Risk",
                           "proportion_seniors_at_risk" = "Seniors at Risk",
                           "racial_majority" = "Race",
                           "Black" = "Black",
                           "White" = "White",
                           "Hispanic" = "Hispanic",
                           "Asian" = "Asian"
                           ))

#horizontal bars
ggplot(summary_risk_analysis, aes(x = reorder(Category, Proportion), y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
            hjust = 1.2, size = 3, color = "white") + 

  scale_y_continuous(labels = scales::percent, limits = c(0, max(summary_risk_analysis$Proportion) * 1.1)) +
  labs(title = "Proportion of At-Risk Groups in High-Displacement Areas",
       subtitle = "Top 25% displacement risk areas in Dallas County",
       x = NULL, 
       y = "Proportion of Population at Risk") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "none") +
  scale_fill_viridis_d(option = "turbo") 
print(summary_risk_analysis %>% arrange(desc(Proportion)))






