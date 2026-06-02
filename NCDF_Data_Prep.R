# Load Packages
library(tidyverse);
library(dplyr);

# Working Directory
getwd();
setwd("C:/Users/User/Documents/NCDF");

# Load NCDF Dataset
NCDF <- read.csv("NEW_NGCDF_DATA_28-JAN_2026.csv", sep = ',', header = TRUE);

# Data Cleaning
# Remove trailing spaces in county names (apply only to the County column)
NCDF$County <- trimws(NCDF$County);

# Inspect unique county names after cleaning
unique(NCDF$County);

# Filter for 3 Counties of Interest

NCDF_County <- NCDF %>%
  filter(County %in% c("Kisii", "Kajiado", "Nyamira"), County != 0);

# Preview first few rows
head(NCDF_County);

# Inspect structure
str(NCDF_County);

# Check for missing values
colSums(is.na(NCDF_County));

# Basic Summaries
# Count records per county
table(NCDF_County$County);
# Pivot years into a single column

NCDF_long <- NCDF_County %>%
  pivot_longer(
    cols = starts_with("X"),       # all columns beginning with "X"
    names_to = "Year",             # new column for year
    values_to = "Allocation"       # new column for allocation values
  ) %>%
  mutate(Year = as.numeric(sub("X", "", Year))) ; # strip "X" and convert to numeric

# Convert Year to numeric
NCDF_long$Year <- as.numeric(NCDF_long$Year);
NCDF_long <- NCDF_long %>% filter(!is.na(Year));

# Summarize allocations by county and year
County_Summary <- NCDF_long %>%
  group_by(County, Year) %>%
  summarise(Total_Allocation = sum(Allocation, na.rm = TRUE), .groups = "drop");

#Constituency summary
Constituency_Summary <- NCDF_long %>%
  group_by(County, Constituency, Year) %>%
  summarise(Total_Allocation = sum(Allocation, na.rm = TRUE), .groups = "drop");

#To check performance
Top_Allocations <- Constituency_Summary %>%
  group_by(County, Year) %>%
  slice_max(Total_Allocation, n = 1, with_ties = FALSE);

# Preview summary
head(County_Summary);
head(Constituency_Summary);
head(Top_Allocations);

# Simple Visualization
# Plot allocation trends (2014–2026) for the three counties
ggplot(County_Summary, aes(x = Year, y = Total_Allocation, color = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "NCDF Allocations (2014–2026)",
       x = "Year",
       y = "Total Allocation (KES)") +
  theme_minimal();

# Visualization: Highlight Top Performers
ggplot(Constituency_Summary, aes(x = Year, y = Total_Allocation, color = Constituency)) +
  geom_line(alpha = 0.6) +
  geom_point(data = Top_Allocations, aes(x = Year, y = Total_Allocation),
             color = "black", size = 3, shape = 21, fill = "yellow") +
  facet_wrap(~County, scales = "free_y") +
  labs(title = "Top-Performing Constituencies Highlighted (2014–2026)",
       x = "Year",
       y = "Total Allocation (KES)") +
  theme_minimal();