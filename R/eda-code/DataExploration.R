# EXPLORING THE DATA A LITTLE BIT

# loading necessary libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(here)

# Load data
data <- read.csv(here("data", "processed-data", "Hot_100s_Processed.csv"))

# Summary statistics
summary_stats <- summary(data)
print(data)

# Bar graph showing the total instances of each main genre
genre_counts <- table(unlist(strsplit(data$main_genres, ", ")))
genre_counts <- sort(genre_counts, decreasing = TRUE) # Sort by count in descending order

# Convert to data frame for plotting
genre_counts_df <- data.frame(genre = names(genre_counts),
                              count = as.numeric(genre_counts))

# Plot bar graph
ggplot(genre_counts_df, aes(x = reorder(genre, -count), y = count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Total Instances of Each Main Genre",
       x = "Main Genre",
       y = "Total Instances") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Prepare the data: Calculate the percentage of instances per year for each main genre
hot100_percentage <- data %>%
  separate_rows(main_genres, sep = ",") %>%
  group_by(year, main_genres) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(total = sum(n), percentage = n / total * 100) %>%
  select(-n, -total) %>%
  spread(key = main_genres, value = percentage, fill = 0) # Convert genres to wide format for area plot

# Summary statistics
summary_stats <- summary(hot100_percentage)
print(hot100_percentage)

# Assuming hot100_percentage is already in the correct format with columns for each genre and year
ggplot(hot100_percentage, aes(x = year)) +
  geom_area(aes(y = classic_folk, fill = 'Classic/Folk')) +
  geom_area(aes(y = country, fill = 'Country')) +
  geom_area(aes(y = electronic_dance, fill = 'Electronic/Dance')) +
  geom_area(aes(y = hip_hop_rap, fill = 'Hip-Hop/Rap')) +
  geom_area(aes(y = indie_alt, fill = 'Indie/Alt')) +
  geom_area(aes(y = jazz_blues, fill = 'Jazz/Blues')) +
  geom_area(aes(y = other, fill = 'Other Genres')) +
  geom_area(aes(y = pop, fill = 'Pop')) +
  geom_area(aes(y = `r&b_reggae`, fill = 'R&B/Reggae')) +
  geom_area(aes(y = rock, fill = 'Rock')) +
  geom_area(aes(y = soul_funk, fill = 'Soul/Funk')) +
  geom_area(aes(y = world_regional, fill = 'World/Regional')) +
  theme_minimal() +
  labs(title = "Percentage of Main Genres Over Time",
       x = "Year",
       y = "Percentage",
       fill = "Genre")






