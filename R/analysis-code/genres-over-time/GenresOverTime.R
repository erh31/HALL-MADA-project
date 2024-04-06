# Load required libraries
library(tidyverse)
library(readr)
library(patchwork)
library(stringr)

# VISUAL ANALYSIS

# Load the dataset
data <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv"))


# Binary flags for main genres (to make analysis easier)
binary_genres <- data %>%
  mutate(
    Genre_Pop = ifelse(str_detect(main_genres, "pop"), 1, 0),
    Genre_Rock = ifelse(str_detect(main_genres, "rock"), 1, 0),
    Genre_Country = ifelse(str_detect(main_genres, "country"), 1, 0),
    Genre_JazzBlues = ifelse(str_detect(main_genres, "jazz_blues"), 1, 0),
    Genre_SoulFunk = ifelse(str_detect(main_genres, "soul_funk"), 1, 0),
    Genre_HipHopRap = ifelse(str_detect(main_genres, "hip_hop_rap"), 1, 0),
    Genre_ElectronicDance = ifelse(str_detect(main_genres, "electronic_dance"), 1, 0),
    Genre_RnBReggae = ifelse(str_detect(main_genres, "r&b_reggae"), 1, 0),
    Genre_ClassicFolk = ifelse(str_detect(main_genres, "classic_folk"), 1, 0),
    Genre_WorldRegional = ifelse(str_detect(main_genres, "world_regional"), 1, 0),
    Genre_IndieAlt = ifelse(str_detect(main_genres, "indie_alt"), 1, 0)
  )

write.csv(binary_genres, here("data", "processed-data", "Intermediate", "Hot100s_BinaryGenres.csv"), row.names = FALSE)


# Assuming 'data' is your dataset with binary flags for genres and 'year' is the year column

# Calculate the total number of songs each year
total_songs_by_year <- data %>%
  group_by(year) %>%
  summarise(total_songs = n())

# Calculate the count of songs for each genre by year
genre_counts_by_year <- binary_genres %>%
  group_by(year) %>%
  summarise(
    Pop = sum(Genre_Pop),
    Rock = sum(Genre_Rock),
    Country = sum(Genre_Country),
    Jazz_Blues = sum(Genre_JazzBlues),
    Soul_Funk = sum(Genre_SoulFunk),
    HipHop_Rap = sum(Genre_HipHopRap),
    Electronic_Dance = sum(Genre_ElectronicDance),
    `R&B_Reggae` = sum(Genre_RnBReggae),
    Classic_Folk = sum(Genre_ClassicFolk),
    World_Regional = sum(Genre_WorldRegional),
    Indie_Alt = sum(Genre_IndieAlt)
  ) %>%
  pivot_longer(-year, names_to = "genre", values_to = "count")

# Assuming 'genre_counts_by_year' already contains the count of songs for each genre by year
# You don't need to join with the total_songs_by_year or calculate percentages

# Check for genres with data
genres_with_data <- unique(genre_counts_by_year$genre)

# List of your genres
genres <- c("Pop", "Rock", "Country", "Jazz_Blues", "Soul_Funk", "HipHop_Rap", 
            "Electronic_Dance", "R&B_Reggae", "Classic_Folk", "World_Regional", "Indie_Alt")

# Filter the list of genres to only those with data
genres <- intersect(genres, genres_with_data)

# Modified function to create a plot for a single genre with total count
create_genre_plot <- function(genre_data, genre_name) {
  if (nrow(genre_data) == 0) {
    stop(paste("No data available for genre:", genre_name))
  }
  
  # Define a color for the fill based on the genre name
  genre_colors <- setNames(rainbow(length(genres)), genres)
  fill_color <- genre_colors[genre_name]
  
  # Plot with total counts
  ggplot(genre_data, aes(x = year, y = count)) +
    geom_area(fill = fill_color, alpha = 0.7) +  # More opaque area with a consistent color
    geom_smooth(method = "loess", color = "black", se = FALSE) +  # Add a smoothing line
    labs(title = genre_name, x = NULL, y = "Total Count") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
    )
}

# Create a list of plots with total counts
genre_plots <- lapply(genres, function(genre_name) {
  genre_data <- genre_counts_by_year %>% filter(genre == genre_name)
  create_genre_plot(genre_data, genre_name)
})

# Arrange the plots into a grid
genre_plot_grid <- do.call(patchwork::wrap_plots, c(genre_plots, ncol = 3))

# Print the grid of plots
genre_plot_grid



# VARIABLE ANALYSIS 

# Load necessary libraries
library(tidymodels)
library(readr)

# Splitting the data into training and testing sets
set.seed(123) # For reproducibility
data_split <- initial_split(binary_genres, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define the model specification using a linear regression model for simplicity
model_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# Define the recipe for preprocessing, targeting 'time_on_chart' as the outcome
recipe_spec <- recipe(time_on_chart ~ danceability + energy + key + loudness + mode +
                        speechiness + acousticness + instrumentalness + liveness +
                        valence + tempo, data = train_data) %>%
  step_normalize(all_predictors())

# Fit the workflow
workflow <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec) %>%
  fit(data = train_data)

# Summarize the workflow and model coefficients
summary(workflow)

# Predict on the test set
predictions <- predict(workflow, new_data = test_data) %>%
  bind_cols(test_data)

# Evaluate the model
metrics <- yardstick::metrics(predictions, truth = time_on_chart, estimate = .pred)
metrics





