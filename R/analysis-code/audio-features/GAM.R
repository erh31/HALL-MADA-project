# Load libraries
library(mgcv)
library(here)
library(caret)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)

# Load the dataset
df <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv"))

# Set RNG value for reproducibility
rngseed <- 42
set.seed(rngseed)

# Assuming 'df' is your original dataframe and 'main_genres' is the genre variable
df <- df %>%
  # Split the genres into a list
  mutate(genre_list = strsplit(as.character(main_genres), ",\\s*")) %>%
  # Create a binary indicator for each genre
  mutate(pop = as.integer(lapply(genre_list, function(g) 'pop' %in% g)),
         rock = as.integer(lapply(genre_list, function(g) 'rock' %in% g)),
         country = as.integer(lapply(genre_list, function(g) 'country' %in% g)),
         jazz_blues = as.integer(lapply(genre_list, function(g) 'jazz_blues' %in% g)),
         soul_funk = as.integer(lapply(genre_list, function(g) 'soul_funk' %in% g)),
         hip_hop_rap = as.integer(lapply(genre_list, function(g) 'hip_hop_rap' %in% g)),
         electronic_dance = as.integer(lapply(genre_list, function(g) 'electronic_dance' %in% g)),
         rnb_reggae = as.integer(lapply(genre_list, function(g) 'r&b_reggae' %in% g)),
         classic_folk = as.integer(lapply(genre_list, function(g) 'classic_folk' %in% g)),
         world_regional = as.integer(lapply(genre_list, function(g) 'world_regional' %in% g)),
         indie_alt = as.integer(lapply(genre_list, function(g) 'indie_alt' %in% g))) %>%
  # Remove the original 'main_genres' and 'genre_list' columns to avoid duplication
  select(-main_genres, -genre_list)

# Define the audio features you're interested in
audio_features <- c("danceability", "energy", "loudness", "speechiness",
                    "acousticness", "instrumentalness", "liveness", "valence", "tempo")

# Define different degrees of freedom to test for the spline on 'year'
degrees_of_freedom <- c(4, 6, 8, 10)

# Initialize a list to store the best GAM model for each feature
best_gam_models <- list()
model_performance <- list()

# Perform cross-validation for each feature
for (feature in audio_features) {
  # Create 5-fold cross-validation indices
  folds <- createFolds(df$year, k = 5, returnTrain = TRUE)
  set.seed(rngseed)  # for reproducibility
  feature_rmses <- list()
  
  # Test each degrees of freedom for the spline
  for (deg_free in degrees_of_freedom) {
    rmses <- vector("numeric", length(folds))
    
    for (i in seq_along(folds)) {
      # Ensure correct subsetting for training and testing
      train_indices <- folds[[i]]
      test_indices <- setdiff(1:nrow(df), train_indices)
      
      train_data <- df[train_indices, , drop = FALSE]
      test_data <- df[test_indices, , drop = FALSE]
      
      # Fit the GAM model on the training data with specified degrees of freedom
      formula <- as.formula(paste(feature, "~ s(year, k =", deg_free, ") + pop + rock + country + jazz_blues + soul_funk +
                               hip_hop_rap + electronic_dance + rnb_reggae + classic_folk + 
                               world_regional + indie_alt"))
      gam_fit <- gam(formula, data = train_data, method = "REML")
      
      # Predict on the testing data
      predictions <- predict(gam_fit, newdata = test_data)
      
      # Calculate and store RMSE for this fold
      rmses[i] <- sqrt(mean((predictions - test_data[[feature]])^2))
    }
    
    # Store average RMSE for this degrees of freedom
    feature_rmses[[as.character(deg_free)]] <- mean(rmses)
  }
  
  # Identify the best degrees of freedom
  best_deg_free <- as.numeric(names(which.min(unlist(feature_rmses))))
  
  # Fit the best model to the entire dataset
  best_formula <- as.formula(paste(feature, "~ s(year, k =", best_deg_free, ") + pop + rock + country + jazz_blues + soul_funk +
                                    hip_hop_rap + electronic_dance + rnb_reggae + classic_folk + 
                                    world_regional + indie_alt"))
  best_gam_models[[feature]] <- gam(best_formula, data = df, method = "REML")
  
  # Store the best model's RMSE
  model_performance[[feature]] <- min(unlist(feature_rmses))
}

# Print the model performance
print(model_performance)

# Generate a rainbow color palette for each audio feature
feature_colors <- setNames(rainbow(length(audio_features)), audio_features)

# Create plots for each audio feature, with customized CI colors
plot_list <- lapply(names(best_gam_models), function(feature) {
  plot_data <- data.frame(year = df$year, feature_value = df[[feature]])
  min_year <- min(plot_data$year)
  max_year <- max(plot_data$year)
  
  # Get the corresponding color for the feature's confidence interval
  fill_color <- feature_colors[feature]
  
  ggplot(plot_data, aes(x = year, y = feature_value)) +
    geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "black", fill = fill_color) +
    ggtitle(paste(tools::toTitleCase(feature))) +
    ylab("Value") +
    xlab("Year") +
    scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 10)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate the x-axis text
})

# Combine valid plots using patchwork and add an overall title
combined_plots <- patchwork::wrap_plots(plot_list, ncol = 3) +
  plot_annotation(title = "GAMs for Audio Features Over Time")

# Print combined plots
print(combined_plots)

