# Load libraries
library(gbm)
library(caret)
library(here)

# Set RNG value for reproducibility
rngseed <- 42
set.seed(rngseed)

# Load the dataset
data <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv"))

# Select for only variables of interest
data <- data %>% 
  select(year, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
  mutate(across(c(year), as.factor)) 

# Split the data into training and testing sets
set.seed(rngseed)
train_index <- createDataPartition(data$year, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Set up cross-validation for training data
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Configure and train the GBM model with cross-validation on training data
model <- train(year ~ ., data = train_data, method = "gbm",
               trControl = train_control,
               verbose = FALSE)

# Print and plot model
print(model)
plot(model)


# Refining model based on results

# Set up a custom tuning grid
tuning_grid <- expand.grid(n.trees = c(50, 100, 150),
                           interaction.depth = c(2, 3),
                           shrinkage = 0.1,  # Constant value
                           n.minobsinnode = 10)  # Constant value

# Set up cross-validation
set.seed(rngseed)
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Configure and train the SGB model with cross-validation and tuning
model <- train(year ~ ., data = train_data, method = "gbm",
               trControl = train_control,
               tuneGrid = tuning_grid,
               verbose = FALSE)  

# Print and plot model
print(model)
plot(model)

# Evaluate the model
predictions <- predict(model, newdata = test_data)
confusionMatrix(predictions, test_data$year)


# Fitting final model

# Define the final parameters
final_n_trees <- 50
final_interaction_depth <- 3
final_shrinkage <- 0.1
final_n_minobsinnode <- 10

# Train the final SGB model with the selected parameters
final_model <- train(year ~ ., data = train_data, method = "gbm",
                     trControl = train_control,
                     tuneGrid = data.frame(n.trees = final_n_trees,
                                           interaction.depth = final_interaction_depth,
                                           shrinkage = final_shrinkage,
                                           n.minobsinnode = final_n_minobsinnode))

# Print and plot the final model
print(final_model)

# Generate predictions using the final SGB model
predictions_final <- predict(final_model, newdata = test_data)

# Calculate performance metrics
accuracy_final <- confusionMatrix(predictions_final, test_data$year)$overall["Accuracy"]
kappa_final <- confusionMatrix(predictions_final, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Final SGB Accuracy:", accuracy_final))
print(paste("Final SGB Kappa:", kappa_final))

# Generate predictions using the final SGB model
predictions_final <- predict(final_model, newdata = test_data, type = "raw")

# Convert predictions to numeric
predictions_numeric <- as.numeric(as.character(predictions_final))

# Convert test data's year column to numeric
test_year_numeric <- as.numeric(as.character(test_data$year))

# Calculate RMSE
rmse <- sqrt(mean((predictions_numeric - test_year_numeric)^2))

# Calculate R-squared
rsquared <- cor(predictions_numeric, test_year_numeric)^2

# Print RMSE and R-squared
print(paste("GBM RMSE:", rmse))
print(paste("GBM R-squared:", rsquared))

