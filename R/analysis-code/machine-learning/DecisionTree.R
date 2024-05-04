# Load libraries
library(rpart)
library(caret)
library(here)

# Set RNG value for reproducibility
rngseed <- 42
set.seed(rngseed)

# Load the dataset
data <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv"))

# Select for only variables of interest
data <- data %>% select(year, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
  mutate(across(c(year), as.factor)) 

# Split the data into training and testing sets
train_indices <- createDataPartition(data$year, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Set up cross-validation
set.seed(rngseed)  # for reproducibility
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Configure and train the Decision Tree model with cross-validation
model <- train(year ~ ., data = train_data, method = "rpart",
               trControl = train_control)

# Print and plot model
print(model)
plot(model)

# Evaluate the model on test data
predictions <- predict(model, newdata = test_data)
confusionMatrix(predictions, test_data$year)


# Refining model based on results

# Define a wider tuning grid
tuning_grid <- expand.grid(cp = c(0.005, 0.01, 0.015, 0.02))

# Configure and train the Decision Tree model with cross-validation and tuning
model <- train(year ~ ., data = data, method = "rpart",
               trControl = train_control,
               tuneGrid = tuning_grid)

# Print and plot model
print(model)
plot(model)

# Evaluate the model on test data
predictions <- predict(model, newdata = test_data)
confusionMatrix(predictions, test_data$year)


# Fitting final model

# Set the chosen complexity parameter
final_cp <- 0.005

# Train the final CART model with the selected cp value using the entire dataset
final_model <- rpart(year ~ ., data = data, method = "class", control = rpart.control(cp = final_cp))

# Print and plot the final model
print(final_model)
plot(final_model)

# Evaluate the final model (optional)
predictions <- predict(final_model, newdata = data, type = "class")
confusionMatrix(predictions, data$year)

# Generate predictions using the final Decision Tree model
predictions_dt <- predict(model, newdata = test_data)

# Calculate performance metrics
dt_accuracy <- confusionMatrix(predictions_dt, test_data$year)$overall["Accuracy"]
dt_kappa <- confusionMatrix(predictions_dt, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Decision Tree Accuracy on Test Data:", dt_accuracy))
print(paste("Decision Tree Kappa on Test Data:", dt_kappa))

# Calculate RMSE
dt_rmse <- sqrt(mean((as.numeric(predictions_dt) - as.numeric(test_data$year)) ^ 2))

# Calculate R-squared
dt_r_squared <- cor(as.numeric(predictions_dt), as.numeric(test_data$year))^2

# Print RMSE and R-squared
print(paste("Decision Tree RMSE on Test Data:", dt_rmse))
print(paste("Decision Tree R-squared on Test Data:", dt_r_squared))

# Print the accuracy and Kappa
print(paste("Decision Tree Accuracy on Test Data:", dt_accuracy))
print(paste("Decision Tree Kappa on Test Data:", dt_kappa))

# Calculate RMSE
dt_rmse <- sqrt(mean((as.numeric(predictions_dt) - as.numeric(test_data$year)) ^ 2))

# Calculate R-squared
dt_r_squared <- cor(as.numeric(predictions_dt), as.numeric(test_data$year))^2


# Print RMSE and R-squared
print(paste("Decision Tree RMSE on Test Data:", dt_rmse))
print(paste("Decision Tree R-squared on Test Data:", dt_r_squared))

