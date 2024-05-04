# Load libraries
library(randomForest)
library(ggplot2)
library(dplyr)
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

# Split data into training and test sets
set.seed(rngseed)  # for reproducibility
train_indices <- createDataPartition(data$year, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Set up cross-validation for model tuning
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Train the Random Forest model with extended hyperparameter tuning
model <- train(year ~ ., data = train_data, method = "rf",
               trControl = train_control,
               tuneLength = 10)  # Increase tuneLength to explore more hyperparameter combinations

# Print and plot model
print(model)
plot(model)

# Evaluate the model on test data
predictions_rf <- predict(model, newdata = test_data)

# Calculate performance metrics on test data
rf_accuracy <- confusionMatrix(predictions_rf, test_data$year)$overall["Accuracy"]
rf_kappa <- confusionMatrix(predictions_rf, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("Random Forest Kappa:", rf_kappa))


# Improving model based on results

# Define a new tuning grid with more mtry values
tuning_grid <- expand.grid(mtry = c(5, 6, 7, 8, 9))

# Train the model using the new tuning grid
model <- train(year ~ ., data = train_data, method = "rf",
               trControl = train_control,
               tuneGrid = tuning_grid)

# Print the summary of the trained model
print(model)

# Plot the model
plot(model)

# Evaluate the model on test data
predictions_rf <- predict(model, newdata = test_data)

# Calculate performance metrics on test data
rf_accuracy <- confusionMatrix(predictions_rf, test_data$year)$overall["Accuracy"]
rf_kappa <- confusionMatrix(predictions_rf, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("Random Forest Kappa:", rf_kappa))


# Refining model again

# Define a new tuning grid with more mtry values
tuning_grid <- expand.grid(mtry = c(5, 6, 9))

# Train the model using the new tuning grid
model <- train(year ~ ., data = train_data, method = "rf",
               trControl = train_control,
               tuneGrid = tuning_grid)

# Print the summary of the trained model
print(model)

# Plot the model
plot(model)

# Evaluate the model on test data
predictions_rf <- predict(model, newdata = test_data)

# Calculate performance metrics on test data
rf_accuracy <- confusionMatrix(predictions_rf, test_data$year)$overall["Accuracy"]
rf_kappa <- confusionMatrix(predictions_rf, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("Random Forest Kappa:", rf_kappa))


# Fitting final model

# Set seed for reproducibility
set.seed(rngseed)

# Train the final Random Forest model with mtry = 5
final_model <- randomForest(year ~ ., data = train_data, mtry = 6, ntree = 500, nodesize = 5)

# Print the model summary
print(final_model)

# Checking variable importance
importance(final_model)
varImpPlot(final_model)

# Evaluate the model on test data
predictions_rf <- predict(model, newdata = test_data)

# Calculate performance metrics on test data
rf_accuracy <- confusionMatrix(predictions_rf, test_data$year)$overall["Accuracy"]
rf_kappa <- confusionMatrix(predictions_rf, test_data$year)$overall["Kappa"]

# Print the accuracy and Kappa
print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("Random Forest Kappa:", rf_kappa))

# Calculate RMSE and R-squared
rf_rmse <- sqrt(mean((as.numeric(predictions_rf) - as.numeric(test_data$year))^2))
rf_r_squared <- cor(as.numeric(predictions_rf), as.numeric(test_data$year))^2

# Print RMSE and R-squared
print(paste("Random Forest RMSE:", rf_rmse))
print(paste("Random Forest R-squared:", rf_r_squared))

#----------------------------------------------------------------------------------------------------

# PREDICTED VS OBSERVED

# Ensure that 'predictions_rf' and 'test_data$year' are numeric
observed <- as.numeric(as.character(test_data$year))
predicted <- as.numeric(as.character(predictions_rf))

# Create a data frame for plotting
data_plot <- data.frame(Observed = observed, Predicted = predicted)

# Create a ggplot scatter plot of observed vs predicted values
p <- ggplot(data_plot, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +  # Points with semi-transparency
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Line of equality
  labs(x = "Observed Year", y = "Predicted Year", title = "Observed vs. Predicted Years") +
  theme_minimal() +  # Minimal theme
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        plot.background = element_rect(fill = "white")) 

# Print the plot
print(p)

# Save the ggplot object to a file in the project directory
ggsave(here("results", "figures","rf_observed_vs_predicted.png"), plot = last_plot(), width = 8, height = 6, units = "in")

# VARIABLE IMPORTANCE

# Get variable importance from the Random Forest model
importance_data <- importance(final_model)

# Convert the importance data to a data frame for plotting
importance_df <- data.frame(Variable = rownames(importance_data), Importance = importance_data[, "MeanDecreaseGini"])

# Reorder the dataframe based on Importance
importance_df <- importance_df %>% arrange(desc(Importance))

# Define colors for each audio feature
feature_colors <- c(instrumentalness = "#ffaedf", energy = "#deaeff", tempo ="#a9adff", 
                    danceability = "#aeddff", speechiness = "#b3fdde", valence ="#b3fcac", 
                    liveness = "#dffcac", acousticness = "#ffdeac", loudness = "#ffacac")

# Plotting the data with different colors for each bar
p <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width
  scale_fill_manual(values = feature_colors) +  # Use manual color scale based on predefined colors
  coord_flip() +  # Flip coordinates to make labels readable
  labs(x = "Variable", y = "Mean Decrease in Gini", title = "Variable Importance in Random Forest Model") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Increase font size for text
        axis.title = element_text(size = 14),  # Increase font size for axis titles
        title = element_text(size = 18, face = "bold"),  # Increase and bold the title font
        plot.title = element_text(hjust = 0.5))  # Center the plot title

# Print the plot
print(p)

# Another way to visualize the data

# Define colors for each audio feature (for the fill)
feature_colors <- c(instrumentalness = "#FF99E6", energy = "#6699FF", tempo = "#3399CC", 
                    danceability = "#00CC99", speechiness = "#00FF99", valence = "#99FF66", 
                    liveness = "#FFFF66", acousticness = "#FFB366", loudness = "#FF6666")


# Define colors for the outlines
feature_outline_colors <- c(instrumentalness = "#ff33cd", energy = "#004ce3", tempo = "#267399", 
                            danceability = "#009973", speechiness = "#00bf73", valence = "#50f100", 
                            liveness = "#d6d600", acousticness = "#ff870d", loudness = "#ff0d0d")

# Define colors for the outlines
# Edited so that outline colors match on legend
legend_outline_colors <- c(instrumentalness = "#ff870d", energy = "#009973", tempo = "#004ce3", 
                            danceability = "#ff33cd", speechiness = "#d6d600", valence = "#ff0d0d", 
                            liveness = "#00bf73", acousticness = "#267399", loudness = "#50f100")

# Create dot plot with colored outlines and matching legend
p <- ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_point(aes(fill = Variable, color = Variable), size = 4, shape = 21, stroke = 1.5) +  # Dots with colored outline
  scale_color_manual(values = feature_outline_colors) +
  scale_fill_manual(values = feature_colors) +  # Fill colors
  labs(x = "Mean Decrease in Gini", y = "Variable", title = "Variable Importance in Random Forest Model") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 18, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(title = "Variable", override.aes = list(color = legend_outline_colors, size = 4, stroke = 1.5)))  # Adjust legend to match dots

# Print the plot
print(p)

# Save the ggplot object to a file in the project directory
ggsave(here("results", "figures","audio_feature_importance.png"), plot = last_plot(), width = 7, height = 8, units = "in")



