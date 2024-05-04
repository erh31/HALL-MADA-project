# Load libraries
library(here)
library(knitr)
library(ggplot2)
library(webshot2)
library(kableExtra)

# Extract performance metrics for each model
rf_accuracy <- 0.0652173913043478
rf_kappa <- 0.042594385285576
rf_rmse <-11.7366945943055
rf_r_squared <- 0.448803636104838

dt_accuracy <- 0.271739130434783
dt_kappa <- 0.251760135955329
dt_rmse <- 13.2176594652431
dt_r_squared <- 0.339970248839123

gbm_accuracy <- 0.0869565217391304
gbm_kappa <- 0.0644067796610169
gbm_rmse <- 13.4099249422404
gbm_r_squared <- 0.337379660548738

# Create a data frame to store the metrics
comparison <- data.frame(Model = c("Random Forest", "Decision Tree", "GBM"),
                         Accuracy = c(rf_accuracy, dt_accuracy, gbm_accuracy),
                         Kappa = c(rf_kappa, dt_kappa, gbm_kappa),
                         RMSE = c(rf_rmse, dt_rmse, gbm_rmse),
                         R_squared = c(rf_r_squared, dt_r_squared, gbm_r_squared))

# Print the comparison
print(comparison)

# Print the comparison as a nicely formatted table
comparison_table <- kable(comparison, format = "html") %>%
  kable_styling(full_width = TRUE)

# Save the table as an image 
save_kable(comparison_table, here("results", "tables", "model_comparison.png"))

# Plot the comparison
ggplot(comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison based on Accuracy",
       x = "Model", y = "Accuracy") +
  theme_minimal()

# Plot RMSE comparison
ggplot(comparison, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison based on RMSE",
       x = "Model", y = "RMSE") +
  theme_minimal()

# Plot R-Squared comparison
ggplot(comparison, aes(x = Model, y = R_squared, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison based on R-Squared",
       x = "Model", y = "R-Squared") +
  theme_minimal()

# Define color palette
my_colors <- c("#ffacac", "#b3fdde", "#a9adff")

# Define color palette for bar outlines
outline_colors <- c("#ff8e8e", "#74fbc3", "#8b91ff") 

# Combine all comparisons into a single plot
comparison_long <- tidyr::pivot_longer(comparison, cols = c(Accuracy, RMSE, R_squared),
                                       names_to = "Metric", values_to = "Value")

ggplot(comparison_long, aes(x = Model, y = Value, fill = Model, color = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), size = 1.5) +  # Add position_dodge to separate bars
  scale_fill_manual(values = my_colors) +  # Apply fill color palette
  scale_color_manual(values = outline_colors) +  # Apply outline color palette
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(title = "Model Comparison",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        plot.background = element_rect(fill = "white"))  # Set white background

# Save the ggplot object to a file in the project directory
ggsave(here("results", "figures","comparison_plot.png"), plot = last_plot(), width = 7, height = 8, units = "in")


