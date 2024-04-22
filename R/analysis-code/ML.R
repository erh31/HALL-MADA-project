# Load libraries
library(here)
library(dials)
library(tidyr)
library(dplyr)
library(themis) 
library(discrim)
library(parsnip)
library(xgboost)
library(yardstick)
library(tidymodels)

# Set RNG value for reproducibility
rngseed <- 42
set.seed(rngseed)

# Load the dataset
df <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv"))

# Remove 'genres' column for clarity
df_split <- subset(df, select = -genres)

# Split the 'main_genre' column into multiple rows
df_split <- separate_rows(df_split, main_genres, sep = ", ") %>%
  rename(genre = main_genres)

# Select for only variables of interest
df_select <- df_split %>% select(genre, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>% 
  mutate(across(c(genre), as.factor)) 

# Define the outcome and predictors
outcome <- "genre"
predictors <- c("danceability", "energy", "loudness", "speechiness", 
                "acousticness", "instrumentalness", "liveness", "valence", "tempo")

# Creating recipie to balance genre counts using smote
df_balanced_rec <- recipe(genre ~ danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo, 
          data = df_select) %>%
  step_impute_mean(all_predictors()) %>%
  step_smote(genre, over_ratio = 0.30) %>%
  prep()



# Applying recipie to original dataset
df_balanced <- bake(df_balanced_rec, new_data = NULL)

# Checking unbalanced genre count
df_unbalanced <- df_select %>%
  filter(!is.na(genre))
count(df_unbalanced, genre)

# Checking balanced genre count
count(df_balanced, genre)

# Prepare data splits
set.seed(rngseed)
split <- initial_split(df_balanced, prop = 0.8)
train_set <- training(split)
test_set <- testing(split)

# Setup for cross-validation
cv_folds <- vfold_cv(train_set, v = 5, repeats = 3)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = dials::tree_depth(),
  min_n = dials::min_n(),
  loss_reduction = dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  mtry = dials::mtry(),
  learn_rate = dials::learn_rate()
) %>% 
  set_mode("classification") %>%
  set_engine("xgboost")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  finalize(mtry(), train_set),
  sample_size = sample_prop(),
  learn_rate(),
  size = 20
)

xgb_workflow <- workflow() %>%
  add_recipe(df_balanced_rec) %>%
  add_model(xgb_spec)

xgb_results <- tune_grid(
  xgb_workflow,
  resamples = cv_folds,
  grid = xgb_grid
)

