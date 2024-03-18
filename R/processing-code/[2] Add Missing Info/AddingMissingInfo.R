# ADDING MISSING INFO

# So I realized there are some artists which do not have genre information listed on Spotify.
# To remedy that, I am going to do some research and manually input those genres myself.

# Missing info is being taken from searching google, as not all artists genres are in the same place
# though I am doing my best to ensure at least multiple sources agree on an artists genre(s) before assigning it

# Load necessecary libraries
library(here)
library(dplyr)

# Load in data
data <- read.csv(here("data", "processed-data", "Intermediates", "unique_songs_mapped.csv"))

list_missing_info <- function(data, genres, artist) {
  # Find rows where the genre is NA
  missing_genre_indices <- is.na(data[[genres]])
  
  # Get the corresponding artist names
  artists_with_missing_genre <- data[missing_genre_indices, artist]
  
  # Print the list of artists
  cat(paste(artists_with_missing_genre, collapse = ", "))
}

list_missing_info(data, "genres", "artist")

# MAPPING INTO DATAFRAME

# Mapping genres to artists
artist_genre_mapping <- list(
  "The Teddy Bears" = "pop",
  "Maurice Williams & The Zodiacs" = "r&b_reggae",
  "Lawrence Welk And His Orchestra" = "pop", "jazz_blues",
  "Joe Dowell" = "pop",
  "Little Peggy March" = "pop",
  "Jimmy Soul" = "pop", "soul_funk",
  "Nino Tempo & April Stevens" = "pop", "rock",
  "Jimmy Gilmer And The Fireballs" = "rock",
  "Bobbie Gentry" = "country", "pop", "soul_funk",
  "Lulu" = "pop",
  "John Fred And The Playboys" = "rock",
  "The Rascals" = "r&b_reggae", "soul_funk",
  "Zager & Evans" = "rock",
  "The Honey Cone" = "r&b_reggae", "soul_funk",
  "The Raiders" = "rock",
  "Johnny Nash" = "pop", "r&b_reggae",
  "The Elton John Band" = "rock", "pop",
  "Walter Murphy & The Big Apple Band" = "disco", "funk", "jazz_blues",
  "Rick Dees & His Cast Of Idiots" = "r&b_reggae", "soul_funk",
  "Marilyn McCoo & Billy Davis, Jr." = "r&b_reggae", "soul_funk", "pop",
  "Mary Macgregor" = "rock", "country", "classic_folk",
  "Alan O'Day" = "pop", "rock",
  "Joe Cocker And Jennifer Warnes" = "pop", "rock",
  "Toni Basil" = "rock", "pop",
  "Gregory Abbott" = "r&b_reggae", "soul_funk",
  "Billy Vera & The Beaters" = "r&b_reggae", "soul_funk",
  "Bill Medley & Jennifer Warnes" = "rock",
  "Gloria Estefan & Miami Sound Machine" = "pop",
  "The Escape Club" = "pop", "rock",
  "Timmy T." = "pop",
  "Londonbeat" = "electronic_dance",
  "Prince And The N.P.G." = "pop", "rock", "r&b_reggae",
  "Peabo Bryson & Regina Belle" = "pop", "r&b_reggae", "soul_funk",
  "Lady Gaga Featuring Colby O'Donis" = "dance", "pop",
  "LMFAO Featuring Lauren Bennett & GoonRock" = "pop", "dance",
  "Wiz Khalifa Featuring Charlie Puth" = "pop", "hip_hop_rap",
  "Rihanna Featuring Drake" = "pop", "hip_hop_rap",
  "Maroon 5 Featuring Cardi B" = "pop", "rock",
  "The Weeknd & Ariana Grande" = "r&b_reggae", "pop"
)


update_genres <- function(data, mapping) {
  # Iterate over the mapping to update the data
  for (artist in names(mapping)) {
    # Find rows with the current artist and update main_genres
    genre <- mapping[[artist]]
    data <- data %>%
      mutate(main_genres = ifelse(artist == !!artist, genre, main_genres))
  }
  return(data)
}

# Update the data
data <- update_genres(data, artist_genre_mapping)


# Checking again for artists with missing genre info
missing_info <- data %>%
  filter(is.na(main_genres)) %>%
  select(artist) %>%
  unique()

# Print missing artists
print(missing_info)

# Now, the 'main_genres' column should be updated with genre information 
# for artists that were previously marked as NA, without affecting rows that already had genre information.

# COMBINE DATASETS

#Load necessary libraries
library(dplyr)
library(readr) 
library(lubridate)
library(here)
library(tidyr)

# Load the datasets
hot100_processed <- read.csv(here("data", "processed-data", "Hot_100_Processed.csv")) 



# Fill in missing values in 'main_genres' based on matching 'artist' and 'song'
hot100_processed_updated <- data %>%
  left_join(data, by = c("artist", "song"), suffix = c("", "_new"))

# For each column that we wanted to replace, we'll update it from the new columns
hot100_processed_updated <- hot100_processed_updated %>%
  mutate(
    main_genres = coalesce(main_genres_new, main_genres))%>%
    select(-ends_with("_new"))  # Remove the new columns after replacing the original ones

# Checking again for artists with missing genre info
missing_info <- hot100_processed_updated %>%
  filter(is.na(main_genres)) %>%
  select(artist) %>%
  unique()

# Print missing artists
print(missing_info)

# Saving Files

write.csv(hot100_processed_updated, here("data", "processed-data", "Hot_100_Processed.csv"), row.names = FALSE)

write.csv(data, here("data", "processed-data", "Intermediates", "hot100_unique_with_missing_info.csv"), row.names = FALSE)

