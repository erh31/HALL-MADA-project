# CONDENSING DATASET

# loading necessecary libraries 
library(tidyverse)
library(dplyr)
library(lubridate)

# loading data, removing columns, and renaming variables
data <- read.csv("./data/raw-data/Hot_100.csv") %>%
  select(-chart_debut, -chart_url, -song_id)
data <- data %>% rename(artist = performer)

# Convert chart_date to Date type (if not already done)
data$chart_date <- as.Date(data$chart_date)

# Add year, month, and week columns for grouping
data$year <- year(data$chart_date)
data$month <- month(data$chart_date)

# Using week of the year, adjust accordingly if you prefer week of the month
data$week <- week(data$chart_date)

# Define a ranking mechanism and filter to the top song per week, per month, per year
top_songs_per_week_month_year <- data %>%
  group_by(year, month, week) %>%
  arrange(year, month, week, peak_position, desc(time_on_chart)) %>%
  slice(1) %>%
  ungroup()

# Remove duplicates for processing purposes
unique_songs <- top_songs_per_week_month_year %>%
  distinct(song, .keep_all = TRUE)



# GETTING SONG ID'S

# loading required libraries
library(spotifyr)
library(progress)

# setting Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "57747ecd81a847ca8f24909dc0b9583c")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "4d32166a2afc4e50996a187f80b2382e")
access_token <- get_spotify_access_token()

# Add a column for spotify_id if not exists
if (!"spotify_song_id" %in% colnames(unique_songs)) {
  unique_songs$spotify_song_id <- NA
}

fetch_spotify_song_ids <- function(df) {
  # Ensure access token is refreshed
  access_token <- get_spotify_access_token()
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(df), format = "[:bar] :percent ETA: :eta")
  
  # Delay to limit to 100 requests per minute
  delay <- 0.6 
  
  # Iterate through the dataset
  for (i in 1:nrow(df)) {
    # Clean artist and song names by removing leading and trailing whitespace
    artist <- trimws(df$artist[i])
    song <- trimws(df$song[i])
    
    # Construct the query string
    query <- paste(artist, song)
    
    # Search for the song on Spotify using the spotify_song_id
    search_result <- search_spotify(q = query, type = "track")
    
    if (length(search_result$id) > 0) {
      ssi <- search_result$id[1]
      df$spotify_song_id[i] <- ssi
    } else {
      df$spotify_song_id[i] <- NA
    }
    
    # Update progress bar
    pb$tick()
    
    # Pause to ensure we don't exceed 100 requests per minute
    # SpotifyAPI does not like it when you exceed 180 requests per minute
    # (found that one out the hard way...)
    Sys.sleep(delay)
  }
  
  return(df)
}


unique_songs_processed <- fetch_spotify_song_ids(unique_songs)


# GETTING ARTIST ID'S

# this has to be done separately, as the SpotifyAPI does not allow for two search types at the same time
# e.g. 'track' and 'artist' 

# Add a column for spotify_id if not exists
if (!"spotify_artist_id" %in% colnames(unique_songs_processed)) {
  unique_songs_processed$spotify_artist_id <- NA
}

fetch_spotify_artist_ids <- function(df) {
  # Ensure access token is refreshed
  access_token <- get_spotify_access_token()
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(df), format = "[:bar] :percent ETA: :eta")
  
  # Delay to limit to 100 requests per minute
  delay <- 0.6 
  
  # Iterate through the dataset
  for (i in 1:nrow(df)) {
    # Clean artist and song names by removing leading and trailing whitespace
    artist <- trimws(df$artist[i])
    
    # Construct the query string
    query <- paste(artist)
    
    # Search for the song on Spotify using the spotify_song_id
    search_result <- search_spotify(q = query, type = "artist")
    
    if (length(search_result$id) > 0) {
      sai <- search_result$id[1]
      df$spotify_artist_id[i] <- sai
    } else {
      df$spotify_artist_id[i] <- NA
    }
    # Update progress bar
    pb$tick()
    
    # Pause to ensure we don't exceed 100 requests per minute
    # SpotifyAPI does not like it when you exceed 180 requests per minute
    # (found that one out the hard way...)
    Sys.sleep(delay)
  }
  
  return(df)
}


unique_songs_processed <- fetch_spotify_artist_ids(unique_songs_processed)


# GETTING GENRES

# Before the loop, ensure the dataframe has a genres column
if (!"genres" %in% colnames(unique_songs_processed)) {
  unique_songs_processed$genres <- NA
}

fetch_genres <- function(df) {
  # Ensure access token is refreshed
  access_token <- get_spotify_access_token()
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(df), format = "[:bar] :percent ETA: :eta")
  
  # Delay to limit to 100 requests per minute
  delay <- 0.6 
  
  # Iterate through the dataset
  for (i in 1:nrow(df)) {
    # Clean artist and song names by removing leading and trailing whitespace
    artist_id <- trimws(df$spotify_artist_id[i])
    
    # Construct the query string
    artist_info <- get_artist(artist_id, access_token)
    
    if (length(artist_info$genres) > 0) {
      g <- paste(artist_info$genres, collapse = ", ")
      df$genres[i] <- g
    } else {
      df$genres[i] <- NA
      
    }
      # Update progress bar
      pb$tick()
      
      # Pause to ensure we don't exceed 100 requests per minute
      # SpotifyAPI does not like it when you exceed 180 requests per minute
      # (found that one out the hard way...)
      Sys.sleep(delay)
       }
  
    return(df)
  }

unique_songs_processed <- fetch_genres(unique_songs_processed)


# SIMPLIFYING GENRES

# in order to make things a little simpler for later
# im adding an extra column with the "main genre" 

# Function to extract variables from a column
extract_variables <- function(column) {
  all_variables <- unlist(strsplit(column, ", "))
  return(all_variables)
}

# Function to remove duplicates from a vector
remove_duplicates <- function(variables) {
  unique_variables <- unique(variables)
  return(unique_variables)
}

# Extract variables from the column
all_variables <- unlist(lapply(unique_songs_processed$genres, extract_variables))

# Remove duplicates
unique_variables <- remove_duplicates(all_variables)

# Print unique genre variables with double quotes around each element
cat(paste(sprintf('"%s"', unique_variables), collapse = ", "))

# i then used chat gpt to help me sort all of these genres into main genres
# i then tweaked what ChatGPT gave me and used those catgories to simplify the genres

# SIMPLIFYING GENRES INTO MAIN GENRES

# The printed list of genres
# This is not the prettiest way to do this, but it gets the job done
# "italian|arkansas|new orleans|memphis|louisiana|

genres <- c("adult standards", "doo-wop", "rock-and-roll", "rockabilly", "classic italian pop", "italian adult pop", 
            "deep adult standards", "rhythm and blues", "arkansas country", "classic country pop", "country", "country rock", 
            "NA", "easy listening", "louisiana blues", "memphis blues", "new orleans blues", "bubblegum pop", "vocal harmony group", 
            "hammond organ", "jazz organ", "cowboy western", "space age pop", "surf music", "lounge", "vocal jazz", "man's orchestra", 
            "folk rock", "mellow gold", "sunshine pop", "brill building pop", "soul", "ballroom", "classic girl group", 
            "classic soul", "motown", "classic rock", "singer-songwriter", "outlaw country", "chicago soul", "northern soul", 
            "southern soul", "jazz clarinet", "jazz blues", "piano blues", "soul blues", "soft rock", "classic garage rock", 
            "merseybeat", "enka", "beach music", "philly soul", "swamp pop", "belgian singer-songwriter", "british invasion", 
            "psychedelic rock", "rock", "dixieland", "harlem renaissance", "jazz trumpet", "new orleans jazz", "swing", "baroque pop", 
            "disco", "album rock", "hard rock", "protopunk", "memphis soul", "quiet storm", "folk", "beatlesque", "blues rock", 
            "cosmic american", "heartland rock", "melancholia", "military cadence", "british folk", "glam rock", "psychedelic folk", 
            "scottish singer-songwriter", "chicano punk", "detroit rock", "vaudeville", "acid rock", "afropop", "south african jazz", 
            "neo soul", "funk", "funk rock", "p funk", "psychedelic soul", "rock keyboard", "classic soundtrack", "classic canadian rock", 
            "american folk revival", "novelty", "halloween", "yacht rock", "vintage hollywood", "k-pop", "dance pop", "pop", "alt z", 
            "electropop", "canadian singer-songwriter", "permanent wave", "roots rock", "r&b", "blues", "piano rock", "nashville sound", 
            "operatic pop", "classic uk pop", "boy band", "canadian country", "art rock", "new mexico music", "jazz funk", "post-disco", 
            "british blues", "progressive rock", "symphonic rock", "europop", "swedish pop", "new wave pop", "australian dance", "hollywood", 
            "hi-nrg", "candy pop", "power pop", "synthpop", "modern blues rock", "modern southern rock", "alternative hip hop", "east coast hip hop", 
            "hip hop", "instrumental hip hop", "rap", "minneapolis sound", "country dawn", "glam metal", "new romantic", "australian rock", 
            "glam punk", "cyberpunk", "synthesizer", "new wave", "sophisti-pop", "dance rock", "rock drums", "contemporary r&b", 
            "urban contemporary", "synth funk", "scottish new wave", "jazz fusion", "british soul", "jangle pop", "paisley underground", 
            "irish rock", "new jack swing", "metal", "reggae fusion", "uk reggae", "freestyle", "girl group", "lovers rock", "reggae", 
            "bedroom soul", "diva house", "eurodance", "hip house", "christian music", "funk metal", "britpop", "grebo", "indietronica", 
            "madchester", "canadian pop", "old school hip hop", "alternative r&b", "rap latina", "trap queen", "hip pop", "lilith", 
            "pop rock", "roots reggae", "atl hip hop", "g funk", "gangster rap", "west coast rap", "tropical", "hardcore hip hop", 
            "conscious hip hop", "new jersey rap", "neo mellow", "post-grunge", "alternative metal", "nu metal", "barbadian pop", 
            "canadian rock", "pop rap", "st louis rap", "talent show", "detroit hip hop", "queens hip hop", "dancehall", "dirty south rap", 
            "old school atlanta hip hop", "southern hip hop", "trap", "crunk", "north carolina hip hop", "rap kreyol", "canadian latin", 
            "country rap", "redneck", "modern rock", "neon pop punk", "pop punk", "miami hip hop", "art pop", "russian pop", "pop soul", 
            "uk pop", "afrofuturism", "australian pop", "seattle hip hop", "metropopolis", "nz pop", "memphis hip hop", "australian hip hop", 
            "canadian contemporary r&b", "singer-songwriter pop", "denpa-kei", "rhythm game", "trap soul", "slap house", "etherpop", 
            "indie poptimism", "melodic rap", "lgbtq+ hip hop", "dfw rap", "indie electropop", "gauze pop", "pov: indie", "shiver pop")

# Initialize empty lists for each main genre category
pop_genres <- list()
rock_genres <- list()
country_genres <- list()
jazz_blues_genres <- list()
soul_funk_genres <- list()
hip_hop_rap_genres <- list()
electronic_dance_genres <- list()
`r&b_reggae_genres` <- list()
world_regional_genres <- list()
indie_alt_genres <- list()
classic_folk_genres <- list()
other_genres <- list()
no_genre <- list() 

# Sort genres into lists
# Allows subgenres to fall into multiple main genre categories
for (genre in genres) {
  if (grepl("pop", genre)) pop_genres <- c(pop_genres, genre)
  if (grepl("rock", genre)) rock_genres <- c(rock_genres, genre)
  if (grepl("country", genre)) country_genres <- c(country_genres, genre)
  if (grepl("jazz|blues", genre)) jazz_blues_genres <- c(jazz_blues_genres, genre)
  if (grepl("soul|funk", genre)) soul_funk_genres <- c(soul_funk_genres, genre)
  if (grepl("hip hop|rap", genre)) hip_hop_rap_genres <- c(hip_hop_rap_genres, genre)
  if (grepl("electronic|dance", genre)) electronic_dance_genres <- c(electronic_dance_genres, genre)
  if (grepl("r&b|reggae", genre)) `r&b_reggae_genres` <- c(`r&b_reggae_genres`, genre)
  if (grepl("folk", genre)) classic_folk_genres <- c(classic_folk_genres, genre) # not sorting by 'classic' because 'classic rock' or similar examples would be miscategorized
  if (grepl("indie|alternative", genre)) indie_alt_genres <- c(indie_alt_genres, genre)
  
  # this one is longer, because there are a lot of world/regional genres in the list
  # provided ChatGPT with the list of genres and had it help me pick out the cities/nationalities
  if (grepl("\\b(italian|new mexico|arkansas|louisiana|memphis|new orleans|belgian|british|scottish|chicano|detroit|south african|american|canadian|swedish|australian|hollywood|irish|uk|barbadian|st louis|queens|atlanta|atl|miami|russian|seattle|nz|dfw)\\b", genre)) world_regional_genres <- c(world_regional_genres, genre)
}

# Sorting the stragglers

# Define genre_mapping based on the populated lists
genre_mapping <- list(
  pop = pop_genres,
  rock = rock_genres,
  country = country_genres,
  jazz_blues = jazz_blues_genres,
  soul_funk = soul_funk_genres,
  hip_hop_rap = hip_hop_rap_genres,
  electronic_dance = electronic_dance_genres,
  `r&b_reggae` = `r&b_reggae_genres`,
  world_regional = world_regional_genres,
  indie_alt = indie_alt_genres,
  classic_folk = classic_folk_genres,
  other = other_genres,
  no_genre = no_genre
)


# Combine all genres from the genre_mapping into a single vector
all_categorized_genres <- unlist(genre_mapping)

# Function to find genres not present in the aggregated list
find_missing_genres <- function(genres_list, all_categorized_genres) {
  missing_genres <- list() # Initialize an empty list for missing genres
  
  # Loop through the original genres list
  for (genre in genres_list) {
    # Check if the genre is not in the aggregated genres vector
    if (!genre %in% all_categorized_genres) {
      missing_genres <- c(missing_genres, genre) # Add missing genre to the list
    }
  }
  
  return(missing_genres)
}

# Use the function with the original genres list and the aggregated categorized genres
missing_genres <- find_missing_genres(genres, all_categorized_genres)

# Print the list of missing genres
cat(paste(missing_genres, collapse = ", "))


# The sorting of the remaining genres was done manually, now that the list had
# been narrowed down to only those whose parent genre was not apparent

# I googled each of the remaining genres, and sorted them into the main genre(s) to
# which most sources seemed to agree that they belong
# INCLUDE A LIST MAYBE???

# Manually adding straggler to their genre lists
genre_mapping$pop <- c(genre_mapping$pop, "lounge",  "beach music", "disco", "permanent wave",  "boy band",  "post-disco",  "paisley underground", "girl group",  "madchester")
genre_mapping$rock <- c(genre_mapping$rock, "surf music", "merseybeat", "beach music", "british invasion", "protopunk", "beatlesque", "cosmic american", "permanent wave", "minneapolis sound", "glam metal", "new romantic", "glam punk", "new wave", "metal", "paisley underground", "grebo", "post-grunge", "nu metal")
genre_mapping$country <- c(genre_mapping$country, "cowboy western", "cosmic american",  "nashville sound", "redneck")
genre_mapping$jazz_blues <- c(genre_mapping$jazz_blues, "afrofuturism", "cosmic american", "dixieland", "hammond organ", "harlem renaissance", "swing")
genre_mapping$soul_funk <- c(genre_mapping$soul_funk, "afrofuturism", "disco", "minneapolis sound", "motown", "new jack swing", "permanent wave")
genre_mapping$hip_hop_rap <- c(genre_mapping$hip_hop_rap,  "alt z", "urban contemporary",  "crunk",  "afrofuturism", "new jack swing")
genre_mapping$electronic_dance <- c(genre_mapping$electronic_dance, "afrofuturism", "ballroom",  "post-disco", "disco", "hi-nrg",  "cyberpunk",  "synthesizer",  "diva house", "hip house", "slap house")
genre_mapping$`r&b_reggae` <- c(genre_mapping$`r&b_reggae`, "doo-wop", "merseybeat", "motown", "new jack swing", "quiet storm")
genre_mapping$classic_folk <- c(genre_mapping$classic_folk, "adult standards", "deep adult standards", "easy listening", "vocal harmony group",  "classic girl group", "military cadence", "classic soundtrack", "nashville sound", "lilith")
genre_mapping$world_regional <- c(genre_mapping$world_regional, "enka", "vaudeville", "freestyle",  "tropical", "denpa-kei")
genre_mapping$indie_alt <- c(genre_mapping$indie_alt, "man's orchestra", "mellow gold",  "grebo",  "madchester",  "lilith", "neo mellow")
genre_mapping$other <- c(genre_mapping$other, "singer-songwriter", "melancholia", "novelty", "halloween", "vintage hollywood",  "hollywood", "christian music",  "talent show", "rhythm game")
genre_mapping$no_genre <- c(genre_mapping$no_genre,"NA")

# Re-check for missing genres
all_categorized_genres <- unlist(genre_mapping)
missing_genres <- find_missing_genres(genres, all_categorized_genres)
cat(paste(missing_genres, collapse = ", "))


# MAPPING GENRES TO THE DATASET

# Function to map subgenres to main genres and append them to a new column
map_subgenres_to_main <- function(df, genre_mapping) {
  # Create a new column 'main_genres' initialized with empty strings
  df$main_genres <- ""
  
  # Iterate over each row of the dataframe
  for (i in 1:nrow(df)) {
    main_genres_set <- character(0) # Initialize an empty vector to store main genres for this row
    
    # Extract the subgenres list for the current row, assuming they are comma-separated
    subgenres_list <- strsplit(as.character(df$genres[i]), ", ")[[1]]
    
    # Iterate over each subgenre and map it to its main genres
    for (subgenre in subgenres_list) {
      for (main_genre in names(genre_mapping)) {
        if (subgenre %in% genre_mapping[[main_genre]]) {
          # Append the main genre if the subgenre is found within its list
          main_genres_set <- unique(c(main_genres_set, main_genre))
        }
      }
    }
    
    # Concatenate the unique main genres back to a comma-separated string
    df$main_genres[i] <- paste(main_genres_set, collapse = ", ")
  }
  
  return(df)
}

# Apply the function to your dataframe
unique_songs_mapped <- map_subgenres_to_main(unique_songs_processed, genre_mapping)

# COMBINE DATASETS

#Creating dataset, but not removing duplicates

# loading data, removing columns, and renaming variables
hot100_processed <- read.csv("./data/raw-data/Hot_100.csv") %>%
  select(-chart_debut, -chart_url, -song_id)
hot100_processed <- hot100_processed %>% rename(artist = performer)

# Convert chart_date to Date type (if not already done)
hot100_processed$chart_date <- as.Date(hot100_processed$chart_date)

# Add year, month, and week columns for grouping
hot100_processed$year <- year(hot100_processed$chart_date)
hot100_processed$month <- month(hot100_processed$chart_date)
# Using week of the year, adjust accordingly if you prefer week of the month
hot100_processed$week <- week(hot100_processed$chart_date)

# Define a ranking mechanism and filter to the top song per week, per month, per year
hot100_processed <- hot100_processed %>%
  group_by(year, month, week) %>%
  arrange(year, month, week, peak_position, desc(time_on_chart)) %>%
  slice(1) %>%
  ungroup()

# Merging datasets
hot100_processed <- left_join(hot100_processed, unique_songs_mapped, by = c("artist", "song"))

# Print to csv
write.csv(hot100_processed, "./data/processed-data/Hot_100.csv", row.names=FALSE)


