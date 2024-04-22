---
title: "Codebook"
author: "Elizabeth Hall"
output:
  html_document:
    keep_md: yes
---

------------------------------------------------------------------------

### Project Description

Leveraging extensive data from the Billboard charts and genre information from Spotify, this project aims to provide an examination of genre trends over time.

------------------------------------------------------------------------

### Raw Data

Raw data was collected from the Billboard Hot 100s website. The dataset for this study was downloaded from user HipsterVizNinja on GitHub.

The raw dataset can be downloaded [here](https://github.com/HipsterVizNinja/random-data/tree/main/Music/hot-100).

\

### Processed Data:

#### SpotifyAPI Info:

The Spotify Web API is free, and only requires an account to access.

Here are some resources which are helpful for getting started:

-   [Spotify Web API - Getting Started](https://developer.spotify.com/documentation/web-api)

-   ![]()[Spotify Web API - API Calls](https://developer.spotify.com/documentation/web-api/concepts/api-calls)

-   [R Wrapper for the Spotify Web API](https://www.rcharlie.com/spotifyr/)

\

#### How to Create Processed Datafile:

1.  Run `DataCleaning.R` on raw data, save processed data.
2.  Run `AddingMissingInfo.R` on processed data from previous step.

\

#### Description of Data Processing:

#### `DataCleaning.R`

1.  **Libraries**: The scripts utilize libraries like **`tidyverse`**, **`dplyr`**, **`lubridate`**, and **`spotifyr`** for data manipulation and accessing Spotify APIs.

2.  **Data Loading and Transformation**:

    -   The data is loaded from a CSV file containing details about Hot 100 chart entries.

    -   Columns irrelevant to further analysis, such as **`chart_debut`** and **`song_id`**, are removed.

    -   The **`chart_date`** is converted to a Date type and used to extract additional temporal features like year, month, and week.

3.  **Data Condensation**:

    -   Songs are grouped by year, month, and week, and only the top songs based on peak position and time on the chart are retained.

    -   Spotify song and artist IDs are fetched using Spotify's search API, with a rate limit handling to avoid exceeding the API call quota.

    -   Genres and audio features for each track are retrieved from Spotify and added to the dataset.

4.  **Simplifying Genres**:

    -   Genres are categorized into major genre groups using a combination of manual sorting and automated mapping based on string matching. Straggler genres are manually categorized after verifying their classification through external sources (e.g., Google searches).

5.  **Final Data Aggregation**:

    -   Data files are merged to combine genre information, Spotify IDs, and audio features into the main dataset.

    -   Additional columns for audio features and genres are merged based on song and artist matches.

\

#### `AddingMissingInfo.R`

1.  **Missing Data Handling**:

    -   Missing genre information for some artists is manually updated by researching and validating through multiple sources.

2.  **Data Integrity**:

    -   Ensures all songs have the necessary metadata, including genre and Spotify identifiers, to facilitate comprehensive analysis.

3.  **Output**:

    -   The cleaned and fully populated dataset is saved in CSV format for easy access and use in subsequent analyses.

------------------------------------------------------------------------

### `Hot_100s_Processed.csv` Info:

#### General Description of Dataset:

-   **Dimensions of the Dataset:** The dataset contains 589 entries and 28 variables.
-   **Dataset Summary:** The dataset contains a condensed and cleaned version of the Billboard Hot 100s data, supplemented with info from the SpotifyAPI.

\

#### Variables:

1.  **chart_position**(num): the position of the song for the given chart date

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: 1-100~

    \

2.  **chart_date**(date): the date that the chart was released

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: YYYY-MM-DD~

    \

3.  **song**(chr): name of the song

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: Song Name~

    \

4.  **artist**(chr): artist credited to song

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: Artist Name~
    
    \

5.  **instance**(int): how many times a song returned to the chart after more than one week off the chart

    -   ~**Source**: Billboard Hot 100 charts~

    \

6.  **time_on_chart**(int): running count of weeks (all-time) a song has been on the chart

    -   ~**Source**: Billboard Hot 100 charts~

    \

7.  **consecutive_weeks**(int): per instance count of how many weeks the song has been on the chart consecutively

    -   ~**Source**: Billboard Hot 100 charts~

    \

8.  **previous_week**(int): chart position for the previous week for the given instance

    -   ~**Source**: Billboard Hot 100 charts~

    \

9.  **peak_position**(int): the all-time best/peak position of a song

    -   ~**Source**: Billboard Hot 100 charts~

    \

10. **worst_position**(int): the all time worst/lowest position of a song

    -   ~**Source**: Billboard Hot 100 charts~

    \

11. **year**(int): year that the song instance was on the chart

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: 1958-2023~

    \

12. **month**(int): month that the song instance was on the chart

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: 1-12~

    \

13. **week**(int): week number that the song instance was on the chart

    -   ~**Source**: Billboard Hot 100 charts~
    -   ~**Format**: 1-53~

    \

14. ~**spotify_song_id**(chr): the Spotify ID for the song~

    -   ~**Source**: Spotify API~
    -   ~**Format**: number/letter string~

    \

15. **spotify_artist_id**(chr): the Spotify ID for the artist

    -   ~**Source**: Spotify API~
    -   ~**Format**: number/letter string~

    \

16. **genres**(chr): genres the artist is associated with

    -   ~**Source**: Spotify API~
    -   ~**Format**: comma seperated list~

    \

17. **danceability**(num): how suitable a track is for dancing

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is least danceable, 1 is most danceable~

    \

18. **energy**(num): perceptual measure of intensity and activity

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is least energetic, 1 is most energetic~

    \

19. **key**(int): the key the track is in

    -   ~**Source**: Spotify API~
    -   ~**Format**: -1-11~
    -   ~**Info**: integers map to pitches using standard pitch class notation~

    \

20. **loudness**(num): overall loudness of a track in decibels

    -   ~**Source**: Spotify API~
    -   ~**Format**: -60-0~

    \

21. **mode**(int): the modality of a track

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is minor, 1 is major~

    \

22. **speechiness**(num): the presence of spoken words in a track

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 has no spoken words, 1 is all spoken words~

    \

23. **acousticness**(num): confidence measure of whether the track is acoustic

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is low confidence, 1 is high confidence~

    \

24. **instrumentalness**(num): predicts whether a track contains no vocals

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 contains vocal content, 1 contains no vocal content~

    \

25. **liveness**(num): presence of an audience in song recording

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is low probability of live recording, 1 is high probability of live recording~

    \

26. **valence**(num): measure of musical positiveness conveyed by a track

    -   ~**Source**: Spotify API~
    -   ~**Format**: 0-1~
    -   ~**Info**: 0 is low musical positiveness, 1 is high musical positiveness~

    \

27. **tempo**(num): the overall estimated tempo of a track in beats per minute

    -   ~**Source**: Spotify API~
    -   ~**Format**: BPM~

    \

28. **main_genres**(chr): broad genre categories derived from 'genres' variable

    -   ~**Source**: Spotify API/Independent Research~
    -   ~**Categories**: pop, rock, country, jazz_blues, soul_funk, hip_hop_rap, electronic_dance, r&b_reggae, classic_folk, world_regional, indie_alt, other~

    \

------------------------------------------------------------------------
