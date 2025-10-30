# Loading libraries (run every time you start)     
# Think of this as opening your toolbox       
library(dplyr)     # For data manipulation - like excel functions   
library(ggplot2)   # For creating beautiful charts  
library(readr)     # For reading CSV files
library(corrplot)  # For correlation plots
library(plotly)    # For interactive charts
library(GGally)    # For advanced plotting
library(caret)     # For machine learning
library(viridis)   # For beeautiful color schemes
library(reshape2)  # For data reshaping
library(knitr)     # For nice table formatting  

# Let's confirm everything loades successfully    
print("SpotiTunes Analytics Setup Complete!")

# Load the spotify data  
spotify_data <- read_csv("data/spotify_data.csv")

# Let's take our first look at the data    
print("Dataset loaded successfully!")
print(paste("Number of songs:", nrow(spotify_data)))
print(paste("Number of features:", ncol(spotify_data)))

# See the column names 
colnames(spotify_data)
# Display first few rows to understand our data    
head(spotify_data)  
# Show the structure of columns   
str(spotify_data)

# Check for missing values      
# Missing data can affect our analysis    
missing_values <- colSums(is.na(spotify_data))
print("Missing values in each column:")
print(missing_values)

# Let's look at the column names to understand what we have   
print("Our musical features:")
print(colnames(spotify_data))


# Clean our data - like tuning instruments before concert
# Rename colums to tidy form   
spotify_data <- spotify_data |> 
  rename(
    index = Index,
    title = Title,
    artist = Artist,
    top_genre = `Top Genre`,
    year = Year,
    beats_per_minute = `Beats Per Minute (BPM)`,
    energy = Energy,
    danceability = Danceability,
    loudness = `Loudness (dB)`,
    liveness = Liveness,
    valence = Valence,
    length = `Length (Duration)`,
    acousticness = Acousticness,
    speechiness = Speechiness,
    popularity = Popularity
    
    
  )

# Remove any duplicate songs     
spotify_clean <-  spotify_data |> 
  distinct(title, artist, .keep_all = TRUE)

print(paste("Removed", nrow(spotify_data) - nrow(spotify_clean), "duplicate songs"))


# Create a popularity category for easier analysis    
# Target = 1 means popular, Target = 0 means less popular    
spotify_clean <- spotify_clean |> 
  mutate(
    popularity_label = case_when(
      popularity < 30 ~ "Low Popularity",
      popularity < 70 ~ "Medium Popularity",
      TRUE ~ "High Popularity"
    ),
    energy_level = case_when(
      energy < 30 ~ "Low Energy",
      energy < 70 ~ "Medium Energy",
      TRUE ~ "High Energy"
    ),
    # create dance ability categories     
    dance_level = case_when(
      danceability < 30 ~ "Not Danceable",
      danceability < 30 ~ "Moderately Danceable",
      TRUE ~ "Very Danceable"
    )
  )

print("Data cleaning complete!")











