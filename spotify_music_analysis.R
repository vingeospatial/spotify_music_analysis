# Loading libraries (run every time you start)     
# Think of this as opening your toolbox       
library(dplyr)     # For data manipulation - like excel functions   
library(ggplot2)   # For creating beautiful charts  
library(readr)     # For reading CSV files
library(corrplot)  # For correlation plots
library(plotly)    # For interactive charts
library(GGally)    # For advanced plotting
library(caret)     # For machine learning
library(viridis)   # For beautiful color schemes
library(reshape2)  # For data reshaping
library(knitr)     # For nice table formatting  

# Let's confirm everything loads successfully    
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
spotify_clean
print("Data cleaning complete!")



# LETS EXPLORE OUR MUSICAL UNIVERSE      

# 1. Basic statistics about popular vs less popular songs    
popularity_summary <- spotify_clean |> 
  group_by(popularity_label) |> 
  summarise(
    count = n(),
    avg_energy = round(mean(energy), 3),
    avg_danceability = round(mean(danceability), 3),
    avg_valence = round(mean(valence), 3),
    avg_loudness = round(mean(loudness), 3)
  )
print("Popularity Analysis:")
knitr::kable(popularity_summary)


# 2. Distribution of song popularity      
ggplot(spotify_clean, aes(x = popularity_label)) +
  geom_bar(fill = c("#FF6B35", "#004E64", "#FF00FF"), alpha = 0.8) +
  labs(title = "Distribution of Song Popularity",
       subtitle = "How many popular vs less popular songs do we have?",
       x = "popularity category",
       y = "Number of songs")+
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



# VISUALIZE THE SOUND OF POPULARITY     

# 1. Energy vs Danceability scatter plot    
p1 <- ggplot(spotify_clean, aes(x = energy, y = danceability, color = popularity_label)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("#FF6B35", "#004E64", "#FF00FF")) +
  labs(title = "🕺 Energy vs Danceability",
       subtitle = "Do popular songs have more energy and danceability?",
       x = "Energy Level",
       y = "Danceability",
       color = "Popularity") +
  theme_minimal()
print(p1)
 


# 2. Valence (Musical Happiness) distribution     
p2 <- ggplot(spotify_clean, aes(x = valence, fill = popularity_label)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  scale_fill_manual(values = c("#ff6b35", "#004E64", "#808080")) +
  labs(title = "Musical Happiness Distribution",
       subtitle = "Are popular songs happier?",
       x = "Valence",
       y = "Number of Songs",
       fill = "popularity") +
  theme_minimal()

print(p2)


# 3. Tempo analysis     
p3 <- ggplot(spotify_clean, aes(x = popularity_label, y = beats_per_minute, fill = popularity_label)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#FF6B35", "#004E64", "#808080")) +
  labs(title = "Tempo Comparison",
       subtitle = "Is there an optimal tempo for popularity?",
       x = "Popularity Category",
       y = "Tempo (BPM)",
       fill = "Popularity") +
  theme_minimal()

print(p3)


# LETS FIND RELATIONSHIPS BETWEEN MUSICAL FEATURES   

# select numerical features for correlation   
numerical_features <-  spotify_clean |> 
  select(acousticness, danceability, energy, liveness, loudness,
         speechiness, beats_per_minute, valence, popularity)

# Create correlation matrix     
correlation_matrix <- cor(numerical_features)

# Visualize correlations     
corrplot(correlation_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.8,
         tl.col = "black",
         title = "Musical Features Correlation Matrix",
         mar = c(0,0,2,0))

# Find strongest correlations with popularity  
target_correlations <- correlation_matrix[, "popularity"] |> 
  sort(decreasing = TRUE)

print("Features most correlated with popularity:")
print(round(target_correlations, 3))



# BUILD DEEPER INSIGHTS WITH RICH VISUALS     
 

# create beautiful, insightful visualizations  

# 1. Multi-feature comparison using violin plots    
feature_long <- spotify_clean |> 
  select(popularity_label, energy, danceability, valence, acousticness) |> 
  reshape2::melt(id.vars = "popularity_label")

ggplot(feature_long, aes(x = popularity_label, y = value, fill = popularity_label)) +
  geom_violin(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("#FF6B35", "#004E64", "#808080")) +
  labs(title = "Musical Feature Comparison",
       subtitle = "How do different features vary between popular and less popular songs?",
       x = "Popularity Category",
       y = "Feature Value",
       fill = "popularity") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))
  
# 
# 2. Top artists analysis
top_artists <- spotify_clean |>
  group_by(artist) |> 
  summarise(
    total_songs = n(),
    highly_popular_songs = sum(popularity_label == "High Popularity", na.rm = TRUE),
    medium_popular_songs = sum(popularity_label == "Medium Popularity", na.rm = TRUE),
    popularity_rate = round(highly_popular_songs  / total_songs * 100, 1)
  ) |> 
  filter(total_songs >= 3) |>  # Artists with at least 3 songs 
  arrange(desc(popularity_rate)) |> 
  head(10)  # ✅ use numeric argument here

View(top_artists)

  
ggplot(top_artists, aes(x = reorder(artist, popularity_rate), y = popularity_rate)) +
  geom_col(fill = "#004e64", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Artists by Success Rate",
       subtitle = "Artists with highest percentage of popular songs (min 3 songs)",
       x = "Artists",
       y = "Success Rate (%)") +
  theme_minimal()


# STEP 10: TRAIN A LOGISTIC REGRESSION TO PREDICT POPULARITY     
# let build a simple model to predict song popularity !  

# Prepare data for modeling    
set.seed(123)

# Create binary target variable
spotify_clean$popular_flag <- ifelse(spotify_clean$popularity >= 50, 1, 0)

# Split into training/testing
train_index <- createDataPartition(spotify_clean$popular_flag, p = 0.8, list = FALSE)
train_data <- spotify_clean[train_index, ]
test_data <- spotify_clean[-train_index, ]

# Logistic regression model
model_features <- c("acousticness", "danceability", "energy", "liveness", 
                    "loudness", "speechiness", "beats_per_minute", "valence")

model <- glm(popular_flag ~ ., 
             data = train_data[, c("popular_flag", model_features)], 
             family = "binomial")

# Predict
predictions <- predict(model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Accuracy
accuracy <- mean(predicted_classes == test_data$popular_flag)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Feature importance
feature_importance <- summary(model)$coefficients[, 4]  # p-values
important_features <- sort(feature_importance[2:length(feature_importance)])
print("Most important features (lowest p-values):")
print(round(important_features, 4))

View(test_data)

# STEP 11 : UPGRADE YOUR MODEL WITH RANDOM FOREST    
library(randomForest)

# Train a Random Forest model    
set.seed(123)
# spotify_clean$popular_flag <- ifelse(spotify_clean$popularity >= 50, 1, 0)
rf_model <-  randomForest(as.factor(popular_flag) ~ .,
                          data = train_data[, c("popular_flag", model_features)],
                          ntree = 100,
                          importance = TRUE)



# Make predictions   
rf_predictions <- predict(rf_model, test_data, type = "prob")[,2]
rf_classes <- ifelse(rf_predictions > 0.5, 1, 0)

# Accuracy of the Random Forest model    
rf_accuracy <- mean(rf_classes == test_data$popular_flag)
print(paste("Random Forest Accuracy:", round(rf_accuracy * 100, 2), "%"))

# feature importance scores 
importance(rf_model)


  





  




