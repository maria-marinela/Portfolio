################################################### UPLOAD DATASET AND CLEANSE IT #####################################
# Installare e caricare i pacchetti necessari
library(tidyverse)
library(ggplot2)
library(factoextra)
library(rpart)
library(rattle)
library(GGally)  
library(tidyr)
library(gridExtra)
library(dplyr)

data <- read.csv("dataset_socialmedia.csv")

# elimino na /nan 
cleaned_data <- na.omit(data)
cleaned_data <- cleaned_data %>% filter(!is.na(Gender) & Gender %in% c("Male", "Female", "Non-binary"))

# Check 
str(cleaned_data)

# trasfrom age in numeric (it is characteristic)
cleaned_data$Age <- as.numeric(cleaned_data$Age)

############################################# OUTLIER #######################################################

# uso IQR method per trovare gli outlier 
identify_outliers <- function(data) {
  outlier_list <- list()
  
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      outliers <- data %>% filter(data[[col]] < (Q1 - 1.5 * IQR) | 
                                    data[[col]] > (Q3 + 1.5 * IQR))
      outlier_list[[col]] <- outliers
    }
  }
  
  return(outlier_list)
}

# Applica al dataset 
outliers_all <- identify_outliers(cleaned_data)

# Print the outliers per ogni colonna 
for (col in names(outliers_all)) {
  cat("Outliers for column:", col, "\n")
  print(outliers_all[[col]])
  cat("\n")
}

## gli 'outlier'che ci sono secondo me non sono outliers bensì le caratteristiche degli users, quelli che presentano outliers in posts o likes forse è perchè sono creators o influencers o hanno i profili pubblici ? 

##############################KEY STATISTICS#########################################

# Seleziono solo le features che mi interessano 
selected_columns <- cleaned_data[, c('Age', 'Daily_Usage_Time..minutes.', 'Posts_Per_Day', 
                                     'Likes_Received_Per_Day', 'Comments_Received_Per_Day', 'Messages_Sent_Per_Day')]

# Funzione per calcolare mean, min, and max
compute_stats <- function(x) {
  stats <- c(Average = mean(x, na.rm = TRUE), 
             Min = min(x, na.rm = TRUE), 
             Max = max(x, na.rm = TRUE))
  return(stats)
}

# Applicare la funzione ad ogni colonna 
key_statistics <- sapply(selected_columns, compute_stats)
key_statistics <- t(key_statistics)

# trasformo in un dataframe così mi è più facile visualizzare
key_statistics_df <- as.data.frame(key_statistics)

# Printo i risultati - commenti direttamente in report 
print(key_statistics_df)

##################################################### EDA #################################################
# Age Distribution
ggplot(cleaned_data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "pink", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Age Distribution", x = "Age", y = "Density") +
  theme_minimal()

# Daily Usage Time Distribution
ggplot(cleaned_data, aes(x = Daily_Usage_Time..minutes.)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Daily Social Media Usage Time (Minutes)", x = "Daily Usage Time (minutes)", y = "Density") +
  theme_minimal()

# Posts Per Day Distribution
ggplot(cleaned_data, aes(x = Posts_Per_Day)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Posts Per Day", x = "Posts Per Day", y = "Density") +
  theme_minimal()

# Likes Received Per Day Distribution
ggplot(cleaned_data, aes(x = Likes_Received_Per_Day)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "yellow", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Likes Received Per Day", x = "Likes Per Day", y = "Density") +
  theme_minimal()

# Comments Received Per Day Distribution
ggplot(cleaned_data, aes(x = Comments_Received_Per_Day)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "lightpink", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Comments Received Per Day", x = "Comments Per Day", y = "Density") +
  theme_minimal()

# Messages Sent Per Day Distribution
ggplot(cleaned_data, aes(x = Messages_Sent_Per_Day)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Messages Sent Per Day", x = "Messages Per Day", y = "Density") +
  theme_minimal()

### boxplot analysis 

# # Boxplot per Posts Per Day
ggplot(cleaned_data, aes(x = Gender, y = Posts_Per_Day, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Posts Per Day by Gender", x = "Gender", y = "Posts Per Day") +
  theme_minimal()

# Boxplot per Likes Received Per Day
ggplot(cleaned_data, aes(x = Gender, y = Likes_Received_Per_Day, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Likes Received Per Day by Gender", x = "Gender", y = "Likes Received Per Day") +
  theme_minimal()

# Boxplot per Daily Usage Time
ggplot(cleaned_data, aes(x = Gender, y = Daily_Usage_Time..minutes., fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Daily Usage Time by Gender", x = "Gender", y = "Daily Usage Time (minutes)") +
  theme_minimal()


# Correlation Heatmap ##è stranissima##
library(ggcorrplot)

# Seleziono solo i numeric 
numeric_data <- cleaned_data %>% select(Age, Daily_Usage_Time..minutes., Posts_Per_Day, Likes_Received_Per_Day, Comments_Received_Per_Day, Messages_Sent_Per_Day)
cor_matrix <- cor(numeric_data, use = "complete.obs")
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, colors = c("tomato2", "white", "springgreen3"),
           title = "Correlation Heatmap for Numerical Variables", ggtheme = theme_minimal())


# Creare un bar plot con le etichette corrette per i generi
ggplot(cleaned_data, aes(x = Gender, fill = Dominant_Emotion)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Dominant emotionio by gender", 
       x = "Gender", y = "Count", fill = "Dominant Emotion") +
  theme_minimal()

# Boxplot per visualizzare la distribuzione del Daily Usage Time per ciascuna piattaforma
ggplot(cleaned_data, aes(x = Platform, y = Daily_Usage_Time..minutes., fill = Platform)) +
  geom_boxplot() +
  labs(title = "Distribuzione del Daily Usage Time per Piattaforma",
       x = "Piattaforma",
       y = "Tempo di utilizzo giornaliero (minuti)") +
  theme_minimal()

# Calcolo della media e deviazione standard del tempo di utilizzo per ciascuna piattaforma
summary_stats <- cleaned_data %>%
  group_by(Platform) %>%
  summarise(mean_usage = mean(Daily_Usage_Time..minutes., na.rm = TRUE),
            sd_usage = sd(Daily_Usage_Time..minutes., na.rm = TRUE))

print(summary_stats)


################################# PCA ######################################################

# Seleziono le colonne per il PCA 
numeric_columns <- cleaned_data[, c('Daily_Usage_Time..minutes.', 'Posts_Per_Day', 
                                    'Likes_Received_Per_Day', 'Comments_Received_Per_Day', 
                                    'Messages_Sent_Per_Day')]

# Standardizzo
scaled_data <- scale(numeric_columns)

# Applico la PCA, tengo solo i primi due 
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Summary of PCA ( mi dà l'explained variance)
summary(pca_result)

# aggiungo i PCA al dataset come delle nuove colonne 
cleaned_data$PCA1 <- pca_result$x[,1]
cleaned_data$PCA2 <- pca_result$x[,2]


## PCA1 lo chiamo Livello di utilizzo sui social perchè è come una sintesi delle caratteristiche principali di utilizzo dei social media (tempo di utilizzo, interazioni come likes e commenti)
## PCA2 invece potrebbe descrivere il coinvolgimento nell'utilizzo dei social 

## attenzione: il primo cattura il 92.8 della varianza il secondo solo il 2%, direi che basta il primo (gli altri anche valgono pochissimo)
##se li tebgo entrambi allora mi spiegano ill 95% della var del dataset, è ottimo 

# Plot 
ggplot(cleaned_data, aes(x = PCA1, y = PCA2, color = Platform)) +
  geom_point() +
  labs(title = "PCA - Data Reduction to 2 Components", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()



########################## REGRESSION MODEL ###########################################################

# Linear regression model
model <- lm(Daily_Usage_Time..minutes. ~ Posts_Per_Day + Likes_Received_Per_Day + 
              Comments_Received_Per_Day + Messages_Sent_Per_Day, data = cleaned_data)

# Summary of the model
summary(model)

############### COMMENTI REGRESSIONE ################################
## qui voglio predirre il tempo di utilizzo basandomi sulle interazioni sui social. 
## piccola analisi del summary per non dimenticare: 
### ci sono nei min negativi = alcune predicitons più alte del valore vero 
### la mediana è quasi zero, quindi forse non è biased 
### attenzione, sembra che per ogni post per day in più il tempo di utilizzo si allunga di 1.50 min 
### sembra che i commenti non aiutino molto la prediction perchè il p-value è 0.437 (e non ha la stella)
###### ricorda che una stella = signficance at 0.05 level

### dalla R-squared adjusted capisco che il modello è un buon fit, quasi il 91% della variability è explained dal modello 
### F-statistic ha un p-value bassissimo quindi ottimo 

# un semplice scatterplot delle observed e predicted value scon la regression line 

library(ggplot2)

# Creo  dataframe con predictions
cleaned_data$Predicted <- predict(model)

# Scatter plot of observed vs predicted values
ggplot(cleaned_data, aes(x = Predicted, y = Daily_Usage_Time..minutes.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Observed vs Predicted Values",
       x = "Predicted Daily Usage Time (minutes)",
       y = "Observed Daily Usage Time (minutes)") +
  theme_minimal()

##### commento: non è molto buono 

library(ggplot2)

# Genero predictions from regression model
predicted_usage_time <- predict(model)

# (faccio prove): MSE 
mse <- mean((predicted_usage_time - cleaned_data$Daily_Usage_Time..minutes.)^2)
print(paste("Mean Squared Error:", round(mse, 2)))

# R-squared
rss <- sum((predicted_usage_time - mean(cleaned_data$Daily_Usage_Time..minutes.))^2) # Residual Sum of Squares
tss <- sum((cleaned_data$Daily_Usage_Time..minutes. - mean(cleaned_data$Daily_Usage_Time..minutes.))^2) # Total Sum of Squares
r_squared <- 1 - (rss / tss)
print(paste("R-squared:", round(r_squared, 4)))

# Plot Predicted vs. Actual Values
ggplot(cleaned_data, aes(x = cleaned_data$Daily_Usage_Time..minutes., y = predicted_usage_time)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line for perfect predictions
  labs(title = "Predicted vs Actual Daily Usage Time",
       x = "Actual Daily Usage Time (minutes)",
       y = "Predicted Daily Usage Time (minutes)") +
  theme_minimal()


# provo un regression model diverso, in questo caso uso il PCA1 + PCA2(avevano un livello di var molto alto) per predirre l'emoizone 
# attenzione, l'emozione è categorico, uso multinomial regression (dubito funzioni bene, forse è proprio la regression a non andare bene)

# Converto Dominant_Emotion to factor
cleaned_data$Dominant_Emotion <- as.factor(cleaned_data$Dominant_Emotion)
# Load necessary library
library(nnet)

# Fit a multinomial logistic regression model using PCA1 and PCA2 as predictors
model_multinom <- multinom(Dominant_Emotion ~ PCA1 + PCA2, data = cleaned_data)

# Summary of the model
summary(model_multinom)

### INTERPRETAZIONE ################### un po' difficile 
## iter 20 - dopo 20 iterazione converge 
## il PCA1 ha effetti strani non credo di poter trarre una conclusione sensata
## il PCA2 sembra avere sempre un'influenza negativa per tutti gli stati d'animo (se il 2 cresce allora la probabilità dell'emozione si abbassa)
## però il PCA2 non era buono, ciò compromette l'intero modello 


# Make predictions
predicted_emotions <- predict(model_multinom, newdata = cleaned_data)

# confusion matrix per vedere performance del modello
table(Predicted = predicted_emotions, Actual = cleaned_data$Dominant_Emotion)

# come volevasi dimostrare non funziona bene, perchè non riesce a predirre sadness ( e nemmeno ansia)

# Calculate accuracy
accuracy <- mean(predicted_emotions == cleaned_data$Dominant_Emotion)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

## faccio accuracy e infatti è bassa 47.62% 


 ####################################### RANDOM FOREST #####################################

library(randomForest)
library(caret)
library(reshape2)

cleaned_data$Platform <- as.factor(cleaned_data$Platform)

set.seed(123)
train_indices <-sample(1:nrow(cleaned_data), size = 0.7*nrow(cleaned_data))
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

# rf usando tutte le features di uso social 
#rf_model <- randomForest(Dominant_Emotion ~ Posts_Per_Day + Likes_Received_Per_Day +
#                           Comments_Received_Per_Day + Messages_Sent_Per_Day +
#                           Daily_Usage_Time..minutes., data = train_data)

rf_model <- randomForest(Platform ~ Posts_Per_Day + Likes_Received_Per_Day + Comments_Received_Per_Day + Messages_Sent_Per_Day + Daily_Usage_Time..minutes., data = train_data)
                           

predicted_rf <- predict(rf_model, newdata = test_data)

# Accuracy
accuracy_rf <- mean(predicted_rf == test_data$Platform)
print(paste("Random Forest Model Accuracy:", round(accuracy_rf * 100, 2), "%"))

confusion_matrix_rf <- table(Predicted = predicted_rf, Actual = test_data$Platform)

# Print the confusion matrix
print(confusion_matrix_rf) 

confusion_results <- confusionMatrix(confusion_matrix_rf)
print(confusion_results)

# grafico
confusion_melted <- melt(confusion_matrix_rf)
ggplot(confusion_melted, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix", x = "Actual Platform", y = "Predicted Platform") +
  theme_minimal()


# Vediamo la variable importance 
importance_rf <- importance(rf_model)

# Convert to a data frame for plotting
importance_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[, 1])

# Plot variable importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Variable Importance in Random Forest Model", x = "Features", y = "Importance") +
  theme_minimal()

########################################### RANDOM FOREST WITH PCA ####################################################

library(randomForest)
library(caret)
library(reshape2)

set.seed(123)

train_indices_2 <-sample(1:nrow(cleaned_data), size = 0.7*nrow(cleaned_data))
train_data_2 <- cleaned_data[train_indices_2, ]
test_data_2 <- cleaned_data[-train_indices_2, ]

# Random Forest model using original features and Daily Usage Time
rf_model_2 <- randomForest(Dominant_Emotion ~ PCA1 + PCA2 + Age, data = train_data_2)

# Evaluate the RF model 
predicted_rf_2 <- predict(rf_model_2, newdata = test_data_2)

# Accuracy
accuracy_rf_2 <- mean(predicted_rf_2 == test_data_2$Dominant_Emotion)
print(paste("Random Forest Model Accuracy:", round(accuracy_rf_2 * 100, 2), "%"))

confusion_matrix_rf_2 <- table(Predicted = predicted_rf_2, Actual = test_data_2$Dominant_Emotion)

# Print the confusion matrix
print(confusion_matrix_rf_2) 
confusion_results_2 <- confusionMatrix(confusion_matrix_rf_2)
print(confusion_results_2)


library(reshape2)
confusion_melted_2 <- melt(confusion_matrix_rf_2)
ggplot(confusion_melted_2, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix_2", x = "Actual Emotion", y = "Predicted Emotion") +
  theme_minimal()


# variable importance 
importance_rf_2 <- importance(rf_model_2)

# Convert to a df
importance_df_2 <- data.frame(Feature = rownames(importance_rf_2), Importance = importance_rf_2[, 1])

# Plot variable importance
ggplot(importance_df_2, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Variable Importance in Random Forest Model", x = "Features", y = "Importance") +
  theme_minimal()


############################################## CLUSTERS ############################################
library(dplyr)
library(factoextra)

# Preparare dati 
# Min-Max Normalization Function
min_max <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

cleaned_data$Age <- min_max(cleaned_data$Age)
cleaned_data$Daily_Usage_Time..minutes. <- min_max(cleaned_data$Daily_Usage_Time..minutes.)

# One-hot encode Gender
cleaned_data <- cleaned_data %>%
  mutate(Gender = as.factor(Gender)) %>%
  bind_cols(model.matrix(~ Gender - 1, data = cleaned_data)) %>%
  select(-Gender)  # Rimuove genere dalla column 

# Seleziona features per clustering 
cluster_data <- cleaned_data %>%
  select(Age, Daily_Usage_Time..minutes., starts_with("Gender"))

# numero di clusters 
num_clusters <- 5 

# k-means clustering
kmeans_result <- kmeans(cluster_data, centers = num_clusters)

# clustering results
fviz_cluster(kmeans_result, data = cluster_data, geom = "point", palette = "jco", 
             ggtheme = theme_minimal(), main = "K-Means Clustering (Age, Gender & Daily Usage Time)")

# cluster assignments to the original data
cleaned_data$Cluster <- as.factor(kmeans_result$cluster)

# riassunto delle caratteristiche 
cluster_summary <- cleaned_data %>%
  group_by(Cluster) %>%
  summarise(across(c(Age, Daily_Usage_Time..minutes., Posts_Per_Day, 
                     Likes_Received_Per_Day, Comments_Received_Per_Day, Messages_Sent_Per_Day), mean))

print(cluster_summary)




######################################## DECISION TREES ###################################################
# Load necessary libraries
library(tree)

# Load and clean the data
data <- read.csv("dataset_socialmedia.csv")
cleaned_data <- na.omit(data)

# Convert relevant columns to appropriate types
cleaned_data$Daily_Usage_Time..minutes.<- as.numeric(cleaned_data$Daily_Usage_Time..minutes.)
cleaned_data$Dominant_Emotion <- as.factor(cleaned_data$Dominant_Emotion)

# Select only relevant columns for the decision tree
tree_data <- cleaned_data[, c("Daily_Usage_Time..minutes.", "Dominant_Emotion")]

# Split the data into training (70%) and testing (30%) sets
set.seed(101)
train_indices <- sample(1:nrow(tree_data), 0.7 * nrow(tree_data))
train <- tree_data[train_indices, ]
test <- tree_data[-train_indices, ]

# Build the decision tree model
tree_emotion <- tree(Dominant_Emotion ~ Daily_Usage_Time..minutes., data = train)

# Display summary and plot the decision tree
summary(tree_emotion)
plot(tree_emotion)
text(tree_emotion, pretty = 0, cex = 0.8)

# Make predictions on the test set
predictions <- predict(tree_emotion, newdata = test, type = "class")

# Calculate accuracy of the predictions
accuracy <- sum(predictions == test$Dominant_Emotion) / nrow(test)
print(paste("Accuracy:", accuracy))

# Display confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test$Dominant_Emotion)
print(confusion_matrix)
