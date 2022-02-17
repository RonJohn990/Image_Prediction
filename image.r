# rm(list = ls())
# setwd("~/Desktop/dev/r_scripts_2/Image_Tutorial")
library(glue)
library(tidyverse)

# Reading Data------------------
data <- read.csv("~/Desktop/dev/r_scripts_2/Image_Tutorial/data/fer2018.csv",col.names = c("emotion", "px")) 

# emotion_label = list('Anger' = 0,
                        # 'Disgust' = 1,
                        # 'Fear' = 2,
                        # 'Happy' = 3,
                        # 'Sad' = 4,
                        # 'Surprise' = 6,
                        # 'Neutral' = 6)

label = c('Anger', 'Disgust', 'Fear', 'Happy', 'Sad', 'Surprise', 'Neutral')

emotion_label  <- set_names(c(0, 1, 2, 3, 4, 5, 6), label)

# Pre-processing data------------------------------------------------------------
tmp_pixels <- data$px
r = 1
tmp <- matrix(0, nrow = 35886, ncol = 2304)
col_name <- c()
while (r <= nrow(data)){
  tmp[r, ] <- lapply(tmp_pixels[r], strsplit, split = " ") %>%
                unlist() %>%
                rev() %>%
                as.numeric()
  r = r + 1
}

i = 0
for (i in 1:dim(tmp)[2]){
  col_name <- append(col_name, glue("px_{i}"))
  i = i + 1
}

df <- cbind(data$emotion, tmp)
colnames(df) <- c("emotion", col_name)

image_view <- matrix(df[3500, 2:2305], 48, 48)
image(image_view, col = gray.colors(20), asp = 1)

df <- as.data.frame(df)
df$emotion <- as.factor(df$emotion)


# EDA------------------------------------------------------------------------
df %>%
  group_by(emotion) %>%
  count() 

df %>%
  group_by(emotion) %>%
  count() %>%
  mutate(emotion = names(emotion_label[emotion_label == emotion])) %>%
  ggplot(aes(x = emotion)) +
    geom_col(aes(y = n, fill = as.factor(emotion)))


# Train, TEst split-----------------------------------------------------------
set.seed(123)
split <- sample(1:nrow(df), as.integer(0.8*nrow(df)), FALSE)
train <- df[split, ]
test <- df[-split, ]


# Going for a smaller sample size, with equal proportions for all emotions-----------------------------------
df_small <- data.frame()

for(i in emotion_label){
  tmp <- df %>% 
    filter(emotion == i)
  
  split_small <- sample(1:nrow(tmp), 500, FALSE)
  tmp <- tmp[split_small,]
  
  df_small <- rbind(df_small, tmp)
}

df_small %>%
  group_by(emotion) %>%
  count() 

split <- sample(1:nrow(df_small), as.integer(0.8*nrow(df_small)), FALSE)
train <- df_small[split, ]
test <- df_small[-split, ]


# KNN ------------------------------------------------------------------------
# install.packages('class')
library(class)
knn_model <- knn(train = train[, 2:dim(train)[2]],
                 test = test[, 2:dim(train)[2]],
                 cl = train$emotion, k = 3,
                 prob = T)

cm <- table(test$emotion, knn_model)

acc <- sum(diag(cm))/dim(test)[1] # 0.3449429

knn_model <- knn(train = train[, 2:dim(train)[2]],
                 test = test[, 2:dim(train)[2]],
                 cl = train$emotion, k = 2,
                 prob = T)

cm <- table(test$emotion, knn_model)

acc <- sum(diag(cm))/dim(test)[1] # 0.2228571. Absolutely terrible.


# Naive Bayes-----------------------------------------------------
library("e1071")
nb_model <- naiveBayes(data = df, emotion ~ .)

pred <- predict(nb_model, test[, 2:dim(test)[2]])

cm <- table(test$emotion, pred)

acc <- sum(diag(cm))/dim(test)[1] # 0.2285714 Absolutely terrible.


# Decision Trees --------------------------------------------
library(rpart)
dt_model <- rpart(formula = emotion ~ ., data = train)

pred <- predict(dt_model, newdata = test[, 2:dim(test)[2]], type = "class")

cm <- table(test[, 1], pred)

acc <- sum(diag(cm))/dim(test)[1] # 0.2456116 Absolutely terrible.


# Random FOrest -----------------------------------------------------------
library(randomForest)
rf_model <- randomForest(x = train[, 2:dim(train)[2]],
                         y = train$emotion,
                         data = train,
                         ntree = 10)

pred <- predict(rf_model, newdata = test)

cm <- table(test[, 1], pred)

acc <- sum(diag(cm))/dim(test)[1] # 0.3899415 Absolutely terrible.


# SVM----------------------------------------------------------------------
library(e1071)
svm_model <- svm(x = train[, 2:dim(train)[2]], y = train$emotion)
summary(svm_model)

pred <- predict(svm_model, newdata = test[, 2:dim(test)[2]])

cm <- table(test[, 1], pred)

acc <- sum(diag(cm))/dim(test)[1] # 0.3657143 Absolutely terrible. But better!
                                  # 0.4536082 Witht the whole dataset. Cautiously optimistic!!


# K - Means Clustering------------
X <- df[, 2:dim(df)[2]]
# Utilising the Elbow method
set.seed(123)
wcss <- vector()
for (i in 1:10){
  wcss[i] <- sum(kmeans(X, i)$withinss)  
}
plot(x = 1:10, y = wcss, type = "l")
points(x = 1:10, y = wcss, col = "darkred", pch = 9)


kmeans_model <- kmeans(X, 7, iter.max = 300, nstart = 10)
