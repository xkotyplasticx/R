# Load required libraries
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)

# Load and view the data
my_data <- read.csv("D:\\Курсы\\DataSceintist\\7. R\\Kunskapskontroll\\carclean.csv", header = TRUE, sep = ",")
print(my_data)
View(my_data)

# Data preprocessing
my_data <- my_data[, -1]  # Remove the first column
my_data$Färg <- as.factor(my_data$Färg)
my_data$Biltyp <- as.factor(my_data$Biltyp)
my_data$Märke <- as.factor(my_data$Märke)
my_data$Modell <- as.factor(my_data$Modell)
my_data$Län <- NULL

# Display structure and summary of data
dim(my_data)
head(my_data)
str(my_data)
summary(my_data)
View(my_data)

# Splitting data into training and test sets
set.seed(29)
spec = c(train = 0.8, test = 0.2)
g = sample(cut(seq(nrow(my_data)), nrow(my_data) * cumsum(c(0, spec)), labels = names(spec)))
res = split(my_data, g)
my_data_train <- res$train
my_data_test <- res$test

str(my_data_train)
str(my_data_test)
str(my_data)

# Creating and evaluating models

# Model 1: Based on theoretical knowledge and logic
lm_1 <- lm(Pris ~ Miltal + Modellår + Biltyp + Drivning + Hästkrafter + Märke, data=my_data_train)
summary(lm_1)
par(mfrow = c(2, 2))
plot(lm_1)

# Check for multicollinearity
vif(lm_1)  

# Model 2: Starting from full model and choosing significant variables where we manually removed not significant ones

lm_2 <- lm(Pris ~ . + Modellår:Miltal - Biltyp - Färg - Märke - Modell - Drivning, data=my_data_train)
summary(lm_2)
par(mfrow = c(2, 2))
plot(lm_2)
vif(lm_2)

# Model 3: Backward stepwise selection from full model

full.model <- lm(Pris ~ ., data=my_data_train)  
backward.stepwise <- stepAIC(full.model, direction="backward", trace=FALSE)
lm_3 <- lm(formula(backward.stepwise), data=my_data_train)

# Summary and diagnostics for lm_3

summary(lm_3)
par(mfrow = c(2, 2))
plot(lm_3)
vif(lm_3)

# Evaluation of models

results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  BIC = c(BIC(lm_1), BIC(lm_2), BIC(backward.stepwise))
)
print(results)

#Comparison of Models

barplot(results$BIC, names.arg = results$Model, col = "skyblue", 
        main = "BIC Comparison of Models", ylab = "BIC Value",
        ylim = c(min(results$BIC) - 10, max(results$BIC) + 10), 
        yaxp = c(min(results$BIC), max(results$BIC), 5))

# Additional diagnostics for Model 3

par(mfrow = c(2, 2))
plot(lm_3)  
summary(lm_3)  


# Evaluating our chosen model (lm_3) on the test data.
test_pred_m3 <- predict(lm_3, newdata = my_data_test)
rmse(my_data_test$Pris, test_pred_m3)


# Inference for model 3 ---------------------------------------------------

summary(lm_3)  # Hypothesis testing
confint(lm_3)

# Create CI & PI for predictions
confidence_intervals <- predict(lm_3, newdata = my_data_test, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_3, newdata = my_data_test, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals

# Predictions for testdata
predictions <- predict(lm_3, newdata = my_data_test)

# A scatterplot for comparison of the predicted and real data
plot(x = my_data_test$Pris, y = predictions,
     xlab = "Real values", ylab = "Predicted values",
     main = "Comparison of real and predicted values",
     col = "skyblue", pch = 20)

# A line for an ideal predictions
abline(0, 1, col = "red")


