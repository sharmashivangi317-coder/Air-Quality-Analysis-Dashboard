
# LINEAR REGRESSION MODEL — AIR QUALITY PREDICTION


# STEP 1: Install & Load Required Libraries
packages <- c("caret", "Metrics", "ggplot2", "dplyr")
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# STEP 2
data <- datasets::airquality

# STEP 3: Data Cleaning
data <- na.omit(data)   # remove missing values
colnames(data) <- c("Ozone", "Solar_Radiation", "Wind", "Temperature", "Month", "Day")

# Convert month & day to factors
data$Month <- as.factor(data$Month)
data$Day   <- as.factor(data$Day)

# Derived feature: Temperature in Celsius
data$Temp_C <- (data$Temperature - 32) * (5/9)

# STEP 4: Split Dataset (Train/Test)
set.seed(123)
splitIndex <- createDataPartition(data$Ozone, p = 0.8, list = FALSE)
train <- data[splitIndex, ]
test  <- data[-splitIndex, ]

# STEP 5: Train Linear Regression Model
lm_model <- lm(Ozone ~ Solar_Radiation + Wind + Temperature + Month + Day + Temp_C,
               data = train)

cat("\n✅ Linear Regression Model Summary:\n")
print(summary(lm_model))

# STEP 6: Make Predictions
pred_lm <- predict(lm_model, newdata = test)

# STEP 7: Evaluate Model Performance
mse_val  <- mse(test$Ozone, pred_lm)
rmse_val <- rmse(test$Ozone, pred_lm)
r2_val   <- cor(test$Ozone, pred_lm)^2

cat("\n📊 MODEL PERFORMANCE SUMMARY:\n")
cat("MSE :", round(mse_val, 3), "\n")
cat("RMSE:", round(rmse_val, 3), "\n")
cat("R²  :", round(r2_val, 3), "\n")

# STEP 8: Visualization – Actual vs Predicted
ggplot(data.frame(Actual = test$Ozone, Predicted = pred_lm),
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Linear Regression: Actual vs Predicted Ozone Levels",
       x = "Actual Ozone", y = "Predicted Ozone") +
  theme_minimal()

# STEP 9: Export Predictions for Power BI
test$Predicted_Ozone <- pred_lm
write.csv(test, "AirQuality_Predictions_Linear.csv", row.names = FALSE)


