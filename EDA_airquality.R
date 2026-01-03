#  AIRQUALITY DATA ANALYSIS PROJECT (R + Power BI)


# STEP 1 — Load Required Libraries

# install.packages("ggplot2")
# install.packages("dplyr")

library(ggplot2)
library(dplyr)

# STEP 2 — Load Dataset
data <- airquality

# STEP 3 — DATA CLEANING


# Treat Missing Values (replace NA with column mean)
data$Ozone[is.na(data$Ozone)] <- mean(data$Ozone, na.rm = TRUE)
data$Solar.R[is.na(data$Solar.R)] <- mean(data$Solar.R, na.rm = TRUE)

# Convert Month and Day to factor
data$Month <- as.factor(data$Month)
data$Day <- as.factor(data$Day)

# Check structure
str(data)

# Outlier Detection (using boxplot stats)
outliers_ozone <- boxplot.stats(data$Ozone)$out
outliers_temp  <- boxplot.stats(data$Temp)$out
print("Outliers in Ozone:")
print(outliers_ozone)
print("Outliers in Temperature:")
print(outliers_temp)

# Optional: Capping Outliers (for smoother plots)
cap <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

data$Ozone <- cap(data$Ozone)
data$Temp  <- cap(data$Temp)

# STEP 4 — SUMMARY STATISTICS

print("Summary Statistics:")
print(summary(data))

# STEP 5 — UNIVARIATE ANALYSIS


# Histograms
par(mfrow = c(2,2))
hist(data$Ozone, main="Ozone Distribution", col="skyblue", xlab="Ozone")
hist(data$Solar.R, main="Solar Radiation Distribution", col="lightgreen", xlab="Solar Radiation")
hist(data$Wind, main="Wind Speed Distribution", col="lightpink", xlab="Wind Speed")
hist(data$Temp, main="Temperature Distribution", col="lightyellow", xlab="Temperature (F)")

# Boxplots
par(mfrow = c(2,2))
boxplot(data$Ozone, main="Ozone Levels", col="skyblue")
boxplot(data$Solar.R, main="Solar Radiation", col="lightgreen")
boxplot(data$Wind, main="Wind Speed", col="lightpink")
boxplot(data$Temp, main="Temperature", col="lightyellow")

# STEP 6 — BIVARIATE ANALYSIS


# Scatter plots
par(mfrow = c(2,2))
plot(data$Temp, data$Ozone, col="blue", pch=19, main="Ozone vs Temperature", xlab="Temperature", ylab="Ozone")
plot(data$Wind, data$Ozone, col="darkgreen", pch=19, main="Ozone vs Wind", xlab="Wind", ylab="Ozone")
plot(data$Solar.R, data$Ozone, col="red", pch=19, main="Ozone vs Solar Radiation", xlab="Solar Radiation", ylab="Ozone")
plot(data$Month, data$Ozone, col="orange", main="Ozone Levels by Month", xlab="Month", ylab="Ozone")

# Correlation Matrix
numeric_cols <- data[, c("Ozone", "Solar.R", "Wind", "Temp")]
cor_matrix <- cor(numeric_cols)
print("Correlation Matrix:")
print(cor_matrix)

# Heatmap
heatmap(cor_matrix, Rowv=NA, Colv=NA, col=heat.colors(256), main="Correlation Heatmap")

# Pair Plot
pairs(numeric_cols, main="Scatterplot Matrix for Air Quality Variables", col="purple")

# STEP 7 — REGRESSION ANALYSIS


# Linear Regression Model
model <- lm(Ozone ~ Temp + Wind + Solar.R, data=data)
summary(model)

# Diagnostic Plots
par(mfrow = c(2,2))
plot(model)

