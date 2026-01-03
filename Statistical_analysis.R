
#  AIRQUALITY DATA - STATISTICAL ANALYSIS PROJECT
 
# STEP 1 — Load Required Libraries


# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("rpart")
# install.packages("rpart.plot")

library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(rpart)
library(rpart.plot)

# STEP 2 — Load & Clean Data
data <- airquality

# Replace missing values with mean
data$Ozone[is.na(data$Ozone)] <- mean(data$Ozone, na.rm = TRUE)
data$Solar.R[is.na(data$Solar.R)] <- mean(data$Solar.R, na.rm = TRUE)

# Convert Month, Day to factors
data$Month <- as.factor(data$Month)
data$Day <- as.factor(data$Day)

# Select numeric variables
numeric_data <- data[, c("Ozone", "Solar.R", "Wind", "Temp")]


# PART A — CORRELATION & ADVANCED REGRESSION


cat("\n🔹 CORRELATION ANALYSIS\n")

cor_matrix <- cor(numeric_data)
print(cor_matrix)

# Visualize correlation heatmap
heatmap(cor_matrix, Rowv=NA, Colv=NA, col=heat.colors(256), main="Correlation Heatmap")

# Regression Model (Ozone as dependent variable)
cat("\n🔹 MULTIPLE LINEAR REGRESSION\n")
model <- lm(Ozone ~ Temp + Wind + Solar.R, data=data)
summary(model)

# Regression Diagnostics
par(mfrow = c(2,2))
plot(model)


# PART B — HYPOTHESIS TESTING

cat("\n🔹 HYPOTHESIS TESTING\n")

# Example 1: t-test — mean temperature difference across months (June vs August)
group_june <- data$Temp[data$Month == 6]
group_aug  <- data$Temp[data$Month == 8]

t_result <- t.test(group_june, group_aug)
print(t_result)

# Example 2: Correlation test — Ozone vs Temperature
cor_test <- cor.test(data$Ozone, data$Temp)
print(cor_test)

# Example 3: ANOVA — does temperature differ by month?
anova_model <- aov(Temp ~ Month, data=data)
summary(anova_model)


# PART C — CLASSIFICATION (Decision Tree)


cat("\n🔹 CLASSIFICATION MODEL\n")

# Create a categorical variable for Ozone levels (Low/High)
data$OzoneLevel <- ifelse(data$Ozone > mean(data$Ozone), "High", "Low")
data$OzoneLevel <- as.factor(data$OzoneLevel)

# Decision Tree Model
tree_model <- rpart(OzoneLevel ~ Temp + Wind + Solar.R, data=data, method="class")
rpart.plot(tree_model, main="Decision Tree for Ozone Level Classification", extra=102)

# Prediction Example
pred <- predict(tree_model, type="class")
table(Predicted = pred, Actual = data$OzoneLevel)


# PART D — CLUSTERING (K-Means)


cat("\n🔹 CLUSTERING ANALYSIS\n")

# Scale numeric data
scaled_data <- scale(numeric_data)

# Determine optimal clusters using Elbow method
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Apply k-means with 3 clusters
set.seed(123)
kmodel <- kmeans(scaled_data, centers=3, nstart=20)

# Visualize clusters
fviz_cluster(kmodel, data = scaled_data, geom = "point", ellipse.type = "convex", main="K-Means Clustering (3 Groups)")

# View cluster summary
print(kmodel$centers)

# Add cluster labels to dataset
data$Cluster <- as.factor(kmodel$cluster)


