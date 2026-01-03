# PROJECT: AirQuality Data Cleaning in R

# STEP 1: Load Dataset
data <- airquality
head(data)
str(data)
summary(data)

# STEP 2:  Check NA count
colSums(is.na(data))

# Replace NA  with column mean
data$Ozone[is.na(data$Ozone)] <- mean(data$Ozone, na.rm = TRUE)
data$Solar.R[is.na(data$Solar.R)] <- mean(data$Solar.R, na.rm = TRUE)

# Confirm no missing left
colSums(is.na(data))

# STEP 3: Out lier Detection and Treatment
# Function to cap out lier's 
cap <- function(x){
  if(is.numeric(x)){
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    H <- 1.5 * IQR(x, na.rm = TRUE)
    lower <- qnt[1] - H
    upper <- qnt[2] + H
    x[x < lower] <- lower
    x[x > upper] <- upper
  }
  return(x)
}

# Apply capping to numeric columns
data$Ozone <- cap(data$Ozone)
data$Solar.R <- cap(data$Solar.R)
data$Wind <- cap(data$Wind)
data$Temp <- cap(data$Temp)

# Check summaries again
summary(data)

# Box plots for visual check
par(mfrow = c(2,2))
boxplot(data$Ozone, main = "Ozone", col = "skyblue")
boxplot(data$Solar.R, main = "Solar Radiation", col = "lightgreen")
boxplot(data$Wind, main = "Wind", col = "lightpink")
boxplot(data$Temp, main = "Temperature", col = "lightyellow")

# STEP 4: Data Type Correction
data$Month <- as.factor(data$Month)
data$Day <- as.factor(data$Day)

# STEP 5: Derived Fields 
# Ensure Temp is numeric before conversion
data$Temp <- as.numeric(data$Temp)

# Convert Temp to Celsius
data$Temp_C <- round((data$Temp - 32) * (5/9), 1)

# Create a Comfort Index (example metric)
data$ComfortIndex <- round(data$Temp - data$Wind, 1)

# STEP 6: Final Check
summary(data)
str(data)

# Confirm structure and few rows
head(data[, c("Ozone", "Solar.R", "Wind", "Temp", "Temp_C", "ComfortIndex")])


