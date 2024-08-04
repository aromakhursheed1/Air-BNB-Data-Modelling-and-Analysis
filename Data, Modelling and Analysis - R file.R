data<- read.csv("FINAL.csv")
str(data)
data$Price <- gsub("[$,]", "", data$Price) # Removing $ and ,
data$Price <- as.numeric(data$Price) # Convert to numeric
# Remove $ and % signs and convert to numeric
data$Income <- as.numeric(gsub("[$,]", "", as.character(data$Income)))
data$Crime.Rate..per.1000.residents. <- as.numeric(gsub("%", "", as.character(data$Crime.Rate..per.1000.residents.)))
data$Population..by.race. <- as.numeric(gsub("%", "", as.character(data$Population..by.race.)))
data$Property.Price <- as.numeric(gsub("[$,]", "", as.character(data$Property.Price)))
data$Educational.Level <- as.numeric(gsub("%", "", as.character(data$Educational.Level)))
data$Unemployment.Rate <- as.numeric(gsub("%", "", as.character(data$Unemployment.Rate)))
data$Property.Price <- as.numeric(gsub("[$,]", "", as.character(data$Property.Price)))
data <- data[!is.na(data$Host_ID), ]
data <- data[, !names(data) %in% "Host_ID"]
data <- data[, -c((ncol(data) - 9):ncol(data))]

# Assuming your data frame is named 'data'
income_column <- data$Income

# Calculate the 10th and 90th percentile
p10 <- quantile(income_column, 0.1)
p90 <- quantile(income_column, 0.90)

# Calculate the average of the "Income" column
average_income <- mean(income_column, na.rm = TRUE)

# Replace values below the 10th percentile and above the 90th percentile with the average income
income_column[income_column < p10] <- average_income
income_column[income_column > p90] <- average_income

# Update the column in the original data frame
data$Income <- income_column
# Assuming 'data' is your dataframe
income_data <- data$Income

# Create a histogram for the 'Income' column
hist(income_data, breaks = 20, col = "skyblue", xlab = "Income", ylab = "Frequency", main = "Distribution of Income")


# Assuming 'data' is your dataframe
crime_rate_column <- data$Crime.Rate..per.1000.residents.

# Replace values 40.4 and 83.5 with 18.79
crime_rate_column[crime_rate_column == 40.4 | crime_rate_column == 83.5] <- 18.79

# Replace all crime rates less than 8.4 with 18.79
crime_rate_column[crime_rate_column < 8.4] <- 18.79

# Update the column in the original data frame
data$Crime.Rate..per.1000.residents. <- crime_rate_column

# Create a histogram for the modified 'Crime.Rate..per.1000.residents.' column
hist(data$Crime.Rate..per.1000.residents., breaks = 20, col = "skyblue", 
     xlab = "Crime Rate per 1000 Residents", ylab = "Frequency", 
     main = "Distribution of Crime Rate (Filtered)")

# Assuming 'data' is your dataframe
property_price_column <- data$Property.Price

# Calculate the average excluding NA values
average_property_price <- mean(property_price_column, na.rm = TRUE)

# Replace NA values with the calculated average
property_price_column[is.na(property_price_column)] <- average_property_price

# Update the column in the original data frame
data$Property.Price <- property_price_column

# Assuming 'data' is your dataframe
property_price_column <- data$Property.Price

# Create a histogram for the modified 'Property.Price' column
hist(property_price_column, breaks = 20, col = "skyblue", 
     xlab = "Property Price", ylab = "Frequency", 
     main = "Distribution of Property Prices")

# Assuming 'data' is your dataframe
unemployment_rate_column <- data$Unemployment.Rate

# Calculate the 10th and 90th percentile
p10 <- quantile(unemployment_rate_column, 0.1)
p90 <- quantile(unemployment_rate_column, 0.9)

# Calculate the average of the column
average_unemployment_rate <- mean(unemployment_rate_column, na.rm = TRUE)

# Replace values below the 5th percentile and above the 95th percentile with the average
unemployment_rate_column[unemployment_rate_column < p10] <- average_unemployment_rate
unemployment_rate_column[unemployment_rate_column > p90] <- average_unemployment_rate

# Update the column in the original data frame
data$Unemployment.Rate <- unemployment_rate_column

# Assuming 'data' is your dataframe
unemployment_rate_column <- data$Unemployment.Rate

# Create a histogram for the modified 'Unemployment.Rate' column
hist(unemployment_rate_column, breaks = 20, col = "skyblue",
     xlab = "Unemployment Rate", ylab = "Frequency",
     main = "Distribution of Unemployment Rate")
summary(data)

# Linear Regression Model 
model <- lm(Price ~ ., data = data)
print(model)
summary(model)

# Get the R-squared value
r_squared <- summary(model)$r.squared
# Print the R-squared value
print(r_squared)

# MAPE 
predicted_values <- predict(model, data)
actual_values <- data$Price
mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
print(mape)

# Step Wise Regression
stepwise_model <- step(model, direction = "both")
summary(stepwise_model)
r_squared_step <- summary(stepwise_model)$r.squared

# Step Wise Mape 
predicted_value_step <- predict(stepwise_model, data = data)
mape_step <- mean(abs(data$Price - predicted_value_step) / data$Price) * 100
print(mape_step)

# Save Data as csv 
write.csv(data, "AAMD PROJECT 1 FINAL GROUP 11.csv", row.names = FALSE)

## CROSS VALIDATION USING K-FOLDS 
install.packages("tidyverse")
library(tidyverse)

install.packages("caret")
library(caret)

num_folds <- 5

# Define the control parameters for k-fold cross-validation
train_control <- trainControl(method = "cv", number = num_folds)

# Create a model using k-fold cross-validation (e.g., linear regression as an example)
model1 <- train(Price ~ ., data = data, method = "lm", trControl = train_control)

# Get cross-validation results
cv_results <- model1$resample
print(cv_results)
