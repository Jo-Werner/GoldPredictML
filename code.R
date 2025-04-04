##########################################################
# Setup
##########################################################

# knitr global chunk options
# Sets default figure dimensions, alignment, and positioning for knitr output
knitr::opts_chunk$set(
  fig.width = 4.5,
  fig.height = 3,
  fig.align = 'center',
  fig.pos = "H"
)

# Custom kable function
# Defines a reusable function to style tables with scaling and positioning
if(!require(kableExtra)) install.packages("kableExtra")
library(kableExtra)

my_kable <- function(x, caption = NULL) {
  knitr::kable(x, 
               format = "latex",
               booktabs = TRUE,
               caption = caption,
               linesep = "") %>%
    kable_styling(
      latex_options = c("scale_down", "hold_position"),
      font_size = 9,
      position = "center"
    )
}

# Load and install ggplot2 library if needed
if(!require(ggplot2)) install.packages("ggplot2")
# Imports ggplot2 for data visualization
library(ggplot2)

# Set custom ggplot2 theme
# Applies a minimal theme with adjusted font sizes and margins
theme_set(
  theme_minimal() +
    theme(
      plot.title = element_text(size = rel(0.9)),
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      legend.title = element_text(size = rel(0.8)),
      plot.margin = unit(c(2,2,2,2), "mm"),
      plot.caption = element_text(size = rel(0.6), hjust = 1)
    )
)


##########################################################
# Data preparation
##########################################################

# Install and load quantmod package if not already installed
if(!require(quantmod)) install.packages("quantmod")
library(quantmod)

# Fetch gold prices (GC=F) from Yahoo Finance starting from 1974-01-01
getSymbols("GC=F", src = "yahoo", from = "1974-01-01", to = "2025-03-13")
gold_prices <- `GC=F`
# Remove the original symbol object to clean up workspace
rm(`GC=F`)

# Fetch S&P 500 data (^GSPC) from Yahoo Finance starting from 2000-01-01
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2025-03-13")
sp500_data <- `GSPC`
# Remove the original symbol object to clean up workspace
rm(`GSPC`)

# Load CPI data from FRED (Federal Reserve Economic Data)
getSymbols("CPIAUCSL", src = "FRED", from = "2000-01-01", to = "2025-03-13")
cpi_data <- CPIAUCSL
# Calculate year-over-year (YoY) inflation rate (12 months lag
inflation_rate_yoy <- 100 * (cpi_data / lag(cpi_data, 12) - 1)
# Clean up by removing temporary CPI objects
rm(cpi_data, CPIAUCSL)


# Install and load necessary libraries for data manipulation and plotting
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Convert gold prices to a data frame with Date and average GoldPrice
gold_prices_df <- data.frame(Date = index(gold_prices), GoldPrice = coredata(gold_prices))
gold_prices_df <- gold_prices_df %>%
  mutate(GoldPrice = (GoldPrice.GC.F.High + GoldPrice.GC.F.Low) / 2) %>%
  select(Date, GoldPrice)

# Convert S&P 500 data to a data frame with Date and average SP500Price
sp500_data_df <- data.frame(Date = index(sp500_data), SP500 = coredata(sp500_data))
sp500_data_df <- sp500_data_df %>%
  mutate(SP500Price = (SP500.GSPC.High + SP500.GSPC.Low) / 2) %>%
  select(Date, SP500Price)

# Convert inflation rate to a data frame and rename the inflation column
inflation_rate_yoy_df <- data.frame(Date = index(inflation_rate_yoy), InflationRate = coredata(inflation_rate_yoy))
inflation_rate_yoy_df <- inflation_rate_yoy_df %>%
  rename(InflationIndex = CPIAUCSL)

# Remove original time series objects to free up memory
rm(gold_prices,inflation_rate_yoy,sp500_data)

# Merge all data frames by Date using left joins
merged_data <- sp500_data_df %>%
  left_join(gold_prices_df, by = "Date") %>%
  left_join(inflation_rate_yoy_df, by = "Date")

# Install and load zoo package for handling NA interpolation
if(!require(zoo)) install.packages("zoo")
library(zoo)

# Linearly interpolate NA values in the time series
merged_data$GoldPrice <- na.approx(merged_data$GoldPrice, x = merged_data$Date, na.rm = FALSE)
merged_data$SP500Price <- na.approx(merged_data$SP500Price, x = merged_data$Date, na.rm = FALSE)
merged_data$InflationIndex <- na.approx(merged_data$InflationIndex, x = merged_data$Date, na.rm = FALSE)

# Fill remaining NAs at the start or end using last observation carried forward/backward
merged_data$InflationIndex <- na.locf(merged_data$InflationIndex, fromLast = FALSE, na.rm = FALSE)
merged_data$InflationIndex <- na.locf(merged_data$InflationIndex, fromLast = TRUE, na.rm = FALSE)
merged_data$GoldPrice <- na.locf(merged_data$GoldPrice, fromLast = FALSE, na.rm = FALSE)
merged_data$GoldPrice <- na.locf(merged_data$GoldPrice, fromLast = TRUE, na.rm = FALSE)


##########################################################
# Data Exploration
##########################################################

# Show summary of merged_data
summary(merged_data) %>%
  my_kable(caption = "Summary of data metrics")

# Display rows 900 to 905 of merged_data to show an example of the row structure
merged_data[900:905, ]%>%
  my_kable(caption = "Row structure example from line 900 to 905")

# Install and load gridExtra package for arranging multiple plots
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

# Arrange multiple ggplot charts in a single column layout
grid.arrange(
  # Plot gold prices over time with a line connecting the data points
  ggplot(merged_data, aes(x = Date)) +
    geom_line(aes(y = GoldPrice, color = "Gold")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    scale_color_manual(values = c("Gold" = "gold", "S&P 500" = "blue", "Inflation" = "red")) +
    labs(x = "Date", y = "Price [$]", color = "") +
    theme(aspect.ratio = 1/3, legend.title = element_blank()),
  
  # Plot S&P 500 prices over time with a line connecting the data points
  ggplot(merged_data, aes(x = Date)) +
    geom_line(aes(y = SP500Price, color = "S&P 500"), na.rm = TRUE) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    scale_color_manual(values = c("Gold" = "gold", "S&P 500" = "blue", "Inflation" = "red")) +
    labs(x = "Date", y = "Price [$]", color = "") +
    theme(aspect.ratio = 1/3, legend.title = element_blank()),
  
  # Plot inflation index over time with a line connecting the data points
  ggplot(merged_data, aes(x = Date)) +
    geom_line(aes(y = InflationIndex, color = "Inflation"), na.rm = TRUE) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    scale_color_manual(values = c("Gold" = "gold", "S&P 500" = "blue", "Inflation" = "red")) +
    labs(x = "Date", y = "Value [%]", color = "") +
    theme(aspect.ratio = 1/3, legend.title = element_blank()),
  
  # Specify single-column layout for the three plots
  ncol = 1
)


# Normalize data to the year 2000
# Normalize gold prices (first value = 100)
gold_prices_df$NormalizedGoldPrice <- (gold_prices_df$GoldPrice / gold_prices_df$GoldPrice[1]) * 100

# Normalize S&P 500 data (first value = 100)
sp500_data_df$NormalizedSP500 <- (sp500_data_df$SP500 / sp500_data_df$SP500[1]) * 100

# Load scales package for secondary axis functionality
if(!require(scales)) install.packages("scales")
library(scales)

# Define a function to filter and plot data starting from a specific date
normalized_plot <- function(start_date) {
  # Filter data based on the specified start date
  filtered_gold <- gold_prices_df[gold_prices_df$Date >= start_date, ]
  filtered_sp500 <- sp500_data_df[sp500_data_df$Date >= start_date, ]
  filtered_inflation <- inflation_rate_yoy_df[inflation_rate_yoy_df$Date >= start_date, ]
  
  # Create the plot
  ggplot() +
    # Plot normalized gold prices (left y-axis)
    geom_line(data = filtered_gold, aes(x = Date, y = NormalizedGoldPrice, color = "Gold Price"), size = 1) +
    
    # Plot normalized S&P 500 (left y-axis)
    geom_line(data = filtered_sp500, aes(x = Date, y = NormalizedSP500, color = "S&P 500"), size = 1) +
    
    # Plot inflation rate (right y-axis, scaled for visibility)
    geom_line(data = filtered_inflation, aes(x = Date, y = InflationIndex * 100, color = "Inflation Rate"), size = 0.5, linetype = "dashed") +
    
    # Define primary and secondary y-axes
    scale_y_continuous(
      name = "Normalized Value [%]",
      limits = c(-200, 1000),
      sec.axis = sec_axis(~ . / 100, name = "Inflation Rate [%]") # Rescale inflation rate for the secondary axis
    ) +
    
    # Set colors and legend order
    scale_color_manual(
      values = c("Gold Price" = "gold", "S&P 500" = "blue", "Inflation Rate" = "red"),
      breaks = c("Gold Price", "S&P 500", "Inflation Rate") # Specify legend order
    ) +
    
    # Add labels and customize theme
    labs(x = "Date",
         color = "Legend") +
    theme(legend.title = element_blank())
}

# Call the plot function with the start date of 2000-01-01
normalized_plot(as.Date("2000-01-01"))


# Normalize gold prices to 2010-01-04 (first value = 100)
gold_prices_df$NormalizedGoldPrice <- (gold_prices_df$GoldPrice / gold_prices_df$GoldPrice[gold_prices_df$Date == as.Date("2010-01-04")]) * 100

# Normalize S&P 500 to 2010-01-04 (first value = 100)
sp500_data_df$NormalizedSP500 <- (sp500_data_df$SP500 / sp500_data_df$SP500[gold_prices_df$Date == as.Date("2010-01-04")]) * 100

library(scales)

# Function to plot normalized data from a specific start date
normalized_plot <- function(start_date) {
  filtered_gold <- gold_prices_df[gold_prices_df$Date >= start_date, ]
  filtered_sp500 <- sp500_data_df[sp500_data_df$Date >= start_date, ]
  filtered_inflation <- inflation_rate_yoy_df[inflation_rate_yoy_df$Date >= start_date, ]
  
  ggplot() +
    geom_line(data = filtered_gold, aes(x = Date, y = NormalizedGoldPrice, color = "Gold Price"), size = 1) +
    geom_line(data = filtered_sp500, aes(x = Date, y = NormalizedSP500, color = "S&P 500"), size = 1) +
    geom_line(data = filtered_inflation, aes(x = Date, y = InflationIndex * 100, color = "Inflation Rate"), size = 0.5, linetype = "dashed") + 
    scale_y_continuous(
      name = "Normalized Value [%]",
      limits = c(-200, 1000),
      sec.axis = sec_axis(~ . / 100, name = "Inflation Rate [%]") # Rückskalierung der Inflationsrate
    ) +
    scale_color_manual(
      values = c("Gold Price" = "gold", "S&P 500" = "blue", "Inflation Rate" = "red"),
      breaks = c("Gold Price", "S&P 500", "Inflation Rate") # Reihenfolge der Legende
    ) +
    
    labs(x = "Date",
         color = "Legend")
}

# Plot data starting from 2010-01-01
normalized_plot(as.Date("2010-01-01"))


##########################################################
# Results
##########################################################

# Install and load corrplot package if not already installed
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Calculate correlation matrix for selected columns
correlation_matrix <- cor(merged_data[, c("GoldPrice", "SP500Price", "InflationIndex")])

# Visualize the correlation matrix with numbers in the upper triangle
corrplot(correlation_matrix, method = "number", type = "upper",number.cex = 0.8,tl.cex = 0.8,cl.offset = 0.1,cl.cex = 0.4)  


# Create a linear regression model with GoldPrice as dependent variable
lm_model <- lm(GoldPrice ~ SP500Price + InflationIndex, data = merged_data)

# Display results 
#summary(lm_model)

# Install and load ggfortify for diagnostic plots if not already installed
if(!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)

# Generate diagnostic plots in a 2x2 grid
autoplot(lm_model, which = c(1, 2, 3, 5), ncol = 2, nrow = 2)+
  geom_point(shape = 1, alpha = 0.2, size =0.05)


# Install and load randomForest package if not already installed
if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

# Set seed for reproducibility
set.seed(123)


# Create Random Forest model with 500 trees
rf_model <- randomForest(GoldPrice ~ SP500Price + InflationIndex,
                         data = merged_data,
                         importance = TRUE,
                         ntree = 500)
                       

# Visualize variable importance from the Random Forest model
par(cex.axis = 0.8) 
par(cex.lab = 0.8)  
varImpPlot(rf_model)


# Training data: up to end of 2024
train_data <- merged_data[merged_data$Date < as.Date("2025-01-01"), ]

# Test data: from 2025 onwards
test_data <- merged_data[merged_data$Date >= as.Date("2025-01-01"), ]


# Train Random Forest model on training data without cross-validation
set.seed(123)
rf_model_train <- randomForest(GoldPrice ~ SP500Price + InflationIndex,
                               data = train_data,
                               importance = TRUE,
                               ntree = 500)

# Display model results
print(rf_model_train)


# Generate predictions for test data
test_data$Predicted_GoldPrice <- predict(rf_model_train, test_data)

# Check the first few predictions
head(test_data[, c("Date", "GoldPrice", "Predicted_GoldPrice")])


# Install and load caret package if not already installed
if(!require(caret)) install.packages("caret")
library(caret)

# Train Random Forest model on training data with 10-fold cross-validation
set.seed(123)
# Define 10-fold cross-validation method
train_control <- trainControl(method = "cv", number = 10)


rf_model_train_cv <- randomForest(GoldPrice ~ SP500Price + InflationIndex,
                               data = train_data,
                               method = "rf",
                               trControl = train_control,
                               ntree = 500)

# Display model results
print(rf_model_train)


# Generate predictions for test data
test_data$Predicted_GoldPrice2 <- predict(rf_model_train_cv, test_data)

# Check the first few predictions
head(test_data[, c("Date", "GoldPrice", "Predicted_GoldPrice","Predicted_GoldPrice2")])


# Plot actual vs. predicted gold prices
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = GoldPrice, color = "Actual Gold Price")) +
  geom_line(aes(y = Predicted_GoldPrice, color = "Predicted Gold Price"), linetype = "dashed") +
  geom_line(aes(y = Predicted_GoldPrice2, color = "Predicted Gold Price (cv)"), linetype = "dashed") +
  labs(title = "Actual vs. Predicted Gold Prices (Test Data)",
       x = "Date",
       y = "Gold Price (USD)",
       color = "Legend") +
  scale_color_manual(values = c("Actual Gold Price" = "gold", "Predicted Gold Price" = "darkgreen","Predicted Gold Price (cv)"="orange"))


set.seed(17) # For reproducibility

# Define date range
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2025-02-28")

# Generate 12 random dates
random_dates <- sample(seq(start_date, end_date, by = "day"), 12)

# Sort the dates for better readability
random_dates <- sort(random_dates)

# Show random dates
random_dates


# Install and load packages if not already installed
if(!require(patchwork)) install.packages("patchwork")
if(!require(cowplot)) install.packages("cowplot")
if(!require(Metrics)) install.packages("Metrics")
library(ggplot2)
library(gridExtra)
library(Metrics)
library(dplyr)
library(randomForest)
library(patchwork)
library(cowplot)

# Initialize lists to store plots, accuracy metrics, and prediction data
plot_list_rf <- list()
accuracy_list_rf <- list()
prediction_data_list_rf <- list()

# Loop through the randomly generated start dates
for (start_date in random_dates) {
  # Filter training data up to the start date
  train_data <- merged_data %>%
    filter(Date <= start_date)
  
  # Identify the last training point (most recent date in training data)
  last_train_date <- max(train_data$Date)
  last_train_point <- merged_data %>%
    filter(Date == last_train_date)
  
  # Define test data: 10 days after the last training date
  test_start_date <- last_train_date + 1
  test_end_date <- test_start_date + 10
  test_data <- merged_data %>%
    filter(Date >= test_start_date & Date <= test_end_date)
  
  # Combine the last training point with the test data for plotting purposes
  combined_data <- rbind(last_train_point, test_data)
  
  # Skip iteration if no test data is available
  if (nrow(test_data) == 0) next
  
  # Train a Random Forest model using the training data
  rf_model_temp <- randomForest(GoldPrice ~ SP500Price + InflationIndex,
                                data = train_data,
                                importance = TRUE,
                                ntree = 500)
  
  # Make predictions and apply correction to align with the last training point
  combined_data$Predicted_GoldPrice <- predict(rf_model_temp, combined_data)
  last_train_gold_price <- last_train_point$GoldPrice
  predicted_start_value <- combined_data$Predicted_GoldPrice[1]
  correction_factor <- last_train_gold_price - predicted_start_value
  combined_data$Predicted_GoldPrice <- combined_data$Predicted_GoldPrice + correction_factor
  
  # Update the test data with corrected predictions
  test_data$Predicted_GoldPrice <- combined_data$Predicted_GoldPrice[match(test_data$Date, combined_data$Date)]
  
  # Calculate accuracy metrics (RMSE and MAPE) for the test data
  rmse_value <- rmse(test_data$GoldPrice, test_data$Predicted_GoldPrice)
  mape_value <- mean(abs((test_data$GoldPrice - test_data$Predicted_GoldPrice) / test_data$GoldPrice)) * 100
  
  # Store accuracy metrics in a list
  accuracy_list_rf[[as.character(start_date)]] <- data.frame(
    StartDate = as.Date(start_date),
    RMSE = rmse_value,
    MAPE = mape_value
  )
  
  # Save prediction data for each start date in a list
  prediction_data_list_rf[[as.character(start_date)]] <- combined_data %>%
    select(Date, GoldPrice, Predicted_GoldPrice) %>%
    mutate(StartDate = as.Date(start_date))
  
  # Create a plot comparing actual and predicted gold prices
  p <- ggplot(combined_data, aes(x = Date)) +
    geom_line(aes(y = GoldPrice, color = "Actual Gold Price")) +
    geom_line(aes(y = Predicted_GoldPrice, color = "Predicted Gold Price"), linetype = "dashed") +
    labs(title = paste("RF - Start Date:", format(as.Date(start_date), "%m/%d/%Y")),
         x = "Date",
         y = "Gold Price (USD)",
         color = "") + # Entfernt den Legendentitel
    scale_color_manual(values = c("Actual Gold Price" = "gold", "Predicted Gold Price" = "darkgreen")) +
    scale_x_date(date_labels = "%m/%d") + # Amerikanisches Datum ohne Jahr auf der x-Achse
    scale_y_continuous(labels = comma) + # Tausender-Trennzeichen für y-Achse
    theme_minimal() +
    theme(
      plot.title = element_text(size = rel(0.7)),
      axis.title = element_text(size = rel(0.7)),
      axis.text.x = element_text(size = rel(0.7)),
      axis.text.y = element_text(size = rel(0.7)),
      legend.position = "none" # Entfernt die Legende aus den einzelnen Plots
    )
  
  # Add plot to the list of plots
  plot_list_rf[[as.character(start_date)]] <- p
}

# Combine all accuracy results into a single dataframe
accuracy_results_rf <- do.call(rbind, accuracy_list_rf)

# Combine all prediction data into a single dataframe for further analysis or visualization
all_prediction_data_rf <- do.call(rbind, prediction_data_list_rf)

# Display the first few rows of the prediction data
#print("Prediction Data (Random Forest) - Head:")
print(head(all_prediction_data_rf))

# Display accuracy results for the Random Forest model
#print("Accuracy Results (Random Forest):")
print(accuracy_results_rf)

# Combine all plots into a grid with a shared legend at the bottom
final_plot <- patchwork::wrap_plots(plot_list_rf, ncol = 3) + 
  patchwork::plot_layout(guides = 'collect') & 
  theme(legend.position="bottom")

# Display the final combined plot
final_plot



# load packages
library(ggplot2)
library(gridExtra)
library(Metrics)
library(dplyr)
library(randomForest)
library(patchwork)
library(cowplot)

# Initialize lists to store plots, accuracy metrics, and prediction data
plot_list_lm <- list()
accuracy_list_lm <- list()
prediction_data_list_lm <- list()

# Loop through the randomly generated start dates
for (start_date in random_dates) {
  # Filter training data up to the start date
  train_data <- merged_data %>%
    filter(Date <= start_date)
  
  # Identify the last training point (most recent date in training data)
  last_train_date <- max(train_data$Date)
  last_train_point <- merged_data %>%
    filter(Date == last_train_date)
  
  # Define test data: 10 days after the last training date
  test_start_date <- last_train_date + 1
  test_end_date <- test_start_date + 10
  test_data <- merged_data %>%
    filter(Date >= test_start_date & Date <= test_end_date)
  
  # Combine the last training point with the test data for plotting purposes
  combined_data <- rbind(last_train_point, test_data)
  
  # Skip iteration if no test data is availabled
  if (nrow(test_data) == 0) next
  
  # Train a Linear Regression model using the training data
  lm_model_temp <- lm(GoldPrice ~ SP500Price + InflationIndex, data = train_data)
  
  # Make predictions and apply correction to align with the last training point
  combined_data$Predicted_GoldPrice <- predict(lm_model_temp, newdata = combined_data)
  last_train_gold_price <- last_train_point$GoldPrice
  predicted_start_value <- combined_data$Predicted_GoldPrice[1]
  correction_factor <- last_train_gold_price - predicted_start_value
  combined_data$Predicted_GoldPrice <- combined_data$Predicted_GoldPrice + correction_factor
  
  # Update the test data with corrected predictions
  test_data$Predicted_GoldPrice <- combined_data$Predicted_GoldPrice[match(test_data$Date, combined_data$Date)]
  
  # Calculate accuracy metrics (RMSE and MAPE) for the test data
  rmse_value <- rmse(test_data$GoldPrice, test_data$Predicted_GoldPrice)
  mape_value <- mean(abs((test_data$GoldPrice - test_data$Predicted_GoldPrice) / test_data$GoldPrice)) * 100
  
  # Store accuracy metrics in a list
  accuracy_list_lm[[as.character(start_date)]] <- data.frame(
    StartDate = as.Date(start_date),
    RMSE = rmse_value,
    MAPE = mape_value
  )
  
  # Save prediction data for each start date in a list
  prediction_data_list_lm[[as.character(start_date)]] <- combined_data %>%
    select(Date, GoldPrice, Predicted_GoldPrice) %>%
    mutate(StartDate = as.Date(start_date))
  
  # Create a plot comparing actual and predicted gold prices
  p <- ggplot(combined_data, aes(x = Date)) +
    geom_line(aes(y = GoldPrice, color = "Actual Gold Price")) +
    geom_line(aes(y = Predicted_GoldPrice, color = "Predicted Gold Price"), linetype = "dashed") +
    labs(title = paste("LM - Start Date:", format(as.Date(start_date), "%m/%d/%Y")),
         x = "Date",
         y = "Gold Price (USD)",
         color = "") + # Entfernt den Legendentitel
    scale_color_manual(values = c("Actual Gold Price" = "gold", "Predicted Gold Price" = "purple")) +
    scale_x_date(date_labels = "%m/%d") + # Amerikanisches Datum ohne Jahr auf der x-Achse
    scale_y_continuous(labels = comma) + # Tausender-Trennzeichen für y-Achse
    theme_minimal() +
    theme(
      plot.title = element_text(size = rel(0.7)),
      axis.title = element_text(size = rel(0.7)),
      axis.text.x = element_text(size = rel(0.7)),
      axis.text.y = element_text(size = rel(0.7)),
      legend.position = "none" # Entfernt die Legende aus den einzelnen Plots
    )
  
  # Add plot to the list of plots
  plot_list_lm[[as.character(start_date)]] <- p
}

# Combine all accuracy results into a single dataframe
accuracy_results_lm <- do.call(rbind, accuracy_list_lm)

# Combine all prediction data into a single dataframe for further analysis or visualization
all_prediction_data_lm <- do.call(rbind, prediction_data_list_lm)


# Display accuracy results for the lm model
#print("Accuracy Results (Linear Regression):")
#print(accuracy_results_lm)

# Display the first few rows of the prediction data
#print("Prediction Data (Linear Regression) - Head:")
#print(head(all_prediction_data_lm))

# Combine all plots into a grid with a shared legend at the bottom
final_plot <- patchwork::wrap_plots(plot_list_lm, ncol = 3) + 
  patchwork::plot_layout(guides = 'collect') & 
  theme(legend.position="bottom")

# Display the final combined plot
final_plot



# Combine accuracy metrics for comparison across models
accuracy_results_combined <- rbind(
  data.frame(Model = "Linear Regression", accuracy_results_lm),
  data.frame(Model = "Random Forest", accuracy_results_rf)
)

library(ggplot2)
library(patchwork)

# Visualization of metrics over time
# Create RMSE plot comparing models
RMSE <- ggplot(accuracy_results_combined, aes(x = StartDate, y = RMSE, color = Model)) +
  geom_point() + geom_line() +
  scale_color_manual(values = c("Linear Regression" = "purple", "Random Forest" = "darkgreen")) +
  labs(title = "Comparison of Model Accuracy Over Time",
       x = "Start Date",
       y = "RMSE (USD)")

# Create MAPE plot comparing models
MAPE <- ggplot(accuracy_results_combined, aes(x = StartDate, y = MAPE, color = Model)) +
  geom_point() + geom_line() +
  scale_color_manual(values = c("Linear Regression" = "purple", "Random Forest" = "darkgreen")) +
  labs(title = "MAPE Comparison", x = "Start Date", y = "MAPE (%)")

# Combine RMSE and MAPE plots into a single layout with shared legend at the bottom
RMSE / MAPE + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")