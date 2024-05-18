# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(nortest)

# Load the dataset
bird_flu_data <- read_csv("Avian Influenza (HPAI).csv")

# Summary statistics
summary_stats <- summarise(bird_flu_data,
                           mean_Month = mean(Month),
                           median_Month = median(Month),
                           sd_Month = sd(Month),
                           mean_Latitude = mean(Latitude),
                           median_Latitude = median(Latitude),
                           sd_Latitude = sd(Latitude),
                           mean_target_H5_HPAI = mean(target_H5_HPAI),
                           median_target_H5_HPAI = median(target_H5_HPAI),
                           sd_target_H5_HPAI = sd(target_H5_HPAI))

print(summary_stats)

# Data visualization
# Histogram and box plot for Month
ggplot(bird_flu_data, aes(x = Month)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Bird Captures by Month",
       x = "Month",
       y = "Frequency") +
  theme_minimal()

ggplot(bird_flu_data, aes(y = Month)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bird Captures by Month",
       x = "",
       y = "Month") +
  theme_minimal()

# Scatter plot for Latitude vs. target_H5_HPAI
ggplot(bird_flu_data, aes(x = Latitude, y = target_H5_HPAI)) +
  geom_point(color = "skyblue") +
  labs(title = "Scatter Plot of Latitude vs. Avian Influenza Occurrence",
       x = "Latitude",
       y = "Avian Influenza Occurrence") +
  theme_minimal()

# Q-Q plot for Month
qqnorm(bird_flu_data$Month)
qqline(bird_flu_data$Month, col = "red")
# The data points closely follow the diagonal reference line, 
# which suggests that the 'Month' variable is approximately normally distributed. 
# There are only slight deviations from the line, indicating that the monthly 
# bird captures closely follow a normal distribution pattern.


# Q-Q plot for target_H5_HPAI
qqnorm(bird_flu_data$target_H5_HPAI)
qqline(bird_flu_data$target_H5_HPAI, col = "red") # the 'target_H5_HPAI' variable follows a normal distribution
# In this plot, the data points deviate more noticeably from the diagonal reference 
# line, especially in the tails (extreme values). This suggests that the 'target_H5_HPAI' 
# variable, which likely represents the occurrence or prevalence of avian influenza H5 
# HPAI, does not follow a normal distribution as closely as the 'Month' variable. 
# The deviations in the tails indicate that the distribution of avian flu occurrences 
# may have heavier tails or be skewed compared to a perfect normal distribution.


# Point Estimations and Confidence Intervals
# Mean number of bird captures per month
mean_bird_captures <- mean(bird_flu_data$Month)
sd_bird_captures <- sd(bird_flu_data$Month)
n_bird_captures <- length(bird_flu_data$Month)
margin_error_bird_captures <- qt(0.975, df = n_bird_captures - 1) * sd_bird_captures / sqrt(n_bird_captures)
confidence_interval_bird_captures <- c(mean_bird_captures - margin_error_bird_captures, mean_bird_captures + margin_error_bird_captures)

# Correlation coefficient between latitude and avian flu occurrence
correlation_coef <- cor(bird_flu_data$Latitude, bird_flu_data$target_H5_HPAI)
z <- 0.5 * log((1 + correlation_coef) / (1 - correlation_coef))
se_z <- 1 / sqrt(n_bird_captures - 3)
margin_error_correlation <- qnorm(0.975) * se_z
confidence_interval_correlation <- tanh(c(z - margin_error_correlation, z + margin_error_correlation))

# Print Point Estimations and Confidence Intervals
print("Point Estimations and Confidence Intervals:")
print(paste("Mean number of bird captures per month:", mean_bird_captures))
# "Mean number of bird captures per month: 6.711175171737"

print(paste("95% Confidence Interval for mean number of bird captures per month:", 
            confidence_interval_bird_captures[1], confidence_interval_bird_captures[2]))
# "95% Confidence Interval for mean number of bird captures per month: 6.66538379632544 6.75696654714855"

print(paste("Correlation coefficient between latitude and avian flu occurrence:", correlation_coef))
# "Correlation coefficient between latitude and avian flu occurrence: 0.0503595692791829"

print(paste("95% Confidence Interval for correlation coefficient:", 
            confidence_interval_correlation[1], confidence_interval_correlation[2]))
# "95% Confidence Interval for correlation coefficient: 0.0350367071596313 0.0656587600412967"

# - Analysis revealed significant differences in bird captures across different months, suggesting seasonal variations in avian flu activity.
# - A weak positive correlation was found between latitude and avian flu occurrence, indicating a potential geographical association.
# 
# Limitations of the analysis include:
# - Reliance on a single dataset
# - Assumption of normality for certain variables
# 
# Future research should:
# - Address limitations by incorporating additional data sources
# - Explore alternative analytical approaches
# 
# Despite limitations, the findings contribute to understanding avian influenza dynamics in Ireland.
# The insights are valuable for policymakers and researchers involved in avian flu surveillance and prevention efforts in Ireland and beyond.



# hypothesis testing


# Hypothesis 1:
# Null Hypothesis (H0): The distribution of bird species captured in Ireland for the H5N1 strain of avian flu is uniform across different months.
# Alternative Hypothesis (H1): There is a significant difference in the distribution of bird species captured across different months.
# We can use a statistical test like Analysis of Variance (ANOVA) to determine if there are significant differences in the mean number of bird captures across different months.
# 
# Hypothesis 2:
# Null Hypothesis (H0): The latitude of bird capture locations is not correlated with the occurrence of the H5N1 strain of avian flu.
# Alternative Hypothesis (H1): There is a correlation between the latitude of capture locations and the occurrence of the H5N1 strain.
# We can use a correlation test, such as Pearson's correlation coefficient, to assess the relationship between latitude and avian flu occurrence.



# Hypothesis 1: ANOVA for Month
anova_month <- aov(Month ~ target_H5_HPAI, data = bird_flu_data)
summary(anova_month)
# Hypothesis 1: ANOVA for Month
# The ANOVA results indicate a significant difference in the mean number of bird captures across different levels of avian flu occurrence (target_H5_HPAI) (F(1, 16302) = 51.27, p < 0.001).
# The p-value (8.4e-13) is much smaller than the significance level (e.g., 0.05), indicating strong evidence against the null hypothesis.
# Therefore, we reject the null hypothesis and conclude that there is a significant difference in the distribution of bird captures across different levels of avian flu occurrence.


# Hypothesis 2: Pearson correlation for Latitude and target_H5_HPAI
correlation <- cor.test(bird_flu_data$Latitude, bird_flu_data$target_H5_HPAI, method = "pearson")
print(correlation)
# Hypothesis 2: Pearson Correlation for Latitude and target_H5_HPAI
# The Pearson correlation coefficient between latitude and avian flu occurrence is statistically significant (t(16302) = 6.438, p < 0.001).
# The correlation coefficient (cor) is 0.050, indicating a weak positive correlation between latitude and avian flu occurrence.
# The 95% confidence interval for the correlation coefficient does not include 0, further supporting the significance of the correlation.


# Goodness of Fit Tests and Distribution Checks
# TODO

# Designing a Linear Regression Model

# Select predictor variables
predictor_variables <- c("Latitude", "Longitude")

# Prepare the data
linear_regression_data <- bird_flu_data[, c(predictor_variables, "target_H5_HPAI")]

# Handle missing data (if any)
linear_regression_data <- na.omit(linear_regression_data)

# Fit the linear regression model
linear_model <- lm(target_H5_HPAI ~ Latitude + Longitude, data = linear_regression_data)

# Summarize the model
summary(linear_model)

# The model indicates that both latitude and longitude have significant effects on avian flu occurrence (target_H5_HPAI).
# Interpretation of coefficients:
# 
# For every one-unit increase in latitude, avian flu occurrence increases by approximately 0.009915 units.
# For every one-unit increase in longitude, avian flu occurrence increases by approximately 0.021067 units.
# 
# 
# Both latitude and longitude have statistically significant effects (p-values < 0.05).
# Multiple R-squared value is 0.01, indicating that approximately 1% of the variability in avian flu occurrence is explained by latitude and longitude in the model.
# The model suggests an association between geographical coordinates (latitude and longitude) and avian flu occurrence in Ireland.
# However, the model's low explanatory power indicates that other factors not included may also influence avian flu occurrence.


# Application of nonparametric tests
# Spearman Rank Correlation Test with permutation for Latitude
spearman_latitude <- cor.test(bird_flu_data$Latitude, bird_flu_data$target_H5_HPAI, method = "spearman")
spearman_latitude

# Spearman Rank Correlation Test with permutation for Longitude
spearman_longitude <- cor.test(bird_flu_data$Longitude, bird_flu_data$target_H5_HPAI, method = "spearman")
spearman_longitude

# Function to calculate Spearman correlation with permutation
spearman_permutation_test <- function(x, y, num_permutations = 10000) {
  observed_rho <- cor(x, y, method = "spearman")
  permuted_rhos <- replicate(num_permutations, cor(x, sample(y), method = "spearman"))
  p_value <- mean(abs(permuted_rhos) >= abs(observed_rho))
  return(list(rho = observed_rho, p_value = p_value))
}

# Apply the permutation test for Latitude
latitude_permutation_test <- spearman_permutation_test(bird_flu_data$Latitude, bird_flu_data$target_H5_HPAI)
print(paste("Spearman correlation (Latitude):", latitude_permutation_test$rho))
print(paste("Permutation test p-value (Latitude):", latitude_permutation_test$p_value))

# Apply the permutation test for Longitude
longitude_permutation_test <- spearman_permutation_test(bird_flu_data$Longitude, bird_flu_data$target_H5_HPAI)
print(paste("Spearman correlation (Longitude):", longitude_permutation_test$rho))
print(paste("Permutation test p-value (Longitude):", longitude_permutation_test$p_value))

# The Spearman correlation for both latitude and longitude is low, suggesting a weak positive relationship.
# The permutation test p-values are very low, indicating that the correlations are 
# statistically significant, i.e., the relationship between latitude/longitude and avian 
# influenza occurrence is unlikely to be due to random chance.


# Enhanced Linear Regression Model with Residuals Check and Transformations

# Check residuals for the initial linear model
par(mfrow = c(2, 2))
plot(linear_model)

# Evaluate potential transformations
# Scatter plot for Latitude vs. target_H5_HPAI
ggplot(linear_regression_data, aes(x = Latitude, y = target_H5_HPAI)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Latitude vs. Avian Influenza Occurrence",
       x = "Latitude",
       y = "Avian Influenza Occurrence")

# Scatter plot for Longitude vs. target_H5_HPAI
ggplot(linear_regression_data, aes(x = Longitude, y = target_H5_HPAI)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Longitude vs. Avian Influenza Occurrence",
       x = "Longitude",
       y = "Avian Influenza Occurrence")

# Check the distribution of the target variable
hist(linear_regression_data$target_H5_HPAI, main = "Histogram of target_H5_HPAI", xlab = "target_H5_HPAI")

# Apply log transformation
linear_regression_data$log_target_H5_HPAI <- log1p(linear_regression_data$target_H5_HPAI) # log1p to handle zero values

# Fit the linear regression model with the transformed target variable
transformed_linear_model <- lm(log_target_H5_HPAI ~ Latitude + Longitude, data = linear_regression_data)

# Summarize the transformed model
summary(transformed_linear_model)

# Plot residuals of the transformed model
par(mfrow = c(2, 2))
plot(transformed_linear_model)

# Compare initial and transformed models
print("Initial Linear Model Summary:")
print(summary(linear_model))

print("Transformed Linear Model Summary:")
print(summary(transformed_linear_model))

# Reset plotting layout
par(mfrow = c(1, 1))