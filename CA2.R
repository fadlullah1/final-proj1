# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(broom)
library(readxl)
library(car)
library(scales)
library(lmtest) 
library(lm.beta)  


# Read the Excel file
data <- read_excel("Dataset_2024.xlsx")
View(data)

# View the structure of the dataset
str(data)

# Check for missing values
sum(is.na(data))

# Remove or impute missing values if necessary
data <- na.omit(data)  # Removing rows with NA values

#Renaming variables 
summary(data)

colnames(data)[colnames(data) == "Body fat (%)"] <- "Body_fat"
colnames(data)[colnames(data) == "Age (years)"] <- "Age"
colnames(data)[colnames(data) == "Chest circumference (cm)"] <- "chest_cir"
colnames(data)[colnames(data) == "Density (g/cm³)"] <- "Density"
colnames(data)[colnames(data) == "Knee circumference (cm)"] <- "Knee_cir"
colnames(data)[colnames(data) == "Weight (lbs)"] <- "Weight"


remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

# Remove outliers for each column
columns_to_check <- c("Body_fat", "Age", "chest_cir", "Density", "Knee_cir", "Weight")
for (column in columns_to_check) {
  data <- remove_outliers(data, column)
}

# Display the first few rows of the cleaned dataset
head(data)




# Descriptive statistics function
descriptive_stats <- function(column) {
  stats <- data %>%
    summarise(
      n = n(),
      mean = mean(!!sym(column), na.rm = TRUE),
      sd = sd(!!sym(column), na.rm = TRUE),
      median = median(!!sym(column), na.rm = TRUE),
      Q1 = quantile(!!sym(column), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(column), 0.75, na.rm = TRUE),
      min = min(!!sym(column), na.rm = TRUE),
      max = max(!!sym(column), na.rm = TRUE)
    )
  return(stats)
}

# Calculate descriptive statistics for each variable
body_fat_stats <- descriptive_stats("Body_fat")
age_stats <- descriptive_stats("Age")
chest_cir_stats <- descriptive_stats("chest_cir")
density_stats <- descriptive_stats("Density")
knee_cir_stats <- descriptive_stats("Knee_cir")
weight_stats <- descriptive_stats("Weight")

# Print descriptive statistics
print("Body Fat Statistics:")
print(body_fat_stats)

print("Age Statistics:")
print(age_stats)

print("Chest Circumference Statistics:")
print(chest_cir_stats)

print("Density Statistics:")
print(density_stats)

print("Knee Circumference Statistics:")
print(knee_cir_stats)

print("Weight Statistics:")
print(weight_stats)

library(psych)
windows(20,10)
pairs.panels(data,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "spearman",
             pch = 21,
             lm =FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)

# Correlation coefficients
cor_matrix <- cor(data[, c("Body_fat", "Age", "chest_cir", "Density", "Knee_cir", "Weight")])
print(cor_matrix)

windows(20,12)
par(mfrow= c(4,2))
# Scatter plot for Body Fat vs Age
ggplot(data, aes(x = Age, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Age", x = "Age (years)", y = "Body Fat (%)")

# Scatter plot for Body Fat vs Chest Circumference
ggplot(data, aes(x = chest_cir, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Chest Circumference", x = "Chest Circumference (cm)", y = "Body Fat (%)")

# Scatter plot for Body Fat vs Density
ggplot(data, aes(x = Density, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Density", x = "Density (g/cm³)", y = "Body Fat (%)")

# Scatter plot for Body Fat vs Knee Circumference
ggplot(data, aes(x = Knee_cir, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Knee Circumference", x = "Knee Circumference (cm)", y = "Body Fat (%)")

# Scatter plot for Body Fat vs Weight
ggplot(data, aes(x = Weight, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Weight", x = "Weight (lbs)", y = "Body Fat (%)")




# Function to calculate and print correlation coefficient with a brief explanation
calc_correlation <- function(x, y) {
  cor_value <- cor(x, y, use = "complete.obs")
  explanation <- ifelse(abs(cor_value) > 0.7, "strong", 
                        ifelse(abs(cor_value) > 0.4, "moderate", "weak"))
  cat(sprintf("Correlation between %s and %s: %.2f (%s relationship)\n", 
              deparse(substitute(x)), deparse(substitute(y)), cor_value, explanation))
}

# Calculate and print correlation coefficients
calc_correlation(data$Age, data$Body_fat)
calc_correlation(data$chest_cir, data$Body_fat)
calc_correlation(data$Density, data$Body_fat)
calc_correlation(data$Knee_cir, data$Body_fat)
calc_correlation(data$Weight, data$Body_fat)

# Scatter plots with correlation values
ggplot(data, aes(x = Age, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = paste("Body Fat vs Age (Correlation:", round(cor(data$Age, data$Body_fat), 2), ")"),
       x = "Age (years)", y = "Body Fat (%)")

ggplot(data, aes(x = chest_cir, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = paste("Body Fat vs Chest Circumference (Correlation:", round(cor(data$chest_cir, data$Body_fat), 2), ")"),
       x = "Chest Circumference (cm)", y = "Body Fat (%)")

ggplot(data, aes(x = Density, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = paste("Body Fat vs Density (Correlation:", round(cor(data$Density, data$Body_fat), 2), ")"),
       x = "Density (g/cm³)", y = "Body Fat (%)")

ggplot(data, aes(x = Knee_cir, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = paste("Body Fat vs Knee Circumference (Correlation:", round(cor(data$Knee_cir, data$Body_fat), 2), ")"),
       x = "Knee Circumference (cm)", y = "Body Fat (%)")

ggplot(data, aes(x = Weight, y = Body_fat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = paste("Body Fat vs Weight (Correlation:", round(cor(data$Weight, data$Body_fat), 2), ")"),
       x = "Weight (lbs)", y = "Body Fat (%)")




#Create boxplots for each measurement
ggplot(data, aes(y = Body_fat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Body Fat", y = "Body Fat (%)")

ggplot(data, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age", y = "Age (years)")

ggplot(data, aes(y = chest_cir)) +
  geom_boxplot() +
  labs(title = "Boxplot of Chest Circumference", y = "Chest Circumference (cm)")

ggplot(data, aes(y = Density)) +
  geom_boxplot() +
  labs(title = "Boxplot of Density", y = "Density (g/cm³)")

ggplot(data, aes(y = Knee_cir)) +
  geom_boxplot() +
  labs(title = "Boxplot of Knee Circumference", y = "Knee Circumference (cm)")

ggplot(data, aes(y = Weight)) +
  geom_boxplot() +
  labs(title = "Boxplot of Weight", y = "Weight (lbs)")




library(corrplot)
# Calculate the correlation matrix
cor_matrix <- cor(data[, c("Body_fat", "Age", "chest_cir", "Density", "Knee_cir", "Weight")], use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
windows(20,10)
corrplot(cor_matrix, method = "number", type = "upper")

# Interpretation of the correlation matrix
interpret_correlation <- function(cor_matrix) {
  for (i in 1:nrow(cor_matrix)) {
    for (j in (i+1):ncol(cor_matrix)) {
      correlation <- cor_matrix[i, j]
      strength <- ifelse(abs(correlation) > 0.7, "strong", 
                         ifelse(abs(correlation) > 0.4, "moderate", "weak"))
      cat(sprintf("The correlation between %s and %s is %.2f, indicating a %s relationship.\n", 
                  rownames(cor_matrix)[i], colnames(cor_matrix)[j], correlation, strength))
    }
  }
}

# Interpret the correlations
interpret_correlation(cor_matrix)


# Histogram with density plot for Body_fat
hist_plot <- ggplot(data, aes(x = Body_fat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(col = "red") +
  labs(title = "Histogram and Density Plot of Body_fat", x = "Body_fat", y = "Density") +
  theme_minimal()
print(hist_plot)

# Q-Q plot for Body_fat
qq_plot <- ggplot(data, aes(sample = Body_fat)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Q-Q Plot of Body_fat", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(qq_plot)

# Skewness for Body_fat
skewness_value <- skewness(data$Body_fat, na.rm = TRUE)
cat(sprintf("Skewness of Body_fat: %.2f\n", skewness_value))
if (skewness_value > 0) {
  cat("The distribution is positively skewed.\n")
} else if (skewness_value < 0) {
  cat("The distribution is negatively skewed.\n")
} else {
  cat("The distribution is approximately symmetric.\n")
}

# Shapiro-Wilk normality test for Body_fat
shapiro_test <- shapiro.test(data$Body_fat)
cat("Shapiro-Wilk normality test for Body_fat:\n")
print(shapiro_test)
if (shapiro_test$p.value > 0.05) {
  cat("The data is normally distributed (fail to reject H0).\n")
} else {
  cat("The data is not normally distributed (reject H0).\n")
}

# Log Transformation
data$log_Body_fat <- log(data$Body_fat)

# Plot and test normality for log-transformed Body_fat
# Histogram with density plot
hist_plot_log <- ggplot(data, aes(x = log_Body_fat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(col = "red") +
  labs(title = "Histogram and Density Plot of Log-transformed Body_fat", x = "Log Body_fat", y = "Density") +
  theme_minimal()
print(hist_plot_log)

# Q-Q plot
qq_plot_log <- ggplot(data, aes(sample = log_Body_fat)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Q-Q Plot of Log-transformed Body_fat", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(qq_plot_log)
View(qq_plot_log)

# Skewness
skewness_log <- skewness(data$log_Body_fat, na.rm = TRUE)
cat(sprintf("Skewness of Log-transformed Body_fat: %.2f\n", skewness_log))
if (skewness_log > 0) {
  cat("The distribution is positively skewed.\n")
} else if (skewness_log < 0) {
  cat("The distribution is negatively skewed.\n")
} else {
  cat("The distribution is approximately symmetric.\n")
}

# Shapiro-Wilk normality test for log-transformed Body_fat
shapiro_test_log <- shapiro.test(data$log_Body_fat)
cat("Shapiro-Wilk normality test for Log-transformed Body_fat:\n")
print(shapiro_test_log)
if (shapiro_test_log$p.value > 0.05) {
  cat("The log-transformed data is normally distributed (fail to reject H0).\n")
} else {
  cat("The log-transformed data is not normally distributed (reject H0).\n")
}


# Square Root Transformation
data$sqrt_Body_fat <- sqrt(data$Body_fat)

# Plot and test normality for square root transformed Body_fat
# Histogram with density plot
hist_plot_sqrt <- ggplot(data, aes(x = sqrt_Body_fat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(col = "red") +
  labs(title = "Histogram and Density Plot of Square Root-transformed Body_fat", x = "Square Root Body_fat", y = "Density") +
  theme_minimal()
print(hist_plot_sqrt)

# Q-Q plot
qq_plot_sqrt <- ggplot(data, aes(sample = sqrt_Body_fat)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Q-Q Plot of Square Root-transformed Body_fat", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(qq_plot_sqrt)

# Skewness
skewness_sqrt <- skewness(data$sqrt_Body_fat, na.rm = TRUE)
cat(sprintf("Skewness of Square Root-transformed Body_fat: %.2f\n", skewness_sqrt))
if (skewness_sqrt > 0) {
  cat("The distribution is positively skewed.\n")
} else if (skewness_sqrt < 0) {
  cat("The distribution is negatively skewed.\n")
} else {
  cat("The distribution is approximately symmetric.\n")
}

# Shapiro-Wilk normality test for square root transformed Body_fat
shapiro_test_sqrt <- shapiro.test(data$sqrt_Body_fat)
cat("Shapiro-Wilk normality test for Square Root-transformed Body_fat:\n")
print(shapiro_test_sqrt)
if (shapiro_test_sqrt$p.value > 0.05) {
  cat("The square root-transformed data is normally distributed (fail to reject H0).\n")
} else {
  cat("The square root-transformed data is not normally distributed (reject H0).\n")
}

# Load necessary library
library(MASS)

# Box-Cox Transformation
bc <- boxcox(lm(Body_fat ~ 1, data = data))
lambda <- bc$x[which.max(bc$y)]
data$bc_Body_fat <- (data$Body_fat^lambda - 1) / lambda

# Plot and test normality for Box-Cox transformed Body_fat
# Histogram with density plot
hist_plot_bc <- ggplot(data, aes(x = bc_Body_fat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(col = "red") +
  labs(title = "Histogram and Density Plot of Box-Cox-transformed Body_fat", x = "Box-Cox Body_fat", y = "Density") +
  theme_minimal()
print(hist_plot_bc)

# Q-Q plot
qq_plot_bc <- ggplot(data, aes(sample = bc_Body_fat)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "Q-Q Plot of Box-Cox-transformed Body_fat", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(qq_plot_bc)

# Skewness
skewness_bc <- skewness(data$bc_Body_fat, na.rm = TRUE)
cat(sprintf("Skewness of Box-Cox-transformed Body_fat: %.2f\n", skewness_bc))
if (skewness_bc > 0) {
  cat("The distribution is positively skewed.\n")
} else if (skewness_bc < 0) {
  cat("The distribution is negatively skewed.\n")
} else {
  cat("The distribution is approximately symmetric.\n")
}

# Shapiro-Wilk normality test for Box-Cox transformed Body_fat
shapiro_test_bc <- shapiro.test(data$bc_Body_fat)
cat("Shapiro-Wilk normality test for Box-Cox-transformed Body_fat:\n")
print(shapiro_test_bc)
if (shapiro_test_bc$p.value > 0.05) {
  cat("The Box-Cox-transformed data is normally distributed (fail to reject H0).\n")
} else {
  cat("The Box-Cox-transformed data is not normally distributed (reject H0).\n")
}


# Fit the full model
full_model <- lm(Body_fat ~ Age + chest_cir + Density + Knee_cir + Weight, data = data)
summary(full_model)

# Perform stepwise regression
step_model <- stepAIC(full_model, direction = "both")
summary(step_model)

library(car) 
# Check for multicollinearity using VIF
vif_values <- vif(step_model)
print(vif_values)

# Interpretation of VIF
cat("VIF Values:\n")
print(vif_values)
if (any(vif_values > 5)) {
  cat("Warning: Multicollinearity detected (VIF > 5).\n")
} else {
  cat("No significant multicollinearity detected (VIF <= 5).\n")
}

# Fit the final model based on stepwise selection
final_model <- lm(Body_fat ~ chest_cir + Density + Weight, data = data)
summary(final_model)

# Diagnostic plots for the final model
windows(20,10)
par(mfrow = c(2, 2))
plot(final_model)

# Check residuals
residuals <- residuals(final_model)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)
qqnorm(residuals)
qqline(residuals, col = "red")

# Shapiro-Wilk test for normality of residuals
shapiro_test_residuals <- shapiro.test(residuals)
cat("Shapiro-Wilk normality test for residuals:\n")
print(shapiro_test_residuals)
if (shapiro_test_residuals$p.value > 0.05) {
  cat("The residuals are normally distributed (fail to reject H0).\n")
} else {
  cat("The residuals are not normally distributed (reject H0).\n")
}


# Adjusted R-squared
adj_r_squared <- summary(final_model)$adj.r.squared
cat("Adjusted R-squared:", adj_r_squared, "\n")

# Regression coefficients with 95% confidence intervals and p-values
coef_summary <- summary(final_model)$coefficients
conf_intervals <- confint(final_model)
conf_intervals

# Combine the results
final_model_summary <- cbind(coef_summary, conf_intervals)
colnames(final_model_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "2.5 %", "97.5 %")
print(final_model_summary)

# Standardized regression coefficients
# We manually standardize coefficients since lm.beta is not available
standardize <- function(model) {
  sdy <- sd(model$model[[1]])
  sdx <- sapply(model$model[-1], sd)
  b <- model$coef[-1]
  beta <- b * sdx / sdy
  return(beta)
}

std_coefficients <- standardize(final_model)
cat("Standardized regression coefficients:\n")
print(std_coefficients)



# Create diagnostic plots and save to files
par(mfrow = c(2, 2))
png("diagnostic_plots.png", width = 800, height = 800)
plot(final_model)
dev.off()

# Linearity: Residuals vs Fitted plot should show no clear pattern
# Independence: Checked via Durbin-Watson test
dw_test <- dwtest(final_model)
print(dw_test)

# Homoscedasticity: Scale-Location plot should show no clear pattern
# Normality: Q-Q plot and Shapiro-Wilk test
residuals <- residuals(final_model)
png("histogram_residuals.png")
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)
dev.off()

png("qqplot_residuals.png")
qqnorm(residuals)
qqline(residuals, col = "red")
dev.off()

shapiro_test_residuals <- shapiro.test(residuals)
cat("Shapiro-Wilk normality test for residuals:\n")
print(shapiro_test_residuals)
if (shapiro_test_residuals$p.value > 0.05) {
  cat("The residuals are normally distributed (fail to reject H0).\n")
} else {
  cat("The residuals are not normally distributed (reject H0).\n")
}

# Homoscedasticity: Breusch-Pagan test
bp_test <- bptest(final_model)
cat("Breusch-Pagan test for homoscedasticity:\n")
print(bp_test)
if (bp_test$p.value > 0.05) {
  cat("The residuals are homoscedastic (fail to reject H0).\n")
} else {
  cat("The residuals are heteroscedastic (reject H0).\n")
}


# Create a summary table for model comparisons
model_comparison <- data.frame(
  Model = c("Full Model", "Stepwise Model", "Final Model"),
  Adjusted_R2 = c(summary(full_model)$adj.r.squared, summary(step_model)$adj.r.squared, summary(final_model)$adj.r.squared),
  AIC = c(AIC(full_model), AIC(step_model), AIC(final_model)),
  BIC = c(BIC(full_model), BIC(step_model), BIC(final_model))
)

print(model_comparison)

# Compare residuals
residuals_full <- residuals(full_model)
residuals_step <- residuals(step_model)
residuals_final <- residuals(final_model)

# Plot histograms of residuals
windows(20,10)
par(mfrow = c(1, 3))
hist(residuals_full, main = "Residuals of Full Model", xlab = "Residuals", breaks = 20)
hist(residuals_step, main = "Residuals of Stepwise Model", xlab = "Residuals", breaks = 20)
hist(residuals_final, main = "Residuals of Final Model", xlab = "Residuals", breaks = 20)

# Shapiro-Wilk normality test for residuals
shapiro_full <- shapiro.test(residuals_full)
shapiro_step <- shapiro.test(residuals_step)
shapiro_final <- shapiro.test(residuals_final)

# Print Shapiro-Wilk test results
cat("Shapiro-Wilk normality test for Full Model residuals:\n")
print(shapiro_full)

cat("Shapiro-Wilk normality test for Stepwise Model residuals:\n")
print(shapiro_step)

cat("Shapiro-Wilk normality test for Final Model residuals:\n")
print(shapiro_final)
