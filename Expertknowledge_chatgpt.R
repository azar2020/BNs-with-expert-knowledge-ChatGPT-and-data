# Read and convert data to factors.
# Define and create network structures.
# Fit Bayesian networks and compute BIC scores.
# Perform bootstrapping to calculate BIC scores and Bayes factors.
# Plot BIC scores and Bayes factors using line plots and boxplots.
# Compute and display the mean and standard deviation of Bayes factors.

# Set working directory
setwd("C:/Azar_Drive/relationships-between-variables1/01_preprocessing/results")

# Define number of rounding digits
options(digits = 10)

# Load required libraries
library(bnlearn)
library(bnstruct)
library(ggplot2)
library(Rgraphviz)
library(caret)
library(BDgraph)
library(reshape2)  # Required for melt function

# Read and preprocess data
Ontario_discretized_data <- read.csv("Discretized_Ontario_data_2.csv")
Ontario_discretized_data <- lapply(Ontario_discretized_data, as.factor)
Ontario_discretized_data <- data.frame(Ontario_discretized_data)
str(Ontario_discretized_data)  # Verify data

# Define the network structures
dag_string_expert <- "[degree_days][solar_radiation][air_temperature][wind_speed][relative_humidity][total_precipitation|relative_humidity][is_weekend][web_page_views_last_seven_days][number_of_boats|web_page_views_last_seven_days:total_precipitation:is_weekend:wind_speed:air_temperature][app_total_trips][app_hours_out|app_total_trips:web_page_views_last_seven_days:total_precipitation:is_weekend:wind_speed:air_temperature][app_catch_rate|app_total_trips:app_hours_out]"
dag_string_chatgpt <- "[degree_days][solar_radiation][air_temperature|degree_days:solar_radiation][total_precipitation][wind_speed][relative_humidity|air_temperature:wind_speed:total_precipitation][is_weekend][web_page_views_last_seven_days|air_temperature:degree_days:is_weekend][number_of_boats|is_weekend:web_page_views_last_seven_days][app_hours_out|air_temperature:solar_radiation:wind_speed:relative_humidity:is_weekend][app_total_trips|is_weekend:web_page_views_last_seven_days:air_temperature][app_catch_rate|number_of_boats:app_hours_out:air_temperature:solar_radiation:wind_speed:relative_humidity]"
dag_string_best <- "[air_temperature][solar_radiation|air_temperature][is_weekend|air_temperature:solar_radiation][web_page_views_last_seven_days|air_temperature:solar_radiation][degree_days|air_temperature:solar_radiation:is_weekend][number_of_boats|web_page_views_last_seven_days][app_total_trips|web_page_views_last_seven_days][wind_speed|degree_days:air_temperature:solar_radiation:is_weekend][app_catch_rate|app_total_trips][total_precipitation|app_total_trips][relative_humidity|degree_days:air_temperature:solar_radiation:wind_speed][app_hours_out|total_precipitation]"

# Create the networks
dag_expert <- model2network(dag_string_expert)
dag_chatgpt <- model2network(dag_string_chatgpt)
dag_best <- model2network(dag_string_best)

# Plot the networks
graphviz.plot(dag_expert)
graphviz.plot(dag_chatgpt)
graphviz.plot(dag_best)

# Fit the networks
fitted_bn_expert <- bn.fit(dag_expert, Ontario_discretized_data)
fitted_bn_chatgpt <- bn.fit(dag_chatgpt, Ontario_discretized_data)
fitted_bn_best <- bn.fit(dag_best, Ontario_discretized_data)

# Evaluate BIC scores
score_expert <- score(dag_expert, Ontario_discretized_data, type = "bde")
score_chatgpt <- score(dag_chatgpt, Ontario_discretized_data, type = "bde")
score_best <- score(dag_best, Ontario_discretized_data, type = "bde")

# Define number of bootstrap resamples
num_resamples <- 10

# Initialize vectors for storing BIC scores and Bayes factors
bic_scores_expert <- numeric(num_resamples)
bic_scores_chatgpt <- numeric(num_resamples)
bic_scores_best <- numeric(num_resamples)

bf_expert_vs_chatgpt <- numeric(num_resamples)
bf_best_vs_expert <- numeric(num_resamples)
bf_best_vs_chatgpt <- numeric(num_resamples)

# Perform bootstrapping
set.seed(123) # For reproducibility
for (i in 1:num_resamples) {
  # Generate bootstrap sample indices
  resampled_indices <- sample(nrow(Ontario_discretized_data), replace = TRUE)
  
  # Extract the resampled dataset
  resampled_data <- data.frame(Ontario_discretized_data)[resampled_indices, ]
  
  # Calculate the BIC score for each network
  bic_scores_expert[i] <- score(dag_expert, resampled_data, type = "bde")
  bic_scores_chatgpt[i] <- score(dag_chatgpt, resampled_data, type = "bde")
  bic_scores_best[i] <- score(dag_best, resampled_data, type = "bde")
  
  # Calculate Bayes factors
  bf_expert_vs_chatgpt[i] <- BF(dag_expert, dag_chatgpt, resampled_data, score = "bde", log = TRUE)
  bf_best_vs_expert[i] <- BF(dag_best, dag_expert, resampled_data, score = "bde", log = TRUE)
  bf_best_vs_chatgpt[i] <- BF(dag_best, dag_chatgpt, resampled_data, score = "bde", log = TRUE)
}

# Combine all BIC scores into one data frame
bic_data_combined <- data.frame(
  Bootstrap_Sample = rep(1:num_resamples, 3),
  BIC_Score = c(bic_scores_expert, bic_scores_chatgpt, bic_scores_best),
  Network_Type = factor(rep(c("Expert", "ChatGPT", "Data"), each = num_resamples))
)

# Plot the combined BIC scores
ggplot(bic_data_combined, aes(x = Bootstrap_Sample, y = BIC_Score, color = Network_Type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Bootstrap Sample", y = "BIC Score", color = "Network Type") +
  theme_minimal()

# Create a boxplot to show the distribution of BIC scores across network types without outlier points
ggplot(bic_data_combined, aes(x = Network_Type, y = BIC_Score, fill = Network_Type)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Network Type", y = "BIC Score", fill = "Network Type") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine Bayes factors into one data frame
bayes_factor <- data.frame(
  Bootstrap_Sample = rep(1:num_resamples, 3),
  BIC_Score = c(bf_expert_vs_chatgpt, bf_best_vs_expert, bf_best_vs_chatgpt),
  Network_Type = factor(rep(c("Expert vs. ChatGPT", "Data vs. Experts", "Data vs. ChatGPT"), each = num_resamples))
)

# Plot the combined Bayes factors with custom colors
ggplot(bayes_factor, aes(x = Bootstrap_Sample, y = BIC_Score, color = Network_Type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Bootstrap Sample", y = "Log Bayes Factor", color = "Comparison Type") +
  scale_color_manual(values = c("Expert vs. ChatGPT" = "#800080",  # Purple
                                "Data vs. Experts" = "#008080",    # Teal
                                "Data vs. ChatGPT" = "#FFA500")) + # Orange
  theme_minimal()

# Create a boxplot for Bayes factors
ggplot(bayes_factor, aes(x = Network_Type, y = BIC_Score, fill = Network_Type)) +
  geom_boxplot() +
  labs(x = "Comparison Type", y = "Log Bayes Factor", fill = "Comparison Type") +
  scale_fill_manual(values = c("Expert vs. ChatGPT" = "#800080",  # Purple
                               "Data vs. Experts" = "#008080",    # Teal
                               "Data vs. ChatGPT" = "#FFA500")) + # Orange
  theme_minimal() +
  theme(legend.position = "none")

# Calculate mean and standard deviation of Bayes factors
mean_bf_best_vs_chatgpt <- mean(bf_best_vs_chatgpt)
sd_bf_best_vs_chatgpt <- sd(bf_best_vs_chatgpt)

# Print Bayes factors
BF(dag_expert, dag_chatgpt, Ontario_discretized_data, score = "bde", log = TRUE)
BF(dag_best, dag_expert, Ontario_discretized_data, score = "bde", log = TRUE)
BF(dag_best, dag_chatgpt, Ontario_discretized_data, score = "bde", log = TRUE)

# Cross-validation for Bayesian Networks
cv_result_expert <- bn.cv(
  Ontario_discretized_data,
  bn = dag_expert,
  loss = "pred",
  loss.args = list(target = "number_of_boats", predict = "bayes-lw"),
  debug = TRUE
)

cv_result_chatgpt <- bn.cv(
  Ontario_discretized_data,
  bn = dag_chatgpt,
  loss = "pred",
  loss.args = list(target = "number_of_boats", predict = "bayes-lw"),
  debug = TRUE
)

cv_result_best <- bn.cv(
  Ontario_discretized_data,
  bn = dag_best,
  loss = "pred",
  loss.args = list(target = "number_of_boats", predict = "bayes-lw"),
  debug = TRUE
)

# Example error vectors
errors_expert <- c(0.02727, 0.01785, 0.03603, 0.04504, 0.03539, 0.00892, 0.01785, 0.03571, 0.02702, 0.01801)
errors_chatgpt <- c(0.03539, 0.01769, 0.02654, 0.00884, 0.02654, 0.00884, 0.03539, 0.026548, 0.026785, 0.04464)
errors_best <- c(0.017699, 0.026548, 0.04424, 0.00884, 0.035398, 0.035398, 0.0088495, 0.008849, 0.044642, 0.01785)

# Paired t-tests
t_test_expert <- t.test(errors_expert, errors_best, paired = TRUE)
t_test_chatgpt <- t.test(errors_chatgpt, errors_best, paired = TRUE)

print(t_test_expert)
print(t_test_chatgpt)

# Create a data frame for errors
errors_df <- data.frame(
  fold = seq_along(errors_expert),
  Chatgpt = errors_chatgpt,
  Data = errors_best,
  Expert = errors_expert
)

# Reshape data to long format
errors_long <- melt(errors_df, id.vars = "fold", variable.name = "Method", value.name = "error")

# Create a line plot to show the variation of errors
ggplot(errors_long, aes(x = fold, y = error, color = Method, group = Method)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Folds",
    y = "Prediction Error"
  ) +
  scale_y_continuous(limits = c(0, 0.1)) +
  theme_minimal()

# Create a boxplot to show the distribution of errors across methods
ggplot(errors_long, aes(x = Method, y = error, fill = Method)) +
  geom_boxplot() +
  labs(
    x = "Method",
    y = "Prediction Error"
  ) +
  scale_y_continuous(limits = c(0, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none")

