### Author: Giri Athrey
### Texas A&M University
### Updated Feburary 2025



# Install necessary packages
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  install.packages("rstanarm")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load the rstanarm library for Bayesian regression
library(rstanarm)
library(ggplot2)
library(NatParksPalettes)

# Load NASS data
data <- read.csv("NASS_data_hatchability.csv")

## Load libraries
library(rstanarm)
library(ggplot2)


# Fit a Bayesian linear regression model
model_bayes <- stan_glm(Hatchability ~ year, data = data, 
                        family = gaussian,
                        prior = normal(0, 2, autoscale = TRUE),  # Normal prior for coefficients
                        prior_intercept = normal(0, 10, autoscale = TRUE),  # Normal prior for intercept
                        chains = 20, iter = 100000, seed = 123)

# Generate future years data frame
future_years <- data.frame(year = (max(data$year)+1):2050)

# Predict using the posterior distribution
predictions <- posterior_predict(model_bayes, newdata = future_years)

# Calculate the 95% credible interval for each future year
ci_95 <- apply(predictions, 2, quantile, probs = c(0.025, 0.975))


# Extract posterior samples from the fitted model
posterior_samples <- as.matrix(model_bayes)

# Calculate ESS using rstan's monitor function
library(rstan)
ess_results <- monitor(posterior_samples, print = FALSE)

# Display ESS for all parameters
ess_results[, "n_eff"]


# Convert to a data frame
ci_95_df <- as.data.frame(t(ci_95))
names(ci_95_df) <- c("Lower", "Upper")
ci_95_df$year <- future_years$year

# Plot the original data and predictions with credible intervals
hatch<-ggplot() +
  geom_point(data = data, aes(x = year, y = Hatchability), col="dodgerblue") +
  geom_line(data = data, aes(x = year, y = Hatchability, group = 1), col="firebrick") +
  geom_ribbon(data = ci_95_df, aes(x = year, ymin = Lower, ymax = Upper), fill = "cornflowerblue", alpha = 0.3)  +
  geom_hline(yintercept = c(0.85, 0.75, 0.65), linetype = "dashed", color = "red")+
  labs(title = "Projected Hatchability with 95% Credible Intervals",
       x = "Year", y = "Hatchability (Hatched/Eggs Set)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )

## Livability

# Fit a Bayesian linear regression model for Livability
model_bayes_livability <- stan_glm(Livability ~ year, data = data, 
                                   family = gaussian,
                                   prior = normal(0, 10, autoscale = TRUE),  
                                   prior_intercept = normal(0, 10, autoscale = TRUE),  
                                   chains = 20, iter = 20000, seed = 123)

# Generate future years data frame
future_years <- data.frame(year = (max(data$year)+1):2050)

# Predict using the posterior distribution for Livability
predictions_livability <- posterior_predict(model_bayes_livability, newdata = future_years)

# Calculate the 95% credible interval for each future year for Livability
ci_95_livability <- apply(predictions_livability, 2, quantile, probs = c(0.025, 0.975))
ci_95_df_livability <- as.data.frame(t(ci_95_livability))
names(ci_95_df_livability) <- c("Lower", "Upper")
ci_95_df_livability$year <- future_years$year

# Plot for Livability
livabilityplot<-ggplot() +
  geom_point(data = data, aes(x = year, y = Livability), col="dodgerblue") +
  geom_line(data = data, aes(x = year, y = Livability, group = 1), col="firebrick") +
  geom_ribbon(data = ci_95_df_livability, aes(x = year, ymin = Lower, ymax = Upper), fill = "cornflowerblue", alpha = 0.3) +
  geom_hline(yintercept = c(0.85, 0.75, 0.65), linetype = "dashed", color = "red") +
  labs(title = "Projected Livability with 95% Credible Intervals",
       x = "Year", y = "Livability (%)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )


# Fit a Bayesian linear regression model for Production_efficiency
model_bayes_efficiency <- stan_glm(Production_efficiency ~ year, data = data, 
                                   family = gaussian,
                                   prior = normal(0, 10, autoscale = TRUE),  
                                   prior_intercept = normal(0, 10, autoscale = TRUE),  
                                   chains = 4, iter = 2000, seed = 123)

# Generate future years data frame
future_years <- data.frame(year = (max(data$year)+1):2050)

# Predict using the posterior distribution for Production_efficiency
predictions_efficiency <- posterior_predict(model_bayes_efficiency, newdata = future_years)

# Calculate the 95% credible interval for each future year for Production_efficiency
ci_95_efficiency <- apply(predictions_efficiency, 2, quantile, probs = c(0.025, 0.975))
ci_95_df_efficiency <- as.data.frame(t(ci_95_efficiency))
names(ci_95_df_efficiency) <- c("Lower", "Upper")
ci_95_df_efficiency$year <- future_years$year

# Plot for Production_efficiency
prdeff<-ggplot() +
  geom_point(data = data, aes(x = year, y = Production_efficiency), col="dodgerblue") +
  geom_line(data = data, aes(x = year, y = Production_efficiency, group = 1), col="firebrick") +
  geom_ribbon(data = ci_95_df_efficiency, aes(x = year, ymin = Lower, ymax = Upper), fill = "cornflowerblue", alpha = 0.3) +
  geom_hline(yintercept = c(0.85, 0.75, 0.65), linetype = "dashed", color = "red") +
  labs(title = "Projected Production Efficiency with 95% Credible Intervals",
       x = "Year", y = "Production Efficiency (%)") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )


combinedgrid<-(hatch/livabilityplot/prdeff)

combinedgrid


## lets check for interactive effects between our predictors

# Including interaction effect
model_interactive <- stan_glm(Hatchability ~ year * Broiler_eggs_set, data = data,
                              family = gaussian,
                              prior = normal(0, 10, autoscale = TRUE),
                              prior_intercept = normal(0, 10, autoscale = TRUE),
                              chains = 4, iter = 2000, seed = 123)

# Creating future data for prediction
future_years <- data.frame(year = seq(max(data$year) + 1, 2050, by = 1))

# Assuming the number of Broiler_eggs_set grows by an average growth rate
average_growth_rate <- mean(diff(data$Broiler_eggs_set) / data$Broiler_eggs_set[-length(data$Broiler_eggs_set)])
future_years$Broiler_eggs_set <- last(data$Broiler_eggs_set) * (1 + average_growth_rate) ^ seq(1, nrow(future_years))

# Predict future values using the interactive model
predictions_interactive <- posterior_predict(model_interactive, newdata = future_years)
mean_predictions_interactive <- apply(predictions_interactive, 2, mean)

# Preparing a dataframe to hold predictions and their credible intervals for plotting
ci_interactive <- apply(predictions_interactive, 2, quantile, probs = c(0.025, 0.975))
ci_df_interactive <- as.data.frame(t(ci_interactive))
names(ci_df_interactive) <- c("Lower", "Upper")
ci_df_interactive$Year <- future_years$year
ci_df_interactive$Mean <- mean_predictions_interactive


## Now we create a composite index called BBPI (Broiler Breeder Performance Index)
data$normalized_hatchability <- scale(data$Hatchability, center = TRUE, scale = TRUE)
data$normalized_livability <- scale(data$Livability, center = TRUE, scale = TRUE)
data$normalized_efficiency <- scale(data$Production_efficiency, center = TRUE, scale = TRUE)
data$normalized_eggs_set <- scale(data$Broiler_eggs_set, center = TRUE, scale = TRUE)

# Assuming data has been standardized and BBPI has been calculated
data$BBPI <- rowMeans(cbind(data$normalized_hatchability, data$normalized_livability,
                            data$normalized_efficiency, data$normalized_eggs_set))

# Bayesian model for BBPI
model_bayes_bbpi <- stan_glm(BBPI ~ year, data = data,
                             family = gaussian,
                             prior = normal(0, 10, autoscale = TRUE),
                             prior_intercept = normal(0, 10, autoscale = TRUE),
                             chains = 20, iter = 20000, seed = 123)

# Predicting future values using BBPI model
future_years <- data.frame(year = (max(data$year)+1):2050)
predictions_bbpi <- posterior_predict(model_bayes_bbpi, newdata = future_years)


# Using a cauchy prior for comparison
model_bayes_bbpi_alt <- stan_glm(BBPI ~ year, data = data,
                                 family = gaussian,
                                 prior = cauchy(0, 2.5, autoscale = TRUE),
                                 prior_intercept = cauchy(0, 2.5, autoscale = TRUE),
                                 chains = 20, iter = 20000, seed = 123)

predictions_bbpi_alt <- posterior_predict(model_bayes_bbpi_alt, newdata = future_years)

# Concordance check (simple visualization)
predicted_means <- apply(predictions_bbpi, 2, mean)
predicted_means_alt <- apply(predictions_bbpi_alt, 2, mean)

plot(future_years$year, predicted_means, type = 'l', col = 'blue', ylim = range(c(predicted_means, predicted_means_alt)))
lines(future_years$year, predicted_means_alt, col = 'red')
legend("topright", legend = c("Original BBPI Model", "Alternative BBPI Model"), col = c("blue", "red"), lty = 1)


# Predict future values using the primary BBPI model
predictions_bbpi <- posterior_predict(model_bayes_bbpi, newdata = future_years)
mean_predictions_bbpi <- apply(predictions_bbpi, 2, mean)

# Calculate credible intervals for the BBPI model predictions
ci_bbpi <- apply(predictions_bbpi, 2, quantile, probs = c(0.025, 0.975))
ci_df_bbpi <- as.data.frame(t(ci_bbpi))
names(ci_df_bbpi) <- c("Lower", "Upper")
ci_df_bbpi$Year <- future_years$year
ci_df_bbpi$Mean <- mean_predictions_bbpi

# Predict future values using the alternative BBPI model
predictions_bbpi_alt <- posterior_predict(model_bayes_bbpi_alt, newdata = future_years)
mean_predictions_bbpi_alt <- apply(predictions_bbpi_alt, 2, mean)

# Calculate credible intervals for the alternative BBPI model predictions
ci_bbpi_alt <- apply(predictions_bbpi_alt, 2, quantile, probs = c(0.025, 0.975))
ci_df_bbpi_alt <- as.data.frame(t(ci_bbpi_alt))
names(ci_df_bbpi_alt) <- c("Lower", "Upper")
ci_df_bbpi_alt$Year <- future_years$year
ci_df_bbpi_alt$Mean <- mean_predictions_bbpi_alt


# The plot thickens
p <- ggplot() + geom_line(data = ci_df_bbpi, aes(x = Year, y = Mean, color = "BBPI Gaussian"), size = 1) +
  geom_line(data = ci_df_bbpi_alt, aes(x = Year, y = Mean, color = "BBPI Cauchy"), size = 1) +
  scale_color_manual(values = natparks.pals("Saguaro",2)) +
  labs(title = "Broiler Breeder Performance Index", x = "Year", y = "Predicted Values") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Adding credible intervals
p <- p +geom_ribbon(data = ci_df_bbpi, aes(x = Year, ymin = Lower, ymax = Upper, fill = "BBPI Gaussian"), alpha = 0.1) +
  geom_ribbon(data = ci_df_bbpi_alt, aes(x = Year, ymin = Lower, ymax = Upper, fill = "BBPI Cauchy"), alpha = 0.1) +
  scale_fill_manual(values = natparks.pals("Yellowstone",2))

# Display the plot
print(p)
