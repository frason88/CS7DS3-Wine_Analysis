library(tidyverse)
library(brms)
library(rstan)
library(lattice)
library(plotly)
library(ggplot2)
library(lme4)


# Read the data
wine_data <- read.csv("wine_review.csv")

# Explore the data
summary(wine_data)
str(wine_data)

# Visualize the distribution of rating points across wine varieties
ggplot(wine_data, aes(x = variety, y = points, fill = variety)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Wine Variety", y = "Rating Points", title = "Distribution of Rating Points by Wine Variety")

# Fit a multiple linear regression model with grouping structure and random slopes
model <- brm(points ~ price + Finish + Rich + Fruit + (1 + price | variety),
             data = wine_data, family = gaussian(),
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 5), class = b)),
             iter = 2000, warmup = 1000, chains = 4, cores = 4)

# Summarize the model results
summary(model)

# Extract the fixed effects coefficients
fixed_effects <- fixef(model)
fixed_effects

# Extract the random effects coefficients
random_effects <- ranef(model)
random_effects



# Check model diagnostics
plot(model)

# Perform posterior predictive checks
pp_check(model)
print(pp_check)




# Calculate the marginal effects of price on rating points for each wine variety
conditional_effects <- conditional_effects(model, "price")
plot(conditional_effects)

# Calculate error metrics
actual_points <- wine_data$points
predicted_points <- fitted(model)

# R-squared
r_squared <- cor(actual_points, predicted_points)^2

# Average Error
average_error <- mean(actual_points - predicted_points)

# Mean Squared Error
mse <- mean((actual_points - predicted_points)^2)

# Median Absolute Error
mae <- median(abs(actual_points - predicted_points))

# Root Mean Squared Error
rmse <- sqrt(mse)

# Print error metrics
cat("Error Metrics:\n")
cat("R-squared:", r_squared, "\n")
cat("Average Error:", average_error, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("Median Absolute Error:", mae, "\n")
cat("Root Mean Squared Error:", rmse, "\n")




#--------------- Further Analysis on the Wine Quality ---------------------

# Extract fitted values from the model and convert to a data frame
fitted_df <- data.frame(fitted = as.vector(fitted(model)))

# Fixed effect of price on rating points
price_effect <- ggplot(data.frame(price = wine_data$price, fitted = fitted_df$fitted), aes(x = price, y = fitted, color = fitted)) +
  geom_point(size = 3) +
  scale_color_viridis_c(option = "viridis", direction = 1, name = "Fitted Rating Points") +
  labs(title = "Effect of Price on Rating Points",
       x = "Price",
       y = "Fitted Rating Points")

# Fixed effect of price on rating points
price_summary <- wine_data %>%
  mutate(fitted = fitted(model)) %>%
  group_by(price_range = cut(price, breaks = quantile(price, probs = seq(0, 1, 0.25)), include.lowest = TRUE)) %>%
  summarize(
    min_price = min(price),
    max_price = max(price),
    mean_fitted = mean(fitted),
    sd_fitted = sd(fitted)
  )


# Fixed effect of richness on rating points
richness_effect <- ggplot(data.frame(Rich = wine_data$Rich, fitted = fitted_df$fitted), aes(x = factor(Rich), y = fitted, fill = factor(Rich))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "orange"), name = "Richness") +
  labs(title = "Effect of Richness on Rating Points",
       x = "Richness",
       y = "Fitted Rating Points")


# Fixed effect of richness on rating points
richness_summary <- wine_data %>%
  mutate(fitted = fitted(model), Rich = factor(Rich, levels = c(0, 1), labels = c("Not Rich", "Rich"))) %>%
  group_by(Rich) %>%
  summarize(
    mean_fitted = mean(fitted),
    sd_fitted = sd(fitted)
  )


# Fixed effect of fruitiness on rating points
fruitiness_effect <- ggplot(data.frame(Fruit = wine_data$Fruit, fitted = fitted_df$fitted), aes(x = factor(Fruit), y = fitted, fill = factor(Fruit))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgreen", "red"), name = "Fruitiness") +
  labs(title = "Effect of Fruitiness on Rating Points",
       x = "Fruitiness",
       y = "Fitted Rating Points")


# Fixed effect of fruitiness on rating points
fruitiness_summary <- wine_data %>%
  mutate(fitted = fitted(model), Fruit = factor(Fruit, levels = c(0, 1), labels = c("Not Fruity", "Fruity"))) %>%
  group_by(Fruit) %>%
  summarize(
    mean_fitted = mean(fitted),
    sd_fitted = sd(fitted)
  )


# Random intercepts for wine varieties
intercepts <- ranef(model)$variety[, , "Intercept"]
intercepts_plot <- ggplot(data.frame(variety = rownames(ranef(model)$variety), intercept = intercepts[,1]), aes(x = reorder(variety, intercept), y = intercept, fill = intercept)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "viridis", direction = 1, name = "Intercept") +
  labs(title = "Random Intercepts for Wine Varieties",
       x = "Wine Variety",
       y = "Intercept") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Random intercepts for wine varieties
intercepts_summary <- ranef(model)$variety[, , "Intercept"] %>%
  as.data.frame() %>%
  rownames_to_column("variety") %>%
  arrange(desc(Estimate)) %>%
  head(10)



# Print the summaries and plots
cat("Fixed Effect of Price on Rating Points:\n")
print(price_summary)
print(price_effect)

cat("\nFixed Effect of Richness on Rating Points:\n")
print(richness_summary)
print(richness_effect)

cat("\nFixed Effect of Fruitiness on Rating Points:\n")
print(fruitiness_summary)
print(fruitiness_effect)

cat("\nTop 10 Wine Varieties with Highest Random Intercepts:\n")
print(intercepts_summary)
print(intercepts_plot)






