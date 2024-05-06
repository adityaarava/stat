library(ggplot2)
library(car)
library(caret)
library(MASS)
library(dplyr)

data <- read.csv("C:\\Users\\arava\\OneDrive\\Desktop\\STAT515\\modified_obesity_data.csv")  

data <- na.omit(data)

data$Highcaloric_food <- as.numeric(data$Highcaloric_food == "yes")
data$Smoking_Habit <- as.numeric(data$Smoking_Habit == "yes")

lm_model <- lm(Weight ~ Highcaloric_food + Vegetable_intake + Smoking_Habit + Technology_dependency, data = data)

# Custom function to perform stepwise selection based on Adjusted R-squared
stepwise_adj_r2 <- function(model, data) {
  current_adj_r2 <- summary(model)$adj.r.squared
  predictors <- names(coef(model))
  best_model <- model
  improvement <- TRUE
  
  while (improvement) {
    improvement <- FALSE
    best_adj_r2 <- current_adj_r2
    
    for (predictor in predictors[-1]) {  # Exclude intercept
      formula <- as.formula(paste("Weight ~", paste(setdiff(predictors[-1], predictor), collapse = " + ")))
      candidate_model <- lm(formula, data = data)
      candidate_adj_r2 <- summary(candidate_model)$adj.r.squared
      
      if (candidate_adj_r2 > best_adj_r2) {
        best_adj_r2 <- candidate_adj_r2
        best_model <- candidate_model
        improvement <- TRUE
      }
    }
    
    if (improvement) {
      current_adj_r2 <- best_adj_r2
      predictors <- names(coef(best_model))
    }
  }
  
  return(best_model)
}

# Perform stepwise selection
final_model <- stepwise_adj_r2(lm_model, data)

# Print the summary of the final model
summary_final_model <- summary(final_model)
print(summary_final_model)

# Coefficient plot
coef_plot_adj_r2 <- ggplot(data = as.data.frame(coef(summary(final_model))), aes(x = rownames(coef(summary(final_model))), y = Estimate)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Coefficients of Predictors (Adjusted R-Squared Criterion)", x = "Predictors", y = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(coef_plot_adj_r2)

# Residual plot
residual_plot_adj_r2 <- ggplot(data, aes(x = fitted(final_model), y = residuals(final_model))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot (Adjusted R-Squared Criterion)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
print(residual_plot_adj_r2)

# Variable importance plot
var_importance_adj_r2 <- ggplot(data = as.data.frame(varImp(final_model)), aes(x = rownames(varImp(final_model)), y = Overall)) +
  geom_bar(stat = "identity", fill = "cornsilk", color = "black") +
  labs(title = "Variable Importance", x = "Predictors", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(var_importance_adj_r2)
