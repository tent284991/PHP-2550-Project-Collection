
# Data 
data1 <- read.csv("/Users/evancheng/Downloads/project2.csv")
head(data1)

# Libraries
library(ggplot2)
library(gtsummary)
library(gt)
library(tidyr)
library(glmnet)
library(dplyr)
library(knitr)
library(caret)
library(pROC)
library(naniar)
  

# Table: Missing Data Bar Chart
gg_miss_var(data1) + 
  ggtitle("Missing Data by Variable") +
  theme_minimal()

# Summary Table  ----------------------------------------------------------
data1 <- read.csv("/Users/evancheng/Downloads/project2.csv")

# Data Transformation
data1$sex_ps <- factor(data1$sex_ps, levels = c("1", "2"), labels = c("Male", "Female"))

data1 <- data1 %>%
  mutate(
    TreatmentGroup = factor(
      paste0(
        ifelse(BA == 0, "ST", "BASC"),
        " + ",
        ifelse(Var == 0, "placebo", "varenicline")
      )
    )
  )


data1 <- data1 %>%
  mutate(
    Race = case_when(
      NHW == 1 ~ "Non-Hispanic White",
      Black == 1 ~ "Black/African American",
      Hisp == 1 ~ "Hispanic",
      TRUE ~ "Other"
    )
  ) %>%
  select(-NHW, -Black, -Hisp)

data1 <- data1 %>%
  mutate(
    inc = factor(inc, levels = c("1", "2", "3", "4", "5"), labels = c("Less than $20,000", "$20,000–35,000", "$35,001–50,000", "$50,001–75,000", "More than $75,000")),
    edu = factor(edu, levels = c("1", "2", "3", "4", "5"), labels = c("Grade school", "Some high school", "High school graduate or GED", "Some college/technical school", "College graduate")),
    antidepmed = factor(antidepmed, levels = c("0", "1"), labels = c("No", "Yes")),
    mde_curr = factor(mde_curr, levels = c("0", "1"), labels = c("Past", "Current")),
    Only.Menthol = factor(Only.Menthol, levels = c("0", "1"), labels = c("No", "Yes")),
    otherdiag = factor(otherdiag, levels = c("0", "1"), labels = c("No", "Yes")),
    abst = factor(abst, levels = c("0", "1"), labels = c("No", "Yes")),
    )

data1$readiness <- as.numeric(as.character(data1$readiness))

# Rename columns 
data1 <- data1 %>%
  rename(
    "Age" = age_ps,
    "Sex" = sex_ps,
    "Income" = inc,
    "Education Level" = edu,
    "FTCD Score at Baseline" = ftcd_score,
    "Smoking Within 5 Mins of Waking Up" = ftcd.5.mins,
    "BDI Score at Baseline" = bdi_score_w00,
    "Cigarettes Per Day at Baseline" = cpd_ps,
    "Cigarette Reward Value at Baseline" = crv_total_pq1,
    "Substitute Reinforcers Scale" = hedonsum_n_pq1,
    "Complementary Reinforcers Scale" = hedonsum_y_pq1,
    "Anhedonia Score" = shaps_score_pq1,
    "Other DSM-5 Diagnoses" = otherdiag,
    "Antidepressant Medication Use" = antidepmed,
    "Current vs Past MDD" = mde_curr,
    "Nicotine Metabolism Ratio" = NMR,
    "Exclusive Menthol Cigarette Use" = Only.Menthol,
    "Readiness to Quit Smoking" = readiness
  )

# Create the summary table
table_summary <- data1 %>%
  select(TreatmentGroup, Race, `Age`, `Sex`, `Income`, `Education Level`, `FTCD Score at Baseline`, `Smoking Within 5 Mins of Waking Up`, `BDI Score at Baseline`, `Cigarettes Per Day at Baseline`, `Cigarette Reward Value at Baseline`, `Substitute Reinforcers Scale`, `Complementary Reinforcers Scale`, `Anhedonia Score`, `Other DSM-5 Diagnoses`, `Antidepressant Medication Use`, `Current vs Past MDD`, `Nicotine Metabolism Ratio`, `Exclusive Menthol Cigarette Use`, `Readiness to Quit Smoking`) %>%
  tbl_summary(
    by = TreatmentGroup,  
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall(col_label = "Overall Sample", last = TRUE) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 1. Summary Table",
    subtitle = "Summary of baseline characteristics by treatment group"
  )

# Print the table
print(table_summary)

# Save table 
gt_table <- as_gt(table_summary)
gt::gtsave(gt_table, filename = "table_summary.pdf")

# Age and Race Plot -------------------------------------------------------
p <- ggplot(data1, aes(x = Age, fill = Race)) +
  geom_density(alpha = 0.5) +  
  scale_fill_brewer(palette = "Set1") +  
  ggtitle("Figure 1. Age Distribution by Race") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal() +
  annotate("text", x = Inf, y = 0, label = "Figure 1. Explanation of the figure", hjust = 0, vjust = 1, size = 3.5, color = "black")

print(p)




# Creating the box plot
data <- read.csv("/Users/evancheng/Downloads/project2.csv")
data <- data %>%
  mutate(
    BA = factor(BA, labels = c("Standard Treatment", "Behavioral Activation")),
    Var = factor(ifelse(Var == 0, "Placebo", "Medication"))
  )


# Treatment Group vs. BDI Score
p1 <- ggplot(data, aes(x = BA, y = bdi_score_w00, fill = BA)) +
  geom_boxplot() +
  labs(title = "BDI Scores by Treatment Group",
       x = "Treatment Group",
       y = "BDI Score") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")  # Hide legend for a cleaner look

# Placebo vs. Medication vs. BDI Score
p2 <- ggplot(data, aes(x = Var, y = bdi_score_w00, fill = Var)) +
  geom_boxplot() +
  labs(title = "BDI Scores by Medication vs. Placebo",
       x = "Group",
       y = "BDI Score") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")  # Hide legend for a cleaner look

library(patchwork)
combined_plot <- p1 + p2 + plot_layout(ncol = 2)
print(combined_plot)

# Model Selection -  Lasso and Ridge ----------------------------------
# Data
data <- read.csv("/Users/evancheng/Downloads/project2.csv")

# Select baseline variables as predictors
predictors <- data %>%
  select(age_ps, sex_ps, NHW, Black ,Hisp, inc, edu, ftcd_score, ftcd.5.mins, bdi_score_w00,
         cpd_ps, crv_total_pq1, hedonsum_n_pq1, hedonsum_y_pq1,
         shaps_score_pq1, otherdiag, antidepmed, mde_curr, NMR, Only.Menthol, readiness)

# As Factor categorical variables
categorical_vars <- c("sex_ps", "inc", "edu", "otherdiag", "antidepmed", "mde_curr", "Only.Menthol", "NHW", "Black" ,"Hisp")

for (var in categorical_vars) {
  if (var %in% names(predictors)) {
    predictors[[var]] <- factor(predictors[[var]])
  } else {
    warning(paste("Column", var, "not found in dataframe."))
  }
}

outcome <- as.factor(data$abst)

crucial_vars <- c("age_ps", "sex_ps", "NHW", "Black", "Hisp", "inc", "edu", "ftcd_score", "ftcd.5.mins", 
                  "bdi_score_w00", "cpd_ps", "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",
                  "shaps_score_pq1", "otherdiag", "antidepmed", "mde_curr", "NMR", "Only.Menthol", "readiness")

data_clean <- data  %>%
  filter(!if_any(all_of(crucial_vars), is.na))

# Set seed for reproducibility
set.seed(123) 
training_indices <- createDataPartition(data_clean$abst, p = 0.8, list = FALSE)
train_data <- data_clean[training_indices, ]
train_outcome <- data_clean$abst[training_indices]
test_data <- data_clean[-training_indices, ]
test_outcome <- data_clean$abst[-training_indices]

# Fit Lasso and Ridge models 
set.seed(123)
cv_lasso <- cv.glmnet(as.matrix(train_data), train_outcome, family = "binomial", alpha = 1)
cv_ridge <- cv.glmnet(as.matrix(train_data), train_outcome, family = "binomial", alpha = 0)

# Select the best lambda
lasso_best_lambda <- cv_lasso$lambda.min
ridge_best_lambda <- cv_ridge$lambda.min

# Fit models on training data
final_lasso <- glmnet(as.matrix(train_data), train_outcome, family = "binomial", alpha = 1, lambda = lasso_best_lambda)
final_ridge <- glmnet(as.matrix(train_data), train_outcome, family = "binomial", alpha = 0, lambda = ridge_best_lambda)

# Make predictions on test set
lasso_predictions <- predict(final_lasso, newx = as.matrix(test_data), type = "response")
ridge_predictions <- predict(final_ridge, newx = as.matrix(test_data), type = "response")

lasso_predictions <- as.numeric(lasso_predictions)
ridge_predictions <- as.numeric(ridge_predictions)

# Evaluate performance with ROC
lasso_roc <- roc(response = test_outcome, predictor = lasso_predictions)
ridge_roc <- roc(response = test_outcome, predictor = ridge_predictions)

# AUC 
lasso_auc <- auc(lasso_roc)
ridge_auc <- auc(ridge_roc)
print(lasso_auc)
print(ridge_auc)

# Model Selection - Logistic Model  ---------------------------------------------------------
# Formula for logistic regression
predictors_formula <- paste("abst ~", paste(names(train_data)[names(train_data) %in% predictors], collapse = " + "))

# Fit Logistic model on training data
glm_model <- glm(abst ~ ., data = train_data, family = binomial())

# Predictions 
glm_predictions <- predict(glm_model, newdata = test_data, type = "response")
glm_predictions <- as.numeric(glm_predictions)

# Evaluate performance using ROC 
glm_roc <- roc(response = test_outcome, predictor = glm_predictions)

# AUC value
glm_auc <- auc(glm_roc)
print(paste("Logistic Regression AUC:", glm_auc))

# Print AUC values for Lasso and Ridge
lasso_auc <- auc(lasso_roc)
ridge_auc <- auc(ridge_roc)
print(lasso_auc)
print(ridge_auc)

# Table 2. Lasso vs Ridge Performance Summary Table -----------------------
lasso_coef <- as.numeric(coef(final_lasso, s = "lambda.min")[-1])  
ridge_coef <- as.numeric(coef(final_ridge, s = "lambda.min")[-1])  

# For data frame of coefficients
coef_comparison <- data.frame(
  Variable = c("Age at Phone Interview", "Sex at Phone Interview", "Non-Hispanic White", "Black",
               "Hispanic", "Income Category", "Education Level", "FTCD Score at Baseline",
               "Smoking Within 5 Mins of Waking Up", "BDI Score at Baseline",
               "Cigarettes Per Day at Baseline", "Cigarette Reward Value at Baseline",
               "Substitute Reinforcers Scale", "Complementary Reinforcers Scale",
               "Anhedonia Score", "Other DSM-5 Diagnoses", "Antidepressant Medication Use",
               "Current vs Past MDD", "Nicotine Metabolism Ratio", "Exclusive Menthol Cigarette Use",
               "Readiness to Quit Smoking", "AUC Score"),
  Lasso = c(lasso_coef, lasso_auc),
  Ridge = c(ridge_coef, ridge_auc)
)


# Set row names
coef_comparison <- coef_comparison %>%
  mutate(Variable = as.character(Variable)) 

# GT table
coef_comparison_gt <- gt(coef_comparison, rowname_col = "Variable") %>%
  tab_header(
    title = "Model Comparison Table",
    subtitle = "Comparing Lasso and Ridge Model Coefficients and AUC Scores"
  ) %>%
  cols_label(
    Lasso = "Lasso Coefficients",
    Ridge = "Ridge Coefficients"
  ) %>%
  fmt_number(
    columns = c(Lasso, Ridge),
    decimals = 5
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "bottom", color = "black", weight = px(3))
    ),
    locations = cells_body(
      columns = everything(),
      rows = coef_comparison$Variable == "Readiness to Quit Smoking"
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "top", color = "black", weight = px(2))
    ),
    locations = cells_body(
      columns = everything(),
      rows = coef_comparison$Variable == "AUC Score"
    )
  ) %>%
  tab_footnote(
    footnote = "Note: Coefficients are shown with two decimal places. AUC scores are included for model performance evaluation.",
    locations = cells_body(
      columns = everything(),
      rows = coef_comparison$Variable == "AUC Score"
    )
  )

# Print
print(coef_comparison_gt)

# Model Performance Table -------------------------------------------------

# Extract logistic coefficients
glm_coef <- summary(glm_model)$coefficients
glm_coef_df <- as.data.frame(glm_coef)
glm_coef_df$Variable <- rownames(glm_coef_df)
rownames(glm_coef_df) <- NULL
colnames(glm_coef_df) <- c("Logistic_Coefficient", "Standard_Error", "Z_value", "P_value", "Variable")

# Extract lasso and ridge coefficients
lasso_coef <- as.numeric(coef(final_lasso, s = "lambda.min")[-1])  
ridge_coef <- as.numeric(coef(final_ridge, s = "lambda.min")[-1])  

# Convert coefficients to data frames
lasso_coef_df <- data.frame(Variable = names(train_data)[-1], Lasso_Coefficient = lasso_coef)
ridge_coef_df <- data.frame(Variable = names(train_data)[-1], Ridge_Coefficient = ridge_coef)

# Merge all coefficients
coef_comparison_df <- glm_coef_df %>%
  left_join(lasso_coef_df, by = "Variable") %>%
  left_join(ridge_coef_df, by = "Variable") %>%
  select(Variable, Logistic_Coefficient, Lasso_Coefficient, Ridge_Coefficient)

# Add AUC values
auc_row <- data.frame(Variable = "AUC", 
                      Logistic_Coefficient = glm_auc, 
                      Lasso_Coefficient = lasso_auc, 
                      Ridge_Coefficient = ridge_auc)

# Combine 
coef_comparison_df <- bind_rows(coef_comparison_df, auc_row)

# GT table
coef_comparison_table <- gt(coef_comparison_df) %>%
  tab_header(
    title = "Model Coefficients and AUC Scores",
    subtitle = "Comparing coefficients and AUC scores for Logistic, Lasso, and Ridge models"
  ) %>%
  cols_label(
    Variable = "Variable",
    Logistic_Coefficient = "Logistic Coefficient",
    Lasso_Coefficient = "Lasso Coefficient",
    Ridge_Coefficient = "Ridge Coefficient"
  ) %>%
  fmt_number(
    columns = c("Logistic_Coefficient", "Lasso_Coefficient", "Ridge_Coefficient"),
    decimals = 3
  ) %>%
  tab_footnote(
    footnote = "AUC values indicate the model's discrimination ability.",
    locations = cells_body(
      columns = "Logistic_Coefficient",
      rows = which(coef_comparison_df$Variable == "AUC")
    )
  )

# Print
print(coef_comparison_table)


