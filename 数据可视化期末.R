
set.seed(123)

pkgs <- c(
  "tidyverse", "skimr", "janitor",
  "GGally", "corrplot",
  "caret", "pROC",
  "broom", "car", "ResourceSelection",
  "randomForest"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

if (!dir.exists("plots")) dir.create("plots")
if (!dir.exists("outputs")) dir.create("outputs")

save_plot <- function(p, filename, w = 8, h = 5) {
  ggsave(filename = file.path("plots", filename), plot = p, width = w, height = h, dpi = 300)
}

# =========================================================
# 1) Load data
# =========================================================
# diabetes.csv from Kaggle: Pima Indians Diabetes Dataset
df_raw <- read.csv("diabetes.csv") %>% janitor::clean_names()

# Basic check
write.csv(head(df_raw, 20), "outputs/head20.csv", row.names = FALSE)
sink("outputs/structure.txt"); str(df_raw); sink()

# =========================================================
# 2) Data cleaning (medical zero -> NA) + imputation
# =========================================================
df <- df_raw

# Outcome -> factor
df$outcome <- factor(df$outcome, levels = c(0, 1), labels = c("NoDiabetes", "Diabetes"))

# Variables where 0 is physiologically implausible in this dataset context
zero_as_na <- c("glucose", "blood_pressure", "skin_thickness", "insulin", "bmi")

df <- df %>%
  mutate(across(all_of(zero_as_na), ~ ifelse(.x == 0, NA, .x)))

# Missingness summary (before impute)
miss_tbl <- df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))
write.csv(miss_tbl, "outputs/missing_before_impute.csv", row.names = FALSE)

# Impute with median (robust for skewness)
df <- df %>%
  mutate(across(all_of(zero_as_na), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

# Missingness summary (after impute)
miss_tbl2 <- df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))
write.csv(miss_tbl2, "outputs/missing_after_impute.csv", row.names = FALSE)

# =========================================================
# 3) Descriptive statistics + EDA visuals 
# =========================================================


# =========================================================
# Descriptive statistics 
# =========================================================

desc_tbl_console <- df %>%
  select(where(is.numeric)) %>%
  summarise(
    across(
      everything(),
      list(
        Mean = ~ mean(.x),
        SD   = ~ sd(.x),
        Min  = ~ min(.x),
        Max  = ~ max(.x)
      )
    )
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "value") %>%
  extract(name,
          into = c("Variable", "Stat"),
          regex = "^(.*)_([^_]*)$") %>%
  pivot_wider(names_from = "Stat", values_from = "value")

print(desc_tbl_console)

desc_tbl_console %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  knitr::kable(
    caption = "表 1：主要数值型变量的描述性统计量",
    align = "c"
  )


# 3.1 Outcome balance
p_outcome <- ggplot(df, aes(x = outcome, fill = outcome)) +
  geom_bar(alpha = 0.9) +
  labs(title = "Outcome Class Distribution", x = "Outcome", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
save_plot(p_outcome, "01_outcome_distribution.png", 7, 5)

# 3.2 Skim summary (nice descriptive stats)
skim_tbl <- skimr::skim(df)
capture.output(skim_tbl, file = "outputs/skim_summary.txt")

# 3.3 Group-wise means (by outcome)
mean_tbl <- df %>%
  group_by(outcome) %>%
  summarise(across(where(is.numeric), ~ mean(.x)), .groups = "drop")
write.csv(mean_tbl, "outputs/group_means.csv", row.names = FALSE)

# 3.4 Density plots for key continuous variables
num_vars <- df %>% select(where(is.numeric)) %>% names()

for (v in num_vars) {
  p <- ggplot(df, aes_string(x = v, fill = "outcome")) +
    geom_density(alpha = 0.35) +
    labs(title = paste("Density:", v, "by Outcome"), x = v, y = "Density") +
    theme_minimal()
  save_plot(p, paste0("dens_", v, ".png"), 7.5, 5)
}

# 3.5 Boxplots (Outcome comparison)
for (v in num_vars) {
  p <- ggplot(df, aes_string(x = "outcome", y = v, fill = "outcome")) +
    geom_boxplot(alpha = 0.75, outlier.alpha = 0.4) +
    labs(title = paste("Boxplot:", v, "by Outcome"), x = "Outcome", y = v) +
    theme_minimal() +
    theme(legend.position = "none")
  save_plot(p, paste0("box_", v, ".png"), 7, 5)
}

# 3.6 Scatter: Glucose vs BMI with outcome color (example of EDA guiding model)
if (all(c("glucose", "bmi") %in% names(df))) {
  p_g_b <- ggplot(df, aes(x = glucose, y = bmi, color = outcome)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "Glucose vs BMI (LOESS trend)", x = "Glucose", y = "BMI") +
    theme_minimal()
  save_plot(p_g_b, "02_glucose_vs_bmi.png", 7, 5)
}

# 3.7 Correlation heatmap for numeric vars
corr_mat <- cor(df %>% select(where(is.numeric)))
png("plots/03_corrplot_numeric.png", width = 1100, height = 900, res = 150)
corrplot::corrplot(corr_mat, method = "color", type = "upper", tl.cex = 0.8)
dev.off()

# 3.8 Pair plot (may be heavy; select a subset to keep readable)
subset_vars <- intersect(c("pregnancies", "glucose", "bmi", "age", "insulin"), names(df))
if (length(subset_vars) >= 3) {
  p_pairs <- GGally::ggpairs(df, columns = subset_vars, aes(color = outcome, alpha = 0.4))
  ggsave("plots/04_pairs_subset.png", p_pairs, width = 10, height = 8, dpi = 250)
}

# =========================================================
# 4) Train/Test split
# =========================================================
train_idx <- caret::createDataPartition(df$outcome, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]

write.csv(dim(train), "outputs/train_dim.csv", row.names = FALSE)
write.csv(dim(test),  "outputs/test_dim.csv", row.names = FALSE)

# =========================================================
# 5) Logistic Regression (with diagnostics)
# =========================================================
# Model formula: use all predictors (.)
logit <- glm(outcome ~ ., data = train, family = binomial)

# 提取回归系数
coef_est <- coef(logit)

# OR
OR <- exp(coef_est)

# 95% CI（基于 Wald 方法）
CI <- exp(confint.default(logit))

# 合并成一个表
OR_table <- data.frame(
  Variable = names(OR),
  OR = round(OR, 3),
  CI_lower = round(CI[,1], 3),
  CI_upper = round(CI[,2], 3)
)

OR_table

# Coefficients table
logit_tidy <- broom::tidy(logit, conf.int = TRUE, exponentiate = TRUE)
# exponentiate=TRUE gives Odds Ratios (OR)
write.csv(logit_tidy, "outputs/logit_odds_ratios.csv", row.names = FALSE)

# Multicollinearity check (VIF) — high VIF suggests collinearity
# Note: car::vif needs numeric outcome; for glm it's okay.
vif_tbl <- data.frame(variable = names(car::vif(logit)), vif = as.numeric(car::vif(logit)))
write.csv(vif_tbl, "outputs/logit_vif.csv", row.names = FALSE)

# Predict probabilities
logit_prob <- predict(logit, newdata = test, type = "response")
logit_pred <- ifelse(logit_prob >= 0.5, "Diabetes", "NoDiabetes") %>%
  factor(levels = levels(test$outcome))

# Confusion matrix
cm_logit <- caret::confusionMatrix(logit_pred, test$outcome)
capture.output(cm_logit, file = "outputs/confusion_logit.txt")

# ROC & AUC
roc_logit <- pROC::roc(test$outcome, logit_prob, levels = c("NoDiabetes", "Diabetes"), direction = "<")
auc_logit <- pROC::auc(roc_logit)
write.csv(data.frame(model="Logistic", auc=as.numeric(auc_logit)), "outputs/auc_logit.csv", row.names = FALSE)

p_roc_logit <- ggplot() +
  geom_line(aes(x = 1 - roc_logit$specificities, y = roc_logit$sensitivities)) +
  geom_abline(linetype = "dashed") +
  labs(title = paste0("ROC - Logistic Regression (AUC=", round(auc_logit, 3), ")"),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()
save_plot(p_roc_logit, "05_roc_logistic.png", 7, 5)

# Calibration plot (binned)
cal_df <- data.frame(prob = logit_prob, outcome = test$outcome) %>%
  mutate(y = ifelse(outcome == "Diabetes", 1, 0),
         bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(mean_prob = mean(prob), obs_rate = mean(y), .groups="drop")

p_cal <- ggplot(cal_df, aes(x = mean_prob, y = obs_rate)) +
  geom_point() +
  geom_abline(linetype = "dashed") +
  labs(title = "Calibration (Logistic Regression, 10 bins)",
       x = "Mean predicted probability", y = "Observed event rate") +
  theme_minimal()
save_plot(p_cal, "06_calibration_logistic.png", 7, 5)

# Hosmer-Lemeshow test (goodness-of-fit)
# Needs numeric y
hl <- ResourceSelection::hoslem.test(ifelse(test$outcome == "Diabetes", 1, 0), logit_prob, g = 10)
capture.output(hl, file = "outputs/hosmer_lemeshow.txt")

# =========================================================
# 6) Random Forest (as extra model, good for 90+)
# =========================================================
set.seed(123)
rf <- randomForest(outcome ~ ., data = train, ntree = 500, importance = TRUE)

capture.output(rf, file = "outputs/rf_model_summary.txt")

# Variable importance plot (built-in)
png("plots/07_rf_var_importance.png", width = 1100, height = 800, res = 150)
varImpPlot(rf, main = "Random Forest Variable Importance")
dev.off()

# Predict probabilities + class
rf_prob <- predict(rf, newdata = test, type = "prob")[, "Diabetes"]
rf_pred <- predict(rf, newdata = test, type = "response")

cm_rf <- caret::confusionMatrix(rf_pred, test$outcome)
capture.output(cm_rf, file = "outputs/confusion_rf.txt")

roc_rf <- pROC::roc(test$outcome, rf_prob, levels = c("NoDiabetes", "Diabetes"), direction = "<")
auc_rf <- pROC::auc(roc_rf)
write.csv(data.frame(model="RandomForest", auc=as.numeric(auc_rf)), "outputs/auc_rf.csv", row.names = FALSE)

p_roc_both <- ggplot() +
  geom_line(aes(x = 1 - roc_logit$specificities, y = roc_logit$sensitivities), linetype = 1) +
  geom_line(aes(x = 1 - roc_rf$specificities, y = roc_rf$sensitivities), linetype = 2) +
  geom_abline(linetype = "dashed") +
  labs(title = paste0("ROC Comparison (Logit AUC=", round(auc_logit,3),
                      ", RF AUC=", round(auc_rf,3), ")"),
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()
save_plot(p_roc_both, "08_roc_comparison.png", 7, 5)

# =========================================================
# 7) Simple model comparison table (for论文结果表)
# =========================================================
metrics_extract <- function(cm_obj) {
  acc <- cm_obj$overall["Accuracy"]
  sens <- cm_obj$byClass["Sensitivity"]
  spec <- cm_obj$byClass["Specificity"]
  data.frame(
    Accuracy = as.numeric(acc),
    Sensitivity = as.numeric(sens),
    Specificity = as.numeric(spec)
  )
}

tab <- bind_cols(
  data.frame(Model = "Logistic Regression", AUC = as.numeric(auc_logit)),
  metrics_extract(cm_logit)
) %>%
  bind_rows(
    bind_cols(
      data.frame(Model = "Random Forest", AUC = as.numeric(auc_rf)),
      metrics_extract(cm_rf)
    )
  )

write.csv(tab, "outputs/model_comparison.csv", row.names = FALSE)

# =========================================================
# 8) Export a "report-friendly" note
# =========================================================
cat(
  "DONE!\n",
  "- Plots saved in: /plots\n",
  "- Tables & outputs saved in: /outputs\n",
  "- Key files:\n",
  "  outputs/logit_odds_ratios.csv (OR + 95% CI)\n",
  "  outputs/model_comparison.csv (AUC/Accuracy/Sens/Spec)\n",
  "  plots/08_roc_comparison.png (ROC compare)\n",
  file = "outputs/README.txt"
)


