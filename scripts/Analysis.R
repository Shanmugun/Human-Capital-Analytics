# ============================
# Human Capital Analytics Project
# ============================

# --- Packages
namesOfPackages <- c(
  "dplyr","ggplot2","tidyr","corrplot","Boruta","caret",
  "e1071","cluster","rpart","rpart.plot","pROC","gridExtra","reshape2"
)

check_install_and_load.package <- function(nameOfPackage) {
  if (system.file(package = nameOfPackage) == '') {
    install.packages(nameOfPackage)
  }
  suppressPackageStartupMessages(
    lapply(nameOfPackage, library, character.only = TRUE)
  )
}
for (package_name in namesOfPackages) check_install_and_load.package(package_name)
rm(package_name, check_install_and_load.package)

# --- Figure utilities
if (!dir.exists("../figures")) dir.create("../figures")

# replaces ONLY spaces with underscores
as_figfile <- function(title, ext = "png") {
  paste0("../figures/", gsub(" ", "_", title), ".", ext)
}
save_gg <- function(p, title, w = 8, h = 6, dpi = 300) {
  ggplot2::ggsave(filename = as_figfile(title), plot = p, width = w, height = h, dpi = dpi)
}
save_device_plot <- function(title, code_expr, w = 8, h = 6, res = 300, units = "in") {
  png(filename = as_figfile(title), width = w, height = h, res = res, units = units)
  on.exit(dev.off(), add = TRUE)
  eval(substitute(code_expr), envir = parent.frame())
}

# --- Load data
data <- read.csv('../data/Employee.csv')

# --- Quick looks   
dim(data); names(data); head(data); tail(data); str(data)

# --- Rename
data <- dplyr::rename(data, work_accident = Work_accident, department = sales)

# --- Missing values / zeros   
na_counts   <- colSums(is.na(data))
count_zeros <- function(x) sum(x == 0, na.rm = TRUE)
zero_counts <- sapply(data, count_zeros)
rm(count_zeros)

# --- Factor conversions
data$work_accident         <- factor(data$work_accident, levels = c(0,1), labels = c("no","yes"))
data$left                  <- factor(data$left, levels = c(0,1), labels = c("no","yes"))
data$promotion_last_5years <- factor(data$promotion_last_5years, levels = c(0,1), labels = c("no","yes"))
data$department            <- factor(data$department)
data$salary                <- factor(data$salary, levels = c("low","medium","high"))

# --- Descriptive stats figure
sink("../figures/FIGURE_7_:_DESCRIPTIVE_STATISTICS_–_SUMMARY_OF_DATA.txt"); print(summary(data)); sink()

# --- Proportion of 'left' (bar)
prop_tab <- prop.table(table(data$left))*100
p_left_prop <- ggplot(
  data.frame(Left = names(prop_tab), Percentage = as.numeric(prop_tab)),
  aes(x = Left, y = Percentage, fill = Left)
) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), vjust = -0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "FIGURE 8 : PROPORTION DISTRIBUTION OF EMPLOYEE LEFT COLUMN", x = "Left", y = "Percentage")
save_gg(p_left_prop, "FIGURE 8 : PROPORTION DISTRIBUTION OF EMPLOYEE LEFT COLUMN")

# --- Boxplots (5 panels), save combined
p1 <- ggplot(data, aes(y = satisfaction_level)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Satisfaction Level", y = "", x = "")
p2 <- ggplot(data, aes(y = last_evaluation)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Last Evaluation", y = "", x = "")
p3 <- ggplot(data, aes(y = number_project)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Number of Projects", y = "", x = "")
p4 <- ggplot(data, aes(y = average_montly_hours)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Average Monthly Hours", y = "", x = "")
p5 <- ggplot(data, aes(y = time_spend_company)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Time Spent in Company", y = "", x = "")
save_device_plot(
  "FIGURE 8 : BOX PLOT TO CHECK FOR ANY OUTLIERS",
  gridExtra::grid.arrange(p1,p2,p3,p4,p5, nrow = 2, ncol = 3),
  w = 12, h = 8, res = 300, units = "in"
)
rm(p1,p2,p3,p4,p5)

# --- Department distribution
department_summary <- data %>% group_by(department) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(count)) %>%
  mutate(department = factor(department, levels = department[order(-count)]))
p_department <- ggplot(department_summary, aes(x = department, y = count, fill = department)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
            vjust = -0.5, color = "black") +
  labs(title = "FIGURE 9 : DEPARTMENT WISE DISTRIBUTION PLOT",
       x = "Department", y = "Count of Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "black"))
save_gg(p_department, "FIGURE 9 : DEPARTMENT WISE DISTRIBUTION PLOT")

# --- Salary distribution
salary_summary <- data %>% group_by(salary) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(count)) %>%
  mutate(salary = factor(salary, levels = salary[order(-count)]))
p_salary <- ggplot(salary_summary, aes(x = salary, y = count, fill = salary)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
            vjust = -0.5, color = "black") +
  labs(title = "FIGURE 10 : SALARY DISTRIBUTION PLOT",
       x = "Salary", y = "Count of Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "black"))
save_gg(p_salary, "FIGURE 10 : SALARY DISTRIBUTION PLOT")

# --- Turnover by salary (stacked %)
salary_left <- data %>%
  count(salary, left) %>%
  tidyr::pivot_wider(names_from = left, values_from = n, values_fill = list(n = 0)) %>%
  mutate(Total = `yes` + `no`) %>%
  mutate(across(c(`yes`, `no`), ~ .x / Total * 100, .names = "pct_{.col}")) %>%
  select(salary, `pct_yes`, `pct_no`) %>%
  arrange(factor(salary, levels = c("low","medium","high")))
salary_left_long <- salary_left %>%
  tidyr::pivot_longer(starts_with("pct_"), names_to = "Left", values_to = "Percentage") %>%
  mutate(Left = dplyr::recode(Left, pct_yes = "yes", pct_no = "no"))
p_turnover_salary <- ggplot(salary_left_long, aes(x = salary, y = Percentage, fill = Left)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "FIGURE 11 : EMPLOYEE TURNOVER BY SALARY (PERCENTAGE)",
       x = "Salary Level", y = "Percentage of Employees") +
  scale_fill_manual(values = c("yes" = "orange", "no" = "skyblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "black"))
save_gg(p_turnover_salary, "FIGURE 11 : EMPLOYEE TURNOVER BY SALARY (PERCENTAGE)")

# --- Turnover by work accident (stacked %)
accident_left <- data %>%
  count(work_accident, left) %>%
  tidyr::pivot_wider(names_from = left, values_from = n, values_fill = list(n = 0)) %>%
  mutate(Total = `yes` + `no`) %>%
  mutate(across(c(`yes`, `no`), ~ .x / Total * 100, .names = "pct_{.col}")) %>%
  select(work_accident, `pct_yes`, `pct_no`) %>%
  arrange(factor(work_accident, levels = c("no","yes")))
accident_left_long <- accident_left %>%
  tidyr::pivot_longer(starts_with("pct_"), names_to = "Left", values_to = "Percentage") %>%
  mutate(Left = dplyr::recode(Left, pct_yes = "yes", pct_no = "no"))
p_turnover_acc <- ggplot(accident_left_long, aes(x = work_accident, y = Percentage, fill = Left)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "FIGURE 12 : EMPLOYEE TURNOVER BY WORK ACCIDENT (PERCENTAGE)",
       x = "Work Accident", y = "Percentage of Employees") +
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +
  scale_fill_manual(values = c("yes" = "orange", "no" = "skyblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "black"))
save_gg(p_turnover_acc, "FIGURE 12 : EMPLOYEE TURNOVER BY WORK ACCIDENT (PERCENTAGE)")

# --- Turnover by promotion (stacked %)
promotion_left <- data %>%
  count(promotion_last_5years, left) %>%
  tidyr::pivot_wider(names_from = left, values_from = n, values_fill = list(n = 0)) %>%
  mutate(Total = `yes` + `no`) %>%
  mutate(across(c(`yes`, `no`), ~ .x / Total * 100, .names = "pct_{.col}")) %>%
  select(promotion_last_5years, `pct_yes`, `pct_no`) %>%
  arrange(factor(promotion_last_5years, levels = c("no","yes")))
promotion_left_long <- promotion_left %>%
  tidyr::pivot_longer(starts_with("pct_"), names_to = "Left", values_to = "Percentage") %>%
  mutate(Left = dplyr::recode(Left, pct_yes = "yes", pct_no = "no"))
p_turnover_promo <- ggplot(promotion_left_long, aes(x = promotion_last_5years, y = Percentage, fill = Left)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "FIGURE 13 : EMPLOYEE TURNOVER BY PROMOTION IN LAST 5 YEARS (PERCENTAGE)",
       x = "Promotion in Last 5 Years", y = "Percentage of Employees") +
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +
  scale_fill_manual(values = c("yes" = "orange", "no" = "skyblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
save_gg(p_turnover_promo, "FIGURE 13 : EMPLOYEE TURNOVER BY PROMOTION IN LAST 5 YEARS (PERCENTAGE)")

# --- Correlation heatmap (numeric predictors)
numeric_data <- dplyr::select_if(data, is.numeric)
cor_matrix   <- cor(numeric_data, use = "complete.obs")
melted_cor   <- reshape2::melt(cor_matrix)
p_cor_heat <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "lightgreen", high = "darkgreen",
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
  labs(title = "FIGURE 14 : CORRELATION AMONG PREDICTORS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid = element_blank()) +
  coord_fixed()
save_gg(p_cor_heat, "FIGURE 14 : CORRELATION AMONG PREDICTORS")
rm(numeric_data, cor_matrix, melted_cor)

# --- Satisfaction vs Turnover (boxplot)
p_sat_turn <- ggplot(data, aes(x = factor(left), y = satisfaction_level, fill = factor(left))) +
  geom_boxplot() +
  labs(title="FIGURE 15 : SATISFACTION LEVEL VS TURNOVER", x="Left (0 = No, 1 = Yes)", y="Satisfaction Level") +
  stat_summary(fun=mean, geom="text", aes(label=round(..y.., 2)), vjust=-0.1, color="black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_fill_manual(values = c("no"="lightgreen","yes"="skyblue"), name="Left")
save_gg(p_sat_turn, "FIGURE 15 : SATISFACTION LEVEL VS TURNOVER")

# --- Time spent vs turnover (grouped bar with labels)
summary_data <- data %>%
  group_by(time_spend_company, left) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count), percentage = count/total*100)
p_time_turn <- ggplot(summary_data, aes(x = factor(time_spend_company), y = count, fill = factor(left))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightgreen","skyblue"), name="Left", labels=c("No","Yes")) +
  labs(title="FIGURE 16 : TIME SPENT AT COMPANY VS TURNOVER", x="Time Spent at Company (years)", y="Number of Employees") +
  geom_text(aes(label=paste0(count, "\n (", round(percentage, 1), "%)")),
            position=position_dodge(width=0.9), vjust=-0.25, color="black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))
save_gg(p_time_turn, "FIGURE 16 : TIME SPENT AT COMPANY VS TURNOVER")

# --- Work accident vs turnover (grouped bar with labels)
summary_data2 <- data %>%
  group_by(work_accident, left) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count), percentage = count/total*100)
p_acc_turn <- ggplot(summary_data2, aes(x=factor(work_accident), y=count, fill=factor(left))) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("lightgreen","skyblue"), name="Left", labels=c("No","Yes")) +
  labs(title="FIGURE 17 : WORK ACCIDENT VS TURNOVER", x="Work Accident (0 = No, 1 = Yes)", y="Number of Employees") +
  geom_text(aes(label=paste0(count, "\n(", round(percentage, 1), "%)")),
            position=position_dodge(width=0.9), vjust=-0.25, color="black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", size = 1))
save_gg(p_acc_turn, "FIGURE 17 : WORK ACCIDENT VS TURNOVER")

# --- Predictor correlation with outcome (corrplot)
data$left <- as.numeric(data$left) - 1
numeric_data2 <- dplyr::select(data, satisfaction_level, last_evaluation, number_project,
                               average_montly_hours, time_spend_company, left)
cor_matrix2 <- cor(numeric_data2)
save_device_plot("FIGURE 18: CORRELATION PLOT OF PREDICTORS WITH OUTCOME(LEFT)",
                 {
                   corrplot::corrplot(cor_matrix2, method = "color", type = "upper",
                                      tl.col = "black", tl.srt = 45, addCoef.col = "black")
                   title("Correlation Plot of Predictors with 'Left'", line = 1.2, font.main = 2, cex.main = 1.5)
                 },
                 w = 8, h = 6, res = 300, units = "in"
)

# --- Boruta importance (no figure printed by default; skip)

# --- Partition
set.seed(2024)
trainIndex <- caret::createDataPartition(data$left, p = 0.6, list = FALSE)
trainData  <- data[trainIndex, ]
tempData   <- data[-trainIndex, ]
testIndex  <- caret::createDataPartition(tempData$left, p = 0.6, list = FALSE)
testData   <- tempData[testIndex, ]
validData  <- tempData[-testIndex, ]
rm(tempData,testIndex,trainIndex)

trainData$left <- factor(trainData$left, levels = c(0,1), labels = c("no","yes"))
testData$left  <- factor(testData$left,  levels = c(0,1), labels = c("no","yes"))
validData$left <- factor(validData$left, levels = c(0,1), labels = c("no","yes"))

# --- Logistic regression + threshold tuning (plot)
logisticModel <- glm(left ~ ., data = trainData, family = binomial)
lpredicted    <- predict(logisticModel, testData, type = "response")

thresholds  <- seq(0, 1, by = 0.01)
performance <- data.frame(threshold = thresholds, accuracy = NA, precision = NA, recall = NA, F1 = NA)

for (i in seq_along(thresholds)) {
  th <- thresholds[i]
  predClass <- ifelse(lpredicted >= th, "yes", "no")
  predClass <- factor(predClass, levels = c("no","yes"))
  cm <- caret::confusionMatrix(predClass, testData$left, positive = "yes")
  performance$accuracy[i]  <- cm$overall["Accuracy"]
  performance$precision[i] <- cm$byClass["Pos Pred Value"]
  performance$recall[i]    <- cm$byClass["Sensitivity"]
  performance$F1[i]        <- cm$byClass["F1"]
}

p_perf <- ggplot(performance, aes(x = threshold)) +
  geom_line(aes(y = accuracy,  color = "Accuracy")) +
  geom_line(aes(y = precision, color = "Precision")) +
  geom_line(aes(y = recall,    color = "Recall/Sensitivity")) +
  geom_line(aes(y = F1,        color = "F1 Score")) +
  labs(title = "FIGURE 23 : PERFORMANCE METRICS VS THRESHOLD",
       y = "Metric Value", x = "Threshold") +
  theme_minimal()
save_gg(p_perf, "FIGURE 23 : PERFORMANCE METRICS VS THRESHOLD")

# apply chosen threshold 0.25  
threshold <- 0.25
lpredClass <- ifelse(lpredicted >= threshold, "1", "0")
lpredClass <- factor(lpredClass, levels = c(0,1), labels = c("no","yes"))
LCM <- caret::confusionMatrix(lpredClass, testData$left, positive = "yes")
sink("../figures/FIGURE_24_:_LOGISTIC_MODEL_EVALUATION.txt"); print(LCM); sink()
rm(LCM)

# --- Classification Tree + plot
treeModel <- rpart::rpart(left ~ ., data = trainData, method = "class", minbucket = 2, maxdepth = 5)
save_device_plot("FIGURE 25 : CLASSIFICATION TREE MODEL",
                 rpart.plot::rpart.plot(treeModel, type = 3, extra = 104, fallen.leaves = TRUE,
                                        main = "Decision Tree for Employee Turnover"))

predictions <- predict(treeModel, testData, type = "class")
TCM <- caret::confusionMatrix(predictions, testData$left, positive = "yes")
sink("../figures/FIGURE_26_:_CLASSIFICATION_TREE_EVALUATION.txt"); print(TCM); sink()
rm(predictions, TCM)

# --- Predictor importance from tree (barplot)
imp_df <- data.frame(Variable = names(treeModel$variable.importance),
                     Importance = as.numeric(treeModel$variable.importance))
imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
p_imp <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "FIGURE 27 : PREDICTOR IMPORTANCE FROM CLASSIFICATION TREE",
       x = "Variable", y = "Importance") +
  theme_minimal()
save_gg(p_imp, "FIGURE 27 : PREDICTOR IMPORTANCE FROM CLASSIFICATION TREE")

# --- Naive Bayes  
nbModel <- e1071::naiveBayes(left ~ ., data = trainData)
nbPred  <- predict(nbModel, testData)
NBCM    <- caret::confusionMatrix(nbPred, testData$left, positive = "yes")
sink("../figures/FIGURE_29_:_NAIVE_BAYES_MODEL_EVALUATION.txt"); print(NBCM); sink()
rm(nbPred, NBCM)

# --- Validate tree  
pred_v <- predict(treeModel, validData, type = "class")
TCM_v  <- caret::confusionMatrix(pred_v, validData$left, positive = "yes")
sink("../figures/FIGURE_30_:_CLASSIFICATION_TREE_VALIDATION.txt"); print(TCM_v); sink()
rm(pred_v, TCM_v)

# --- K-means clustering (only for left == 1)
subset_data <- subset(data, left == 1)
subset_data <- subset_data[, !(names(subset_data) == "left")]
subset_data$department <- ifelse(subset_data$department %in% "management",
                                 as.character(subset_data$department), "Others")
subset_data$department <- factor(subset_data$department, levels = c("management","Others"))

data_dummy <- as.data.frame(model.matrix(~ . - 1, data = subset_data))

normalize <- function(x) (x - min(x)) / (max(x) - min(x))
normalized_data <- as.data.frame(lapply(data_dummy, normalize))

set.seed(123)
sil_widths <- numeric(10)
for (k in 2:10) {
  km <- kmeans(normalized_data, centers = k, nstart = 25)
  sil_widths[k] <- mean(cluster::silhouette(km$cluster, dist(normalized_data))[,3])
}
# FIGURE 33 : Silhouette width vs k
save_device_plot("FIGURE 33 : KMEAN CLUSTERING TUNING K-MEAN PARAMETER",
                 plot(2:10, sil_widths[2:10], type = "b", pch = 19,
                      xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width",
                      main = "Silhouette Width vs. Number of Clusters"))

optimal_k <- 4
kmeans_result <- kmeans(normalized_data, centers = optimal_k, nstart = 25)
cluster_model <- kmeans_result
dis <- dist(normalized_data)
silhouette_plot <- cluster::silhouette(cluster_model$cluster, dis)

# centers table saved as CSV
write.csv(cluster_model$centers, "../figures/FIGURE_33_:_CLUSTER_CENTERS.csv", row.names = TRUE)

# FIGURE 33 : Profile plot (bar)
save_device_plot("FIGURE 33 : PROFILE PLOT OF ALL 4 CLUSTERS",
                 {
                   myColors <- RColorBrewer::brewer.pal(n = ncol(normalized_data), name = "Set3")
                   barplot(t(cluster_model$centers), beside = TRUE,
                           xlab = "Clusters", ylab = "Values", col = myColors,
                           main = "Clusters Profile Plot")
                   legend("topright", ncol = 2, legend = colnames(normalized_data), fill = myColors, cex = 0.8)
                 },
                 w = 12, h = 7, res = 300, units = "in"
)

# FIGURE 34 : Silhouette plot
save_device_plot("FIGURE 34 : SILHOUETTE PLOT FOR K =4",
                 plot(silhouette_plot, col = 1:length(cluster_model$size), border = NA),
                 w = 8, h = 6, res = 300, units = "in"
)

# clean up devices
if (!is.null(dev.list())) dev.off()

