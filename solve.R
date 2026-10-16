#!/usr/bin/env Rscript

# College admission classification pipeline
# Usage: Rscript solve.R [path/to/College_admission.csv] [output_directory]

args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args) >= 1) args[[1]] else "College_admission.csv"
output_dir <- if (length(args) >= 2) args[[2]] else "outputs"

if (!file.exists(data_path)) {
  stop(paste0("Dataset not found: '", data_path, "'.\n",
              "Pass its path explicitly, for example:\n",
              "  Rscript solve.R data/College_admission.csv"), call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

clean_name <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(x)))
  gsub("^_|_$", "", x)
}

classification_metrics <- function(actual, predicted) {
  actual <- factor(actual, levels = c("0", "1"))
  predicted <- factor(predicted, levels = c("0", "1"))
  cm <- table(actual = actual, predicted = predicted)
  tp <- cm["1", "1"]; tn <- cm["0", "0"]
  fp <- cm["0", "1"]; fn <- cm["1", "0"]
  divide <- function(a, b) if (b == 0) NA_real_ else a / b
  c(accuracy = divide(tp + tn, sum(cm)),
    precision = divide(tp, tp + fp), recall = divide(tp, tp + fn),
    specificity = divide(tn, tn + fp), f1 = divide(2 * tp, 2 * tp + fp + fn))
}

stratified_split <- function(target, train_fraction = 0.75, seed = 123) {
  set.seed(seed)
  ids <- unlist(lapply(split(seq_along(target), target), function(x) {
    sample(x, max(1, floor(length(x) * train_fraction)))
  }))
  sort(unique(ids))
}

raw_data <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
names(raw_data) <- clean_name(names(raw_data))
if ("gendermale" %in% names(raw_data)) names(raw_data)[names(raw_data) == "gendermale"] <- "gender_male"
if ("gender" %in% names(raw_data)) names(raw_data)[names(raw_data) == "gender"] <- "gender_male"

required <- c("admit", "gre", "gpa", "ses", "gender_male", "race", "rank")
missing_columns <- setdiff(required, names(raw_data))
if (length(missing_columns)) {
  stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")), call. = FALSE)
}

data <- raw_data[required]
for (column in required) data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
if (any(!is.na(data$admit) & !data$admit %in% c(0, 1))) {
  stop("'admit' must contain only 0 and 1.", call. = FALSE)
}

missing_before <- colSums(is.na(data))
data <- data[complete.cases(data), , drop = FALSE]
if (nrow(data) < 20) stop("At least 20 complete rows are required.", call. = FALSE)
if (length(unique(data$admit)) < 2) stop("'admit' must contain both classes.", call. = FALSE)

data$admit <- factor(data$admit, levels = c(0, 1))
for (column in c("ses", "gender_male", "race", "rank")) data[[column]] <- factor(data[[column]])

train_ids <- stratified_split(data$admit)
train <- data[train_ids, , drop = FALSE]
test <- data[-train_ids, , drop = FALSE]
if (!nrow(test)) stop("The test split is empty; provide more observations.", call. = FALSE)

results <- list()
predictions <- data.frame(actual = test$admit)

logistic_model <- glm(admit ~ ., data = train, family = binomial())
logistic_probability <- predict(logistic_model, newdata = test, type = "response")
logistic_prediction <- factor(ifelse(logistic_probability >= 0.5, 1, 0), levels = c(0, 1))
results$logistic_regression <- classification_metrics(test$admit, logistic_prediction)
predictions$logistic_probability <- logistic_probability
predictions$logistic_prediction <- logistic_prediction
saveRDS(logistic_model, file.path(output_dir, "logistic_model.rds"))

if (requireNamespace("rpart", quietly = TRUE)) {
  tree_model <- rpart::rpart(admit ~ ., data = train, method = "class")
  tree_prediction <- predict(tree_model, newdata = test, type = "class")
  results$decision_tree <- classification_metrics(test$admit, tree_prediction)
  predictions$tree_prediction <- tree_prediction
  saveRDS(tree_model, file.path(output_dir, "decision_tree_model.rds"))
} else message("Package 'rpart' is not installed; skipping the decision tree.")

if (requireNamespace("e1071", quietly = TRUE)) {
  svm_model <- e1071::svm(admit ~ ., data = train, kernel = "radial", scale = TRUE)
  svm_prediction <- predict(svm_model, newdata = test)
  results$svm <- classification_metrics(test$admit, svm_prediction)
  predictions$svm_prediction <- svm_prediction
  saveRDS(svm_model, file.path(output_dir, "svm_model.rds"))
} else message("Package 'e1071' is not installed; skipping SVM.")

metrics <- do.call(rbind, results)
metrics <- data.frame(model = rownames(metrics), metrics, row.names = NULL, check.names = FALSE)
metrics <- metrics[order(metrics$accuracy, decreasing = TRUE), ]
write.csv(metrics, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
write.csv(predictions, file.path(output_dir, "test_predictions.csv"), row.names = FALSE)

capture.output(list(input_rows = nrow(raw_data), complete_rows = nrow(data),
                    missing_values_by_column = missing_before,
                    training_rows = nrow(train), test_rows = nrow(test),
                    admission_rate = mean(as.numeric(as.character(data$admit))),
                    numeric_summary = summary(data[c("gre", "gpa")])),
               file = file.path(output_dir, "data_summary.txt"))

png(file.path(output_dir, "admission_by_gpa.png"), width = 1200, height = 800, res = 140)
boxplot(gpa ~ admit, data = data, col = c("#94A3B8", "#2563EB"),
        xlab = "Admission outcome (0 = no, 1 = yes)", ylab = "GPA",
        main = "GPA distribution by admission outcome")
dev.off()

gre_group <- cut(data$gre, breaks = c(-Inf, 440, 580, Inf),
                 labels = c("Low (<440)", "Medium (440-580)", "High (>580)"))
admission_by_gre <- aggregate(as.numeric(as.character(data$admit)),
                              by = list(gre_category = gre_group), FUN = mean)
names(admission_by_gre)[2] <- "admission_rate"
write.csv(admission_by_gre, file.path(output_dir, "admission_by_gre.csv"), row.names = FALSE)

png(file.path(output_dir, "admission_rate_by_gre.png"), width = 1200, height = 800, res = 140)
barplot(admission_by_gre$admission_rate * 100, names.arg = admission_by_gre$gre_category,
        col = c("#94A3B8", "#60A5FA", "#2563EB"), ylim = c(0, 100),
        ylab = "Admission rate (%)", xlab = "GRE category",
        main = "Admission rate by GRE category")
dev.off()

message("Analysis complete. Results written to: ", normalizePath(output_dir))
print(metrics, row.names = FALSE)
