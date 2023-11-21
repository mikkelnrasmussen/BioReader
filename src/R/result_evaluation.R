library("tidyverse")
library("tidymodels")
library("here")
library("caret")
library("gt")

####################################################################
################# Class classification - all data ##################
####################################################################

# Read in the cross-validation results for level 1 classifier
tune_results <- read_rds(
  "results/tuning_results_class.rds"
)

calculate_summary_statistics <- function(tune_results) {
  # Function to calculate and extract metrics
  calculate_and_extract_metrics <- function(predictions) {
    # Split the data by fold
    predictions_by_fold <- split(predictions, predictions$id)

    # Function to calculate confusion matrix metrics for one fold
    calculate_metrics <- function(data) {
      conf_mat <- confusionMatrix(data$.pred_class, data$target)
      return(conf_mat)
    }

    # Calculate metrics for each fold
    metrics_list <- lapply(predictions_by_fold, calculate_metrics)

    # Function to extract relevant metrics from confusion matrix
    extract_metrics <- function(conf_mat, fold) {
      tmp_tibble <- conf_mat$byClass %>%
        as.data.frame() %>%
        rownames_to_column("class") %>%
        as_tibble() %>%
        mutate(
          class = str_remove(class, "Class: "),
          fold = fold
        ) # Add fold information
      return(tmp_tibble)
    }

    # Fold names (assuming they are like 'Fold1', 'Fold2', etc.)
    fold_names <- names(metrics_list)

    # Extract and combine the metrics across folds
    average_metrics <- Map(extract_metrics, metrics_list, fold_names) %>%
      bind_rows() %>%
      group_by(class) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

    return(average_metrics)
  }

  # Function to calculate ROC AUC per class for one fold
  calculate_auc_per_class <- function(data) {
    data %>%
      select(target, starts_with(".pred_") & where(is.numeric)) %>%
      pivot_longer(-target, names_to = "class", values_to = "prob") %>%
      mutate(class = str_remove(class, "^\\.pred_")) %>%
      group_nest(class) %>%
      mutate(
        auc = map2_dbl(class, data, ~ {
          roc_auc_vec(factor(.y$target == .x), .y$prob, event_level = "second")
        })
      ) %>%
      select(-data) %>%
      unnest(cols = c(auc))
  }

  # Select the best model based on accuracy
  best_model <- tune_results |>
    select_best("accuracy") |>
    pull(.config)

  # Extract the overall metrics for the best model
  best_metrics <- tune_results %>%
    collect_metrics() %>%
    filter(.config == best_model)

  # Collect predictions for the best model
  predictions <- tune_results |>
    collect_predictions() |>
    filter(.config == best_model)

  # Calculate AUC per class for each fold
  auc_results_by_fold <- map(
    split(
      predictions, predictions$id
    ),
    calculate_auc_per_class
  )

  # Combine and average the AUC scores across folds
  average_auc_per_class <- bind_rows(auc_results_by_fold, .id = "fold") %>%
    group_by(class) %>%
    summarise(mean_auc = mean(auc, na.rm = TRUE))

  train_samples_per_category <- tune_results$splits[[1]] |>
    analysis() |>
    group_by(target) |>
    dplyr::summarise(n = n())

  # Number of validation observations in each fold
  val_samples_per_category <- tune_results$splits[[1]] |>
    assessment() |>
    group_by(target) |>
    dplyr::summarise(n = n())

  # We first bind them into one tibble
  combined_counts <- bind_rows(
    train_samples_per_category,
    val_samples_per_category
  )

  # Now we group by the 'target' column and summarise to get the total counts
  total_samples_per_category <- combined_counts %>%
    group_by(target) %>%
    dplyr::summarise(total = sum(n))

  # Calculate and extract metrics
  average_metrics <- calculate_and_extract_metrics(predictions)

  # Bind all metrics together
  final_results <- bind_cols(
    total_samples_per_category,
    select(average_metrics, -class),
    select(average_auc_per_class, mean_auc)
  )

  return(final_results)
}

# Example usage:
final_summary <- calculate_summary_statistics(tune_results)

final_table <- final_summary %>%
  select(target, total, mean_auc, `Balanced Accuracy`) %>%
  gt() %>%
  tab_header(
    title = "Classification Performance Summary"
  ) %>%
  cols_label(
    target = "Level 1 category",
    total = "Curatable abstracts",
    mean_auc = "AUC individual category",
    `Balanced Accuracy` = "Category prediction accuracy (%)"
  ) %>%
  fmt_number(
    columns = c("mean_auc", "Balanced Accuracy"),
    decimals = 3
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D3D3D3"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c("mean_auc", "Balanced Accuracy"),
      rows = target == "Total"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#14bf1d"),
    locations = cells_body(
      columns = "Balanced Accuracy",
      rows = !is.na(`Balanced Accuracy`)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D3D3D3"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )

# Print the gt table
print(final_table)
