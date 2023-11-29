#! /usr/bin/RScript
suppressMessages(library("optparse"))
suppressMessages(library("here"))
suppressMessages(library("dplyr"))
suppressMessages(library("stringr"))
suppressMessages(library("readr"))

option_list <- list(
  make_option(
    c("-i", "--input"),
    type = "character",
    help = "The file containing the files that should be classified"
  ),
  make_option(
    c("-o", "--output"),
    type = "character",
    default = here("results"), # Should be changed
    help = "The directory where the results will be saved [default: %default]"
  ),
  make_option(
    c("-m", "--model_dir"),
    type = "character",
    default = here("results"),
    help = "The directory where the models are located [default: %default]"
  ),
  make_option(
    c("-v", "--verbose"),
    action = "store_true",
    default = FALSE,
    help = "Whether the script should print information [default: %default]"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Display parameters passed to the script
if (opt$verbose) {
  cat(paste("Input file:", opt$input), fill = TRUE)
  cat(paste("Output directory:", opt$output), fill = TRUE)
  cat(paste("Location of classifier:", opt$model_dir), fill = TRUE)
}


# Helper function to load and predict with a model
load_and_predict <- function(model_dir, category, new_data_row) {
  # Construct the model filename
  model_name <- paste0("model_fitted_on_entire_data_", category, ".rds")
  filename <- list.files(model_dir, full.names = TRUE) |> str_subset(model_name)

  # Check if the model is already loaded
  model_variable <- paste0(category, "_model")
  if (!exists(model_variable, envir = globalenv())) {
    # Load the model
    cat("R: Load model for category", category, "...", fill = TRUE)
    assign(model_variable, readRDS(filename), envir = globalenv())
  }

  # Predict using the model
  model <- get(model_variable, envir = globalenv())
  return(predict(model, new_data_row)$.pred_class)
}

# Main function for predicting the category of articles
category_predict <- function(model_dir, new_data) {
  # Read in the class labels
  cat("R: Load class labels...", fill = TRUE)
  class_info <- read_csv(here("data/class_info.csv"))

  # Load the level 1 classifier
  cat("R: Load level 1 model...", fill = TRUE)
  filename_model_1 <- model_dir |>
    list.files(full.names = TRUE) |>
    str_subset("model_fitted_on_entire_data_class_with_HIV.rds")

  # Load level 1 model - check if the model is already loaded
  if (!exists("model_level_1", envir = globalenv())) {
    model_level_1 <- readRDS(filename_model_1)
    cat("R: Level 1 model loaded!", fill = TRUE)
    assign("model_level_1", model_level_1, envir = globalenv())
  }

  # Classify articles according to level 1 model
  cat("R: Classifying articles according to level 1 model...", fill = TRUE)
  get("model_level_1", envir = globalenv())
  new_data$level_1 <- predict(model_level_1, new_data)$.pred_class

  # Classify articles according to level 2 models
  cat("R: Classifying articles according to level 2 models...")

  # Add placeholder for level 2 classification
  new_data$level_2 <- NA

  # Define the classes
  classes <- class_info$class |>
    unique() |>
    sort()

  # Loop through each article
  for (i in seq_len(nrow(new_data))) {
    level_1_category <- new_data[i, "level_1"] |> pull(level_1)
    if (level_1_category %in% classes) {
      # If level 1 class is the same as level 2 class, assign level 1 class to level 2 class
      if (level_1_category %in% c("Cancer", "HIV", "Transplant")) {
        new_data[i, "level_2"] <- level_1_category
      } else {
        new_data[i, "level_2"] <- load_and_predict(model_dir, level_1_category, new_data[i, ])
      }
    }
  }

  # Classify articles according to level 3 models
  cat("R: Classifying articles according to level 3 models...")

  # Add placeholder for level 3 classification
  new_data$level_3 <- NA

  # Define the categories
  categories <- class_info$category |>
    unique() |>
    sort()

  # Loop through each article
  for (i in seq_len(nrow(new_data))) {
    level_2_category <- new_data[i, "level_2"] |> pull(level_2)
    if (level_2_category %in% categories) {
      new_data[i, "level_3"] <- load_and_predict(model_dir, level_2_category, new_data[i, ])
    }
  }

  return(new_data)
}

# Gather data and make prediction.
#    data_path: str - gives path to csv file with articles to classify
#    model_path: str - gives path to the directory where the models are located
#    output_dir: str - gives path to the directory where the results are saved
classify_data <- function(data_path, model_dir, output_dir) {
  # Read the new articles from csv file
  cat("R: Load the articles...", fill = TRUE)
  new_data <- read_csv(data_path) |>
    rename(pmid = PubMed_ID, abstract = Abstract) |>
    select(pmid, abstract) |>
    slice(1:10)

  # Perform prediction
  cat("R: Classifying the articles...", fill = TRUE)
  pred <- category_predict(
    model_dir = model_dir,
    new_data = new_data
  )

  cat("R: Articles classified!", fill = TRUE)
  # Save the results
  cat("R: Saving the results...", fill = TRUE)
  write_csv(pred, file.path(output_dir, "predictions.csv"))

  return(pred)
}

opt$input <- "data/_raw/training_data/LEU.csv"

res <- classify_data(
  data_path = opt$input,
  model_dir = opt$model_dir,
  output_dir = opt$output
)
View(class_info)
