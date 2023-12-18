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
  ),
  make_option(
    c("-d", "--drop_model"),
    action = "store_true",
    default = FALSE,
    help = "Whether the model should be dropped from memory after prediction [default: %default]"
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
load_and_predict <- function(model_dir, category, new_data_row, drop_model = FALSE) {
  # Construct the model filename
  model_name <- paste0("model_fitted_on_entire_data_", category, ".rds")
  filename <- list.files(model_dir, full.names = TRUE) |> str_subset(model_name)

  # Check if the model exists
  if (length(filename) == 0) {
    stop("Model for category ", category, " not found!")
  }

  # Check if the model is already loaded
  model_variable <- paste0(category, "_model")
  if (!exists(model_variable, envir = globalenv())) {
    # Load the model
    cat("R: Load model for category", category, "...", fill = TRUE)
    assign(model_variable, readRDS(filename), envir = globalenv())
  }

  # Get the model
  model <- get(model_variable, envir = globalenv())

  # Predict using the model
  prediction <- predict(model, new_data_row)$.pred_class

  # Remove the model from memory
  if (drop_model) {
    rm(model, envir = globalenv())
    rm(list = model_variable, envir = globalenv())
  }

  return(as.character(prediction))
}

# Main function for predicting the category of articles
category_predict <- function(model_dir, new_data, drop_model = FALSE) {
  # Read in the class labels
  cat("R: Load class labels...", fill = TRUE)
  class_info <- read_csv(here("data/class_info.csv"))

  # Find classes that are the same as in the category column
  classes <- class_info |>
    select(class) |>
    unique() |>
    pull()

  categories <- class_info |>
    select(category) |>
    unique() |>
    pull()

  repeated_classes <- classes[(classes %in% categories)]

  # Find cagesories that are the same as in the subcategory column
  repeated_categories <- class_info |>
    group_by(category) |>
    summarise(n = n()) |>
    filter(n == 1) |>
    pull(category)

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
  new_data <- new_data |>
    mutate(
      level_1 = as.character(predict(model_level_1, new_data)$.pred_class)
    )

  # Remove the level 1 model from memory
  if (drop_model) {
    rm(model_level_1, envir = globalenv())
  }

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
      # If level 1 class is the same as level 2 class, assign level 1 class
      # to level 2 class
      if (level_1_category %in% repeated_classes) {
        new_data[i, "level_2"] <- as.character(level_1_category)
      } else {
        new_data[i, "level_2"] <- load_and_predict(
          model_dir = model_dir,
          category = level_1_category,
          new_data_row = new_data[i, ],
          drop_model = drop_model
        )
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
      # If level 2 class is the same as level 3 class, assign level 2 class
      # to level 3 class
      if (level_2_category %in% repeated_categories) {
        if (level_2_category == "Other_Other") {
          new_data[i, "level_3"] <- "OTOTAP"
        } else if (level_2_category == "Other_Virus") {
          new_data[i, "level_3"] <- "OTV"
        } else {
          new_data[i, "level_3"] <- as.character(level_2_category)
        }
      } else {
        new_data[i, "level_3"] <- load_and_predict(
          model_dir = model_dir,
          category = level_2_category,
          new_data_row = new_data[i, ],
          drop_model = drop_model
        )
      }
    }
  }

  return(new_data)
}

# Gather data and make prediction.
#    data_path: str - gives path to csv file with articles to classify
#    model_path: str - gives path to the directory where the models are located
#    output_dir: str - gives path to the directory where the results are saved
classify_data <- function(data_path, model_dir, output_dir, drop_model = FALSE) {
  # Read the new articles from csv file
  cat("R: Load the articles...", fill = TRUE)
  new_data <- read_csv(data_path) |>
    rename(pmid = PubMed_ID, abstract = Abstract) |>
    select(pmid, abstract) #|>
  # slice(1:10)

  # Perform prediction
  cat("R: Classifying the articles...", fill = TRUE)
  pred <- category_predict(
    model_dir = model_dir,
    new_data = new_data,
    drop_model = drop_model
  )

  cat("R: Articles classified!", fill = TRUE)
  # Save the results
  cat("R: Saving the results...", fill = TRUE)
  write_csv(pred, file.path(output_dir, "predictions.csv"))

  return(pred)
}

# Classify the articles
res <- classify_data(
  data_path = opt$input,
  model_dir = opt$model_dir,
  output_dir = opt$output,
  drop_model = opt$drop_model
)
