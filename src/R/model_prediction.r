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
    default = here("result/"), # Should be changed
    help = "The directory where the results will be saved [default: %default]"
  ),
  make_option(
    c("-m", "--model_dir"),
    type = "character",
    default = here("results/"),
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
  cat(paste("Location of classifier:", opt$model_dir, fill = TRUE))
}


# Function for predicting the category of articles
category_predict <- function(model_dir, new_data) {
  # Extact the model from the model directory
  filename_model_1 <- model_dir |>
    list.files(full.names = TRUE) |>
    str_subset("model_fitted_on_entire_data_class_with_HIV.rds")

  # Load the level 1 classifier
  cat("R: Load level 1 model...", fill = TRUE)
  model_level_1 <- readRDS(filename_model_1)

  # Classify articles according to level 1 model
  cat("R: Classifying articles according to level 1 model...", fill = TRUE)
  new_data$level_1 <- predict(model_level_1, new_data)$.pred_class


  # Classify articles according to level 2 models
  print("R: Classifying articles according to level 2 models...")

  # Add placeholder for level 2 classification
  new_data$level_2 <- NA
  for (i in seq(1:nrow(new_data))) {
    if (new_data[i, "level_1"] == "Allergen") {
      # Detect the filename for the level 2 Allergen classifier
      filename_allergen <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Allergen.rds")

      # Check if model is already loaded
      if (exists("allergen_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(allergen_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        allergen_model <- xfun::cache_rds({
          readRDS(filename_allergen)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(allergen_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "Autoimm") {
      # Detect the filename for the level 2 Autoimm classifier
      filename_autoimm <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Autoimm.rds")

      # Check if model is already loaded
      if (exists("autoimm_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(autoimm_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        autoimm_model <- xfun::cache_rds({
          readRDS(filename_autoimm)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(autoimm_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "Cancer") {
      # Detect the filename for the level 2 Cancer classifier
      filename_cancer <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Cancer.rds")

      # Check if model is already loaded
      if (exists("cancer_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(cancer_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        cancer_model <- xfun::cache_rds({
          readRDS(filename_cancer)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(cancer_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "HIV") {
      # Detect the filename for the level 2 HIV classifier
      filename_hiv <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_HIV.rds")

      # Check if model is already loaded
      if (exists("hiv_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(hiv_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        hiv_model <- xfun::cache_rds({
          readRDS(filename_hiv)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(hiv_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "Infectious_Disease") {
      # Detect the filename for the level 2 Infectious_Disease classifier
      filename_infectious_disease <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Infectious_Disease.rds")

      # Check if model is already loaded
      if (exists("infect_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(infect_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        infect_model <- xfun::cache_rds({
          readRDS(filename_infectious_disease)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(infect_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "Other") {
      # Detect the filename for the level 2 Other classifier
      filename_other <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Other_Other.rds")

      # Check if model is already loaded
      if (exists("other_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(other_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        other_model <- xfun::cache_rds({
          readRDS(filename_other)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(other_model, new_data[i, ])$.pred_class
      }
    } else if (new_data[i, "level_1"] == "Transplant") {
      # Detect the filename for the level 2 Transplant classifier
      filename_transplant <- model_dir |>
        list.files(full.names = TRUE) |>
        str_subset("model_fitted_on_entire_data_Transplant.rds")

      # Check if model is already loaded
      if (exists("transplant_model")) {
        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(transplant_model, new_data[i, ])$.pred_class
      } else {
        # Load model and cache it for faster loading
        transplant_model <- xfun::cache_rds({
          readRDS(filename_transplant)
        })

        # Classify articles according to level 2 model
        new_data[i, "level_2"] <- predict(transplant_model, new_data[i, ])$.pred_class
      }
    }
  }



  return(out)
}

# Gather data and make prediction.
#    model_path: str - gives path to trained model file
#    article path: str - gives path to csv file with articles to classify
classify_data <- function(data_path, model_dir, output_dir) {
  # Read the new articles from csv file
  cat("R: Load the articles...", fill = TRUE)
  new_data <- read_csv(data_path) |>
    # rename(pmid = PubMed_ID, abstract = Abstract) |>
    select(pmid, abstract)

  # Perform prediction
  cat("R: Classifying the articles...", fill = TRUE)
  pred <- category_predict(
    model_dir = model_dir,
    new_data = new_data
  )

  cat("R: Articles classified!", fill = TRUE)
  return(pred)
}

opt$input <- "data/_raw/training_data/ALPHA.csv"

res <- classify_data(
  data_path = opt$input,
  model_dir = opt$model_dir,
  output_dir = opt$output
)
out <- cbind(res$pred["pmid"], res$pred[".pred_class"])
write.csv(out, output_file, row.names = FALSE)

# You can then save or return these predictions as needed
write_csv(predicted_classes, "predicted_classes.csv") # Save predicted classes
write_csv(predicted_probabilities, "predicted_probabilities.csv") # Save predicted probabilities
