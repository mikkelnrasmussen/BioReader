retrive_articles <- function(pmidPositive, pmidNegative, pmidTBD, 
                             queryPositive="", queryNegative="", queryTBD="",
                             verbose=FALSE, shiny_input=FALSE, 
                             progress=FALSE){
  
  
  
  # Functions for choosing the chunk size to be extracted in each loop
  safe_check <- function(chnk_len=floor(N/K), K=1){
    if(chnk_len < 400){
      return(K)
    } else {
      K <- K+1
      chnk_len <- floor(N/K)
      return(safe_check(chnk_len, K))
    }
  }
  
  # If no PMIDs are supplied - set to NULL
  if(is.character(pmidPositive) && pmidPositive == ""){pmidPositive <- NULL}
  if(is.character(pmidNegative) && pmidNegative == ""){pmidNegative <- NULL}
  if(is.character(pmidTBD) && pmidTBD == ""){pmidTBD <- NULL}
  
  if(shiny_input){
    # Using regex to find all PubMed IDs in input boxes and convert to a list 
    # - only used when the input is giving through the Shiny app
    pmidPositive <- if(!is.null(pmidPositive)){str_extract_all(pmidPositive, "\\d+")}
    pmidNegative <- if(!is.null(pmidNegative)){str_extract_all(pmidNegative, "\\d+")} 
    pmidTBD <- if(!is.null(pmidTBD)){str_extract_all(pmidTBD, "\\d+")}
  }
  
  # Extracting PMIDs from PubMed queries
  if(queryPositive != ""){
    posQueryIDs <- entrez_search(db="pubmed", term=queryPositive, retmax=10000)$ids
  } else {
    posQueryIDs <- NULL
  }
  
  if(queryNegative != ""){
    negQueryIDs <- entrez_search(db="pubmed", term=queryNegative, retmax=10000)$ids
  } else {
    negQueryIDs <- NULL
  }
  if(queryTBD != ""){
    tbdQueryIDs <- entrez_search(db="pubmed", term=queryTBD, retmax=10000)$ids
  } else {
    tbdQueryIDs <- NULL
  }
  
  # Storing PMIDs and class in dataframes
  indexPos <- as.data.frame(c(pmidPositive, posQueryIDs))
  indexPos$class <- 1
  names(indexPos) <- c("pmid", "class")
  
  indexNeg <- as.data.frame(c(pmidNegative, negQueryIDs))
  indexNeg$class <- 0
  names(indexNeg) <- c("pmid", "class")
  
  indexTBD <- as.data.frame(c(pmidTBD, tbdQueryIDs))
  indexTBD$class <- 2
  names(indexTBD) <- c("pmid", "class")
  
  # Combine index dataframes and order by pmid
  index <- do.call("rbind", list(indexPos, indexNeg, indexTBD))
  
  # Create data frame for storing the results concatenate PMIDs
  df_final <- data.frame()
  all_ids <- index$pmid
  
  # Initialize variables for abstract retrieval
  K <- 1
  N <- length(all_ids)
  K <- safe_check()
  chnk_len <- floor(N/K)
  retreived <- 0
  
  if(progress){
    setProgress(message = "Downloading articles from PubMed...",
                detail = paste(retreived, "out of", N),
                value = 0)
  }

  for(i in seq(1, N, by=chnk_len)){
    
    # Setting indexes for articles to be retrieved
    start <- i
    stop <- i+chnk_len-1
    if(stop > N){
      stop <- N
    }
    
    # Select a subset of PMIDs to be downloaded
    current_ids <- paste0(all_ids[start:stop], collapse=",")
    
    # Define url with PMIDs and parse from PubMed
    api_key <- "cb3f7db995488faf13d1996de6a08a200a08"
    url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db",
                  "=pubmed&id=", current_ids, "&retmode=abstract&rettype=xml",
                  "&api_key=", api_key)
    
    # Set up retry procedure should the connection fail
    maxTimes <- 5
    req <- RETRY(verb = "GET", url = url, times = maxTimes, 
                 quiet = FALSE, terminate_on = NULL)
    main_node <- xml2::read_xml(req) %>% xml_children()
    
    # Extract relevant information from PubMed XML
    pmid_path <- "MedlineCitation/PMID"
    title_path <- "MedlineCitation/Article/ArticleTitle"
    abstract_path <- "MedlineCitation/Article/Abstract"
    year_path <- "PubmedData/History/PubMedPubDate/Year"
    pmid <- main_node %>% map_chr(. %>% xml_find_first(pmid_path) %>% xml_text())
    title <- main_node %>% map_chr(. %>% xml_find_first(xpath=title_path) %>% xml_text())
    abstract <- main_node %>% map_chr(. %>% xml_find_first(xpath=abstract_path) %>% xml_text())
    year <- main_node %>% map_chr(. %>% xml_find_first(xpath=year_path) %>% xml_text())
    df_current <- tibble(pmid=pmid, year=year, title=title, abstract=abstract)
    df_final <- rbind(df_final, df_current)
    
    if(progress){
      retreived <- length(df_final$pmid)
      # Increment the progress bar, and update the detail text.
      incProgress(amount=chnk_len/N, 
                  detail=paste(retreived, "out of", N))
    }
  }
  
  # Filter the index dataframe based on the articles it was possible to 
  # retrieve from PubMed
  index <- filter(index, index$pmid %in% df_final$pmid)
  
  df_final <- df_final %>% 
    dplyr::select(pmid, year, title, abstract)
  
  # If abstracts are missing on PubMed, find out which pmids are associated 
  # with these
  missing_pmids <- df_final %>% 
    group_by(pmid) %>% 
    filter(all(is.na(abstract))) %>% 
    pull(pmid)
  
  # Filter df_final, index and info based on the pmids for articles
  # with missing abstracts 
  df_final <- df_final %>% filter(!(pmid %in% missing_pmids))
  if(verbose){
    frac_retrieved <- sum(df_final$pmid %in% index$pmid)/length(index$pmid)
    print(paste0("Procent of PubMed articles with abstract: ", 
                 round(frac_retrieved, 4)*100, "%"))
  }
  index <- index %>% filter(!(pmid %in% missing_pmids))
  
  # Join information retreived from PubMed with class labels
  index$pmid <- as.character(index$pmid)
  df_final <- df_final %>% 
    inner_join(., index, by='pmid')
  
  return(df_final)
}

pubmed_articles <- function(pmidPositive, pmidNegative, pmidTBD, verbose=FALSE,
                            shiny_input=FALSE, progress=FALSE){
    
    # Detech number of cores for parallel processing
    numCores <- detectCores()
    
    if(shiny_input){
      # Using regex to find all PubMed IDs in input boxes and convert to lists 
      # - only used when the input is giving through the Shiny app
      if(!(is.integer(pmidPositive) & is.integer(pmidNegative) & is.integer(pmidTBD))){
        pmidPositive <- str_match_all(pmidPositive, "\\d+")
        pmidNegative <- str_match_all(pmidNegative, "\\d+")
        pmidTBD <- str_match_all(pmidTBD, "\\d+")
      }
    }
    
    # Storing PMIDs and class in dataframes
    indexPos <- as.data.frame(pmidPositive)
    indexPos$class <- 1
    names(indexPos) <- c("pmid", "class")
    
    indexNeg <- as.data.frame(pmidNegative)
    indexNeg$class <- 0
    names(indexNeg) <- c("pmid", "class")
    
    indexTBD <- as.data.frame(pmidTBD)
    indexTBD$class <- 2
    names(indexTBD) <- c("pmid", "class")
    
    # Combine index dataframes and order by pmid
    index <- do.call("rbind", list(indexPos, indexNeg, indexTBD))
    
    # Function for retrieving all of the information from PubMed based
    # on the listed PMIDs
    retrieve_pubmed <- function(x){
        
        # Making query and splitting it up into groups depending on number of cores. 
        split_query <- split(x, f = factor(seq(1,numCores)))
        my_query_pl <- lapply(1:numCores, function(i) paste(split_query[[i]], 
                                                            collapse = " "))
        
        # Creating enterz-object with info about fx WebEnv - multicore version
        my_entrez_id_pl <- lapply(my_query_pl, get_pubmed_ids)
        
        # Fetch xml file from all of the given PudMed IDs - multicore version
        my_abstracts_xml_pl <- mclapply(my_entrez_id_pl, fetch_pubmed_data, 
                                        mc.cores = numCores)
        
        # Convert back to a single character string
        my_abstracts_xml <- toString(my_abstracts_xml_pl)
        
        # Store Pubmed Records as elements of a list
        all_xml <- articles_to_list(my_abstracts_xml)
      
        # Perform operation (using mclapply here (example is with lapply), 
        # no further parameters)
        final_df <- do.call(rbind, mclapply(all_xml, article_to_df, 
                                            max_chars = -1, getAuthors = FALSE, 
                                            mc.cores = numCores))
        return(final_df)
    }
    
  
    
    # Total number of articles
    total <- length(index$pmid)
    retreived <- 0
    articles <- 100
    
    if(progress){
      # Increment the progress bar, and update the detail text.
      # incProgress(amount=0, 
      #             detail = paste(retreived, "out of", total))
    }
    
    # If the number of PMIDs are greater than 1300, the retrievel from PubMed is 
    # splitted into several subprocesses, since I (Mikkel) have encountered issues, 
    # when the datasetes are too big.
    start_time <- Sys.time()
    if(length(index$pmid) > 1000){
      
      # Splitting of the PMIDs into dataframes with 100 PMIDs 
      sub_split <- total/articles
      splitted_data <- split(index, f = factor(seq(1, sub_split)))
        
      # Initializing the final dataframe
      final_df <- data.frame()
        
      # Looping over the splitted dataframes
      for(i in splitted_data){
        temp_df <- retrieve_pubmed(i[["pmid"]])
        final_df <- rbind(final_df, temp_df)
        
        if(progress){
          # Increment the progress bar, and update the detail text.
          # retreived <- length(final_df$pmid)
          # incProgress(amount=articles/total, 
          #             detail=paste(retreived, " out of ", total))
        }
      }
      end_time <- Sys.time()
      print(end_time - start_time)
        
    } else {

      # If the number of PMIDs are less than 1000, the normal process is run
      final_df <- retrieve_pubmed(index[["pmid"]])
      
      end_time <- Sys.time()
      print(end_time - start_time)

    }
    
    if(verbose){
        frac_retrieved <- sum(final_df$pmid %in% index$pmid)/length(index$pmid)
        print(paste0("Procent of PubMed articles retrieved: ", 
                     round(frac_retrieved, 4)*100, "%"))
    }
    
    # Filter the index dataframe based on the articles it was possible to 
    # retrieve from PubMed
    index <- filter(index, index$pmid %in% final_df$pmid)
    
    final_df <- final_df %>% 
       dplyr::select(pmid, year, title, abstract)
    
    # If abstracts are missing on PubMed, find out which pmids are associated 
    # with these
    missing_pmids <- final_df %>% 
        group_by(pmid) %>% 
        filter(all(is.na(abstract))) %>% 
        pull(pmid)
    
    # Filter final_df, index and info based on the pmids for articles
    # with missing abstracts 
    final_df <- filter(final_df, !(pmid %in% missing_pmids))
    if(verbose){
      frac_retrieved <- sum(final_df$pmid %in% index$pmid)/length(index$pmid)
      print(paste0("Procent of PubMed articles with abstract: ", 
                   round(frac_retrieved, 4)*100, "%"))
    }
    index <- filter(index, !(pmid %in% missing_pmids))
    
    # Join information retreived from PubMed with class labels
    index$pmid <- as.character(index$pmid)
    final_df <- final_df %>% 
       inner_join(., index, by='pmid')
    
    return(final_df)
}

split_data <- function(data){
    
   # Create data frame with train data and remove articles with 
   # missing PMID or abstract and set class labels
   df_train <- data[data$class < 2, ] %>% 
      drop_na(pmid) %>% 
      filter(!is.na(abstract)) %>% 
      mutate(class = factor(if_else(class == 1, "Positive", "Negative"),
                            levels = c("Positive", "Negative")))
   
   # Create data with test data and remove articles with missing PMID or abstract
   df_test <- data[data$class == 2, ] %>% 
      drop_na(pmid) %>% 
      filter(!is.na(abstract))

   out <- list()
   
   out$train_data <- df_train
   out$test_data <- df_test
   
   return(out)
}

train_classifiers <- function(train_data, eval_metric, verbose=FALSE,
                              fit_all=FALSE, seed_num=FALSE, fold=5, progress=FALSE,
                              model_names=c('bag_mars', 'bag_tree', 'bart','xgboost', 
                                       'c5', 'dt', 'fdm', 'ldm', 'rdm',
                                       'logit', 'mars', 'nnet', 'mr', 
                                       'nb', 'knn', 'null', 'pls', 'rf', 'rule', 
                                       'svm_linear' ,'svm_rbf', 'svm_poly')){ 
    
   # Creating recipe and specifying outcome and predictors and setting pmids as 
   # id variable. Preprocessing steps are also determined here. 
   train_data <- train_data[, c('pmid', 'abstract', 'class')]
   train_rec <-
        recipe(class ~ ., data = train_data) %>% 
        update_role(pmid, new_role = "id") %>% 
        step_tokenize(abstract) %>% 
        step_stopwords(abstract) %>%
        step_stem(abstract) %>%
        step_tokenfilter(abstract, max_tokens = 500) %>%
        step_tfidf(abstract)
    
    # Setting up Bagged MARS Model
    bag_mars_spec <- bag_mars() %>%
        set_engine("earth") %>%
        set_mode("classification")

    # Setting up Bagged Decision Tree Model
    bag_tree_spec <- bag_tree() %>%
        set_engine("rpart") %>%
        set_mode("classification")
    
    # Setting up Bayesian additive regression trees (BART)
    bart_spec <- parsnip::bart() %>% 
      set_engine("dbarts") %>% 
      set_mode("classification")
    
    # Setting up Boosted Trees (the XGboost model)
    xgboost_spec <- boost_tree() %>% 
        set_engine("xgboost") %>% 
        set_mode("classification")
    
    # Setting up C5.0 Rule-Based Classification Model
    c5_spec <- C5_rules() %>%
        set_engine("C5.0") %>%
        set_mode("classification")
    
    # Setting up Decision Tree Model
    dt_spec <- decision_tree() %>% 
        set_engine("C5.0") %>% 
        set_mode("classification")
    
    # Setting up Flexible Discriminant analysis model
    fdm_spec <- discrim_flexible() %>% 
        set_engine("earth") %>% 
        set_mode("classification")
    
    # Setting up Linear Discriminant analysis model
    ldm_spec <- discrim_linear() %>% 
        set_engine("MASS") %>% 
        set_mode("classification")
    
    # Setting up Regularized Discriminant Model
    rdm_spec <- discrim_regularized(frac_common_cov=0, frac_identity=0.25) %>%
        set_engine("klaR") %>%
        set_mode("classification")
    
    # Setting up the Logistic Regression Model
    logit_spec <- logistic_reg(penalty = 0.1, mixture = 0.5) %>%
        set_engine("glmnet") %>% 
        set_mode("classification")
    
    # Setting up MARS
    mars_spec <- parsnip::mars() %>% 
        set_engine("earth") %>% 
        set_mode("classification")
    
    # Setting up the Single Layer Neural Network (NNET model)
    nnet_spec <- mlp(epochs = 50, hidden_units = 10,  activation = "relu") %>%
        set_engine("keras") %>%
        set_mode("classification")
    
    # Setting up Multinomial Regression Model
    mr_spec <- multinom_reg(penalty = 0.01, mixture=0.5) %>% 
        set_engine("glmnet") %>% 
        set_mode("classification")
    
    # Setting up the Naive Bayes model
    nb_spec <- naive_Bayes(Laplace = 1) %>%
        set_engine("naivebayes") %>% 
        set_mode("classification")
    
    # Setting up the K-nearest neighbors model
    knn_spec <- nearest_neighbor() %>% 
        set_engine("kknn") %>% 
        set_mode("classification")
    
    # Setting up the Null model
    null_spec <- null_model() %>% 
      set_engine("parsnip") %>% 
      set_mode("classification") 
      
    # Setting up the Partial least squares (PLS)
    pls_spec <- parsnip::pls(num_comp=10, predictor_prop=1/3) %>% 
      set_engine("mixOmics") %>% 
      set_mode("classification")
    
    # Setting up the Random forest model
    rf_spec <- rand_forest(trees = 1000) %>%
        set_engine("ranger") %>%
        set_mode("classification")
    
    # Setting up RuleFit Models
    rule_spec <- rule_fit() %>% 
        set_engine("xrf") %>% 
        set_mode("classification")
    
    # Setting up the Linear support vector machines (SVM)
    svm_linear_spec <- svm_linear() %>%
      set_engine("kernlab") %>%
      set_mode("classification")
    
    # Setting up the Polynomial support vector machines (SVM)
    svm_poly_spec <- svm_poly() %>%
        set_engine("kernlab") %>%
        set_mode("classification")
     
    # Setting up the Radial basis function support vector machines (SVM) model
    svm_rbf_spec <- svm_rbf(cost = 0.5) %>%
        set_engine("kernlab") %>%
        set_mode("classification")
    
    # List with all of the model specifications
    model_specs <- list(bag_mars = bag_mars_spec, bag_tree = bag_tree_spec,
                        bart = bart_spec, xgboost = xgboost_spec, c5 = c5_spec, 
                        dt = dt_spec, fdm = fdm_spec, ldm = ldm_spec, 
                        rdm = rdm_spec, logit = logit_spec, 
                        mars = mars_spec, nnet = nnet_spec, 
                        mr = mr_spec, nb = nb_spec, knn = knn_spec, 
                        null = null_spec, pls = pls_spec, rf = rf_spec, 
                        rule = rule_spec, svm_linear = svm_linear_spec, 
                        svm_poly = svm_poly_spec, svm_rbf = svm_rbf_spec)
    
    # Include only the specified models
    selected_model_specs <- model_specs[names(model_specs) %in% model_names]
    
    # Evaluating model proformance with 10-fold cross-validation resampling
    if(seed_num){
      set.seed(seed_num)
    }
    train_folds <- vfold_cv(train_data, v = fold, repeats = 1)
    
    # Evaluation metrics and save predictions
    metrics = metric_set(roc_auc, sens, spec, accuracy, precision)
    control <- control_resamples(save_pred = TRUE)
    
    # Training the models using workflow set
    # Set up workflow where the train recipe is applied to every model specified
    # train_models <- 
    #     workflow_set(
    #         preproc = list(base = train_rec),
    #         models = selected_model_specs,
    #         cross = TRUE
    #     )
    
    # doParallel
    # cores <- parallel::detectCores(logical = FALSE)
    # cl <- makePSOCKcluster(cores)
    # registerDoParallel(cores = cl)
    # Train all the models by mapping the fit_resamples function to every 
    # training workflow
    # train_models <- train_models %>%
    #    workflow_map("fit_resamples", resamples = train_folds,
    #                 metrics = metrics,
    #                 verbose = TRUE,
    #                 control=control_resamples(save_pred = TRUE))
    # stopCluster(cl)
    
    # Initialize number of models and list for storing results
    num_models <- length(selected_model_specs)
    results <- list()
    
    # Increase progress if running in Shiny
    if(progress){
      
      # Increment the progress bar, and update the detail text.
      setProgress(value=0,
                  message = "Training classification models....",
                  detail = paste(0, "out of", num_models))
    }
    
    wtime <- system.time({
    for(i in 1:num_models){
      start.time <- Sys.time()
      model <- names(selected_model_specs[i])
      print(model)
      train_result <- fit_resamples(selected_model_specs[[model]],
                                    train_rec, 
                                    train_folds, 
                                    metrics = metrics,
                                    control = control)
      results[[model]] <- train_result
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(time.taken)
      
      # Increment the progress bar, and update the detail text.
      if(progress){
        incProgress(amount=1/num_models,
                    message = "Training classification models....",
                    detail=paste(i, "out of", num_models))
      }
    }
    })
    wtime
    
    # Evaluation
    # Create a tibble with model results, specs and respective names
    models <- tibble(model = results,
                     model_spec = selected_model_specs,
                     model_name = model_names)
    
    # Create a helper function for collecting the metrics 
    map_collect_metrics <- function(model){
        model %>% 
            dplyr::select(id, .metrics) %>% 
            unnest(.metrics)
    }
    
    # Apply helper function and extract the metrics 
    model_metrics <- models %>% 
        mutate(res = purrr::map(model, map_collect_metrics)) %>% 
        dplyr::select(model_name, res) %>% 
        unnest(res)
    
    # Create a helper function for collecting the predictions 
    map_collect_predictions <- function(model){
        model %>% 
            dplyr::select(id, .predictions) %>% 
            unnest(.predictions)
    }
    
    # Apply helper function and extract the predictions 
    model_predictions <- models %>% 
        mutate(res = purrr::map(model, map_collect_predictions)) %>% 
        dplyr::select(model_name, res) %>% 
        unnest(res)
    
    # Selecting the best model
    models_summary <- model_metrics %>% 
        group_by(model_name, .metric) %>% 
        dplyr::summarise(mean = mean(.estimate), .groups='drop') %>% 
        filter(.metric == eval_metric)
    
    # Determine which model preformed the best
    bestclassifier <- models_summary$model_name[which.max(models_summary$mean)]
    
    # Create mapping for printing results
    metrics_map <- data.frame(
        abbreviation = c("roc_auc", "sens", "spec", "accuracy", "precision"),
        metric = c("AUC", "Sensitivity", "Specificity", "Accuracy", 
                   "Precision")
    )
    
    # Print the name of the best classifier, metric used and value
    if(verbose){
        metric <- metrics_map[metrics_map$abbreviation == eval_metric, ]$metric
        value <- models_summary[models_summary$model_name == bestclassifier,]$mean
        print(paste("Comparing the models using the metric:", metric))
        print(paste("The best classifier was:", bestclassifier))
        print(paste("Performance:", metric, "=", round(value, 4)))
    }
    
    # Select the final model's specifications
    final_model_spec <- model_specs[bestclassifier][[1]]
    
    # Specify the final workflow
    final_wf <- workflow() %>%
        add_recipe(train_rec) %>% 
        add_model(final_model_spec)
    
    # Fit the final model to the whole training dataset
    best_model_fit <- fit(final_wf, data = train_data)
    
    # Create list to store return objects
    out <- list()
    
    out$model_metrics <- model_metrics
    out$model_predictions <- model_predictions
    out$best_model <- best_model_fit
    
    model_fits <- list()
    # In order to compare the performance of all the models on the training data
    if(fit_all){
       for(model in models$model_name){
         
         # Extract the model specification
         model_spec <- model_specs[model][[1]]
          
         # Specify the workflow object for each model
         wf <- workflow() %>%
            add_recipe(train_rec) %>%
            add_model(model_spec)
         
         # Fit the model on the whole training dataset and save the fitted model
         model_fit <- fit(wf, data = train_data)
         model_fits[[model]] <- model_fit

       }
       out$fitted_models <- model_fits
    } else {
      model_fits <- list()
      model_fits[[bestclassifier]] <- best_model_fit
      out$fitted_models <- model_fits
    }
    
    return(out)
}

classifier_predict <- function(final_model_fit, test_data){
    
   # Predict classes for the test data and the related probabilities
   pred <- bind_cols(test_data,
      predict(final_model_fit, new_data = test_data[, c('pmid', 'abstract')]),
      predict(final_model_fit, new_data = test_data[, c('pmid', 'abstract')], 
              type = "prob")
   )
   
   # Create hyperlinks to articles, collect in a dataframe and specify the 
   # column names. Not using links to PubMed for IEDB use. 
   articles <- paste("<a href=", "\"http://www.ncbi.nlm.nih.gov/pubmed/", 
                   test_data$pmid,"\", target=\"_blank\">", test_data$title, 
                   "</a>", sep="")
   
   results <- cbind(articles, 
                  as.vector(test_data$year), 
                  pred$.pred_class, 
                  pred$.pred_Positive,
                  as.vector(test_data$pmid))
   
   colnames(results) <- c("Article", 
                        "Year of publication", 
                        "Label", 
                        "Rank of article (ordered by probability of correct classification)", 
                        "PMID")
   
   # Select the artcles classified as positive and rank by highest probability
   positive <- results[as.numeric(results[,3]) == 1,]
   positive <- positive[order(as.numeric(as.character(positive[,4])), 
                            decreasing = TRUE), ]
   
   # Select the artcles classified as positive and rank by highest probability
   negative <- results[as.numeric(results[,3]) == 2,]
   negative <- negative[order(as.numeric(as.character(negative[,4])), 
                            decreasing = TRUE), ]
   
   # Collet the positive and negative results in a dataframe and specify the 
   # order of rank    
   ranked_results <- rbind(positive, negative)
   ranked_results <- as.data.frame(ranked_results)
   ranked_results[, 4] <- c(1:length(rownames(ranked_results)))
   ranked_results <- ranked_results %>% 
     mutate(Label = if_else(Label == 1, "Positive", "Negative"))
   
   out <- list()
   
   out$pred <- pred
   out$model <- final_model_fit
   out$ranked_results <- ranked_results
   
   return(out)
    
}

# Wrapper function for training and predicting in one go
classify_articles <- function(data_separated, metric="roc_auc", fold=5,
                              verbose=TRUE, fit_all=FALSE, progress=FALSE,
                              model_names=c("xgboost", "dt", "fdm", "logit", "mars",
                                       "nnet", "mr","knn", "rf", "svm_rbf")){
  
  # Splitting the PMIDs into training and testing data
  training_data <- tibble(data_separated$train_data)
  testing_data <- tibble(data_separated$test_data)
  
  sum(training_data$class == 'Positive')
  sum(training_data$class == 'Negative')
  dim(testing_data)
  
  # Training the classifiers and select the best classifier based on specified 
  # metric
  training_results <- train_classifiers(train_data = training_data, 
                                        eval_metric=metric, fold=fold,
                                        verbose=verbose, fit_all=fit_all, 
                                        progress=progress,
                                        model_names=model_names)
  
  # Select best model
  best_model <- training_results$best_model
  
  # Predict classes for the test data 
  prediction_results <- classifier_predict(final_model_fit=best_model, 
                                           test_data=testing_data)
  
  return(prediction_results)
  
}

evaluate_models <- function(pred_train=NULL, test_data=NULL, fitted_models=NULL, 
                            metrics, classes){
   
   # Create dataframe for mapping between model abbreviations and names
   models_map <- data.frame(model_name=c('bag_mars', 'bag_tree', 'bart','xgboost', 
                                         'c5', 'dt', 'fdm', 'ldm', 'rdm',
                                         'logit', 'mars', 'nnet', 'mr', 
                                         'nb', 'knn', 'null', 'pls', 'rf', 'rule', 
                                         'svm_linear' ,'svm_rbf', 'svm_poly'),
                            Model=c('Bagged MARS', 'Bagged Decision Tree', 
                                    'Bayesian additive regression trees (BART)',
                                    'Boosted Trees', 'C5.0 Rule-Based', 
                                    'Decision Tree', 'Flexible Discriminant',
                                    'Linear Discriminant',
                                    'Regularized Discriminant Model', 
                                    'Logistic Regression', 'MARS', 
                                    'Neural Network', 'Multinomial Regression', 
                                    'Naive Bayes', 
                                    'K-Nearest Neighbors', 'Null model',
                                    'Partial least squares (PLS)', 'Random Forest', 
                                    'RuleFit', 'Linear support vector machines',
                                    'Radial Basis Function SVM', 
                                    'Polynomial SVM'))
   
   # Subset mapping table to the trained models
   models_map <- models_map[models_map$model_name 
                            %in% unique(names(fitted_models)), ]
   if(!is.null(pred_train)){
     # Map from model abbreviations to model names
     pred_train$Model <- mapvalues(pred_train$model_name, 
                                   from=models_map$model_name, 
                                   to=models_map$Model)
     
     # Get AUC scores from 10-fold cross-validation
     auc_metrics <- metrics[metrics$.metric == 'roc_auc', ] %>% 
        drop_na() %>% 
        group_by(model_name) %>% 
        dplyr::summarise(mean_auc = mean(.estimate), .groups='drop') %>% 
        mutate(label.auc = sprintf("AUC = %.3f", mean_auc))
     
     # Map from model abbreviations to model names
     auc_metrics$Model <- mapvalues(auc_metrics$model_name, 
                                    from=models_map$model_name, 
                                    to=models_map$Model)
     
     # Generate data for the ROC curves
     df.roc.train <- pred_train %>% 
        drop_na() %>% 
        dplyr::group_by(Model, id) %>% 
        roc_curve(event_level='first', truth=class, .pred_Positive)
     
     
     # Generate plot of the ROC curves for each model on the training data
     train_plot <- df.roc.train %>% 
        ggplot(aes(x=1 - specificity, y=sensitivity)) +
        geom_path(aes(group=id, colour=Model), alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, lty = 3) +
        facet_wrap(. ~Model) +
        theme(legend.position="none") +
        geom_text(data=auc_metrics, x=0.75, y=0.25, size=3, 
                  aes(label=label.auc), inherit.aes = F) + 
        labs(title='ROC curves across the 10-fold cross-validation for training data',
             subtitle='Mean AUC scores across the 10 cross-validation folds')
     
     print(train_plot)
   }
   
   # Evaluate all classifiers on test data
   all_prediction_results <- data.frame()
   for(model in models_map$model_name){
      current_results <- classifier_predict(final_model_fit=fitted_models[[model]], 
                                            test_data=testing_data)$pred
      current_results$Model <- models_map[models_map$model_name == model, ]$Model
      all_prediction_results <- rbind(all_prediction_results, current_results)
   }
   
   # Add the true classes
   all_test_results <- inner_join(all_prediction_results %>% 
                                     dplyr::select(-one_of('class')), 
                                  classes, by='pmid')
   
   # Calculate the test AUC scores for the all the models
   all_metrics <- all_test_results %>% 
      group_by(Model) %>% 
      roc_auc(event_level='first', truth=class, .pred_Positive) %>% 
      drop_na() %>% 
      mutate(label.auc = sprintf("AUC = %.3f", .estimate))
   
   # Generate data for the ROC curves
   df.roc.test <- all_test_results %>% 
      drop_na() %>% 
      dplyr::group_by(Model) %>% 
      roc_curve(event_level='first', truth=class, .pred_Positive)
   
   df.roc.test.smooth <- all_test_results %>% 
      drop_na() %>% 
      dplyr::group_by(Model) %>% 
      roc_curve(event_level='first', truth=class, .pred_Positive)
   
   # Plot ROC curves for all of the models using the test data
   test_plot <- df.roc.test %>% 
      ggplot(aes(x=1 - specificity, y=sensitivity)) +
      geom_path(aes(colour=Model), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      facet_wrap(. ~Model) +
      theme(legend.position="none") +
      geom_text(data=all_metrics, x=0.75, y=0.25, size=3, 
                aes(label=label.auc), inherit.aes = F) + 
      labs(title='ROC curves for the test data',
           subtitle='AUC scores')
   print(test_plot)
   

   # Find the lowest 1 - specificity at the 5% false negative level 
   # (= 95% sensitivity)
   df.best.spec <- df.roc.test %>% 
      group_by(Model) %>% 
      filter(round(sensitivity, 2) == 0.95) %>% 
      dplyr::summarise(max_spec = max(specificity)) %>% 
      mutate(spec.label = sprintf("Specificity = %.3f", max_spec))
   
   df.best.spec.smooth <- df.roc.test.smooth[!(df.roc.test.smooth$Model %in% 
                                                  df.best.spec$Model), ] %>% 
      group_by(Model) %>% 
      filter(round(sensitivity, 2) == 0.95) %>% 
      dplyr::summarise(max_spec = max(specificity)) %>% 
      mutate(spec.label = sprintf("Specificity = %.3f", max_spec))
   
   df.best.spec <- rbind(df.best.spec, df.best.spec.smooth)
      
   df.lines <- data.frame(X = c(rep(df.best.spec$max_spec, 2)),
                          Y = c(rep(0, length(df.best.spec$Model)), 
                                rep(0.95, length(df.best.spec$Model))))
   df.lines$Model <- as.factor(rep(df.best.spec$Model, times = 2))
   
   # Generate plot of the ROC curves for each model on the test data
   train_plot_sens_thres <- df.roc.test %>% 
      ggplot(aes(x=1 - specificity, y=sensitivity)) +
      geom_path(aes(colour=Model), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      facet_wrap(. ~Model) +
      theme(legend.position="none") +
      geom_text(data=all_metrics, x=0.75, y=0.25, size=3, 
                aes(label=label.auc), inherit.aes = F) + 
      labs(title='ROC curves for the test data',
           subtitle='AUC scores') + 
      geom_point(data=df.lines, aes(x=1-X, y=Y, group = Model), colour="red") + 
      geom_line(data=df.lines, aes(x=1-X, y=Y, group = Model), colour = "red", 
                alpha = 0.5) + 
      geom_text(data=df.best.spec, x=0.65, y=0.15, size=3, 
                aes(label=spec.label), inherit.aes = F)
   
   print(train_plot_sens_thres)
   
   # Generate table with AUC scores and best specificity at 95% sensitivity
   df.summarized <- df.best.spec %>% 
      dplyr::select(Model, spec.label) %>% 
      inner_join(., all_metrics %>% dplyr::select(Model, label.auc), 
                 by='Model') 
   
   DT::datatable(df.summarized,
                 colnames = c('Model', "Specificity at 95% sensitivity", 
                              'AUC score'),
                 rownames = FALSE,
                 options = list(
                    columnDefs = list(list(className = 'dt-left', targets = 0:2))
                 ))
   
   selected.models <- c("Random Forest", "Neural Network", 
                        "Radial Basis Function SVM", "RuleFit", "Naive Bayes", 
                        "K-Nearest Neighbors")
   
   # Generate plot of the ROC curves for each model on the test data
   df.roc.test.compare <- df.roc.test %>% filter(Model %in% selected.models)
   all_metrics.compare <- all_metrics %>% filter(Model %in% selected.models)
   
   train_plot_compare <- df.roc.test.compare %>% 
      ggplot(aes(x=1 - specificity, y=sensitivity)) +
      geom_path(aes(colour=Model), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      labs(title='Comparison of ROC curves for the 3 best classifiers and the 3 worst classifiers',
           subtitle = "Classifier performance is assessed by AUC scores")
   
   print(train_plot_compare)
   
}
