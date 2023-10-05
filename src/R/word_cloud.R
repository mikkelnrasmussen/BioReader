## Call libraries
suppressMessages(library(wordcloud))
# suppressMessages(library(wordcloud2)) # to render wordclouds
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(workflows))
suppressMessages(library(tune))
suppressMessages(library(stopwords))
suppressMessages(library(tidymodels))
suppressMessages(library(tidytext))
suppressMessages(library(textrecipes))
suppressMessages(library(forcats))
suppressMessages(library(RColorBrewer))
library(grid)
library(gridExtra)
# library(ggwordcloud)

word_cloud <- function(data, pmid_data){

   train_data <- data$train_data
   test_data <- data$test_data

   # Creating recipe and specifying outcome and predictors and setting pmids as id variable.
   # Preprocessing steps are also determined here. 
   train_rec <-
      recipe(class ~ ., data = train_data) %>% 
      update_role(pmid, new_role = "id") %>% 
      step_tokenize(abstract) %>% 
      step_stopwords(abstract) %>%
      step_stem(abstract) %>%
      step_tokenfilter(abstract, max_tokens = 500)

   # Create dtm with term frequency (the counts of words in each document)
   dtm_count <- train_rec %>% 
      step_tf(abstract) %>% 
      step_mutate_at(starts_with("tf_"), fn = as.integer) %>% 
      prep() %>% 
      juice() %>% 
      dplyr::select(-c(class, year, title)) %>% 
      column_to_rownames(var = "pmid")
   
   # Create dtm with tfidf weigthing
   dtm_tfidf <- train_rec %>% 
     step_tfidf(abstract) %>% 
     prep() %>% 
     juice() %>% 
     dplyr::select(-c(class, year, title)) %>% 
     column_to_rownames(var = "pmid")
     
   
   # Convert to matrix for compatibility with old code
   dtm_tfidf <- as.matrix(dtm_tfidf)
   dtm_count <- as.matrix(dtm_count)
   
   ## Reduce terms using T test !STOP! DO GLM INSTEAD (http://www.r-bloggers.com/do-not-log-transform-count-data-bitches/)
   ## Or do permutation test
   fac <- as.factor(pmid_data$class[pmid_data$class < 2])
   colnames(dtm_count) <- gsub(pattern = "tf_abstract_", "", colnames(dtm_count))
   colnames(dtm_tfidf) <- gsub(pattern = "tfidf_abstract_", "", colnames(dtm_tfidf))
   dtmcount <- dtm_count[,grepl(pattern = "[a-z]", colnames(dtm_count), ignore.case=TRUE, perl=TRUE)]
   testdtm <- dtm_tfidf[,grepl(pattern = "[a-z]", colnames(dtm_tfidf), ignore.case=TRUE, perl=TRUE)]
   
   # Calculate the most significant words
   pvals <- apply(testdtm, 2, function(x){wilcox.test(x[fac==1],x[fac==0],
                                                      exact=FALSE)$p.value})
   pvals <- unname(pvals)
   
   testdtm <- testdtm[,order(pvals)]
   dtmcount <- dtmcount[,order(pvals)]
   pvals <- pvals[order(pvals)]
   fc <- (colMeans(dtmcount[fac==1,])+1)/(colMeans(dtmcount[fac==0,])+1)
   
   dtmcount_pos <- dtmcount[,fc>1]
   dtmcount_neg <- dtmcount[,fc<1]
   pos_av_freq <- signif(colMeans(dtmcount_pos[,1:25]), digits = 2)
   neg_av_freq <- signif(colMeans(dtmcount_neg[,1:25]), digits = 2)
   
   if(length(pvals[pvals<0.05]) > 100) {
         testdtm <- testdtm[,1:length(pvals[pvals<0.05])]
   } else if (ncol(testdtm) > 100) {
         testdtm <- testdtm[,1:100]
   }
   
   pos <- as.matrix(dtmcount[rownames(dtmcount) %in% pmid_data[pmid_data$class == 1, ]$pmid, ])
   neg <- as.matrix(dtmcount[rownames(dtmcount) %in% pmid_data[pmid_data$class == 0, ]$pmid, ])
   testdtm <- testdtm[, order(colnames(testdtm))]
   dictionary <- colnames(testdtm)
   
   topterms <- colnames(pos)[1:50]
   pos_av_freq <- signif(colMeans(pos[,1:50]), digits = 2)
   pos_av_freq <- pos_av_freq/max(pos_av_freq)
   neg_av_freq <- signif(colMeans(neg[,1:50]), digits = 2)
   neg_av_freq <- neg_av_freq/max(neg_av_freq)
   for (i in 1:10) {
         if (pos_av_freq[i] > neg_av_freq[i]) {
               neg_av_freq[i] <- neg_av_freq[i] / pos_av_freq[i]
               pos_av_freq[i] <- 1
         }
         else {
               pos_av_freq[i] <- pos_av_freq[i] / neg_av_freq[i]
               neg_av_freq[i] <- 1
         }
   }
   
   pos_av_freq <- data.frame(word = names(pos_av_freq), freq = pos_av_freq)
   neg_av_freq <- data.frame(word = names(neg_av_freq), freq = neg_av_freq)
   
   out <- list()
   
   out$pos_av_freq <- pos_av_freq
   out$neg_av_freq <- neg_av_freq
   
   return(out)
   
   # dev.new <- function(width = 7, height = 7){
   #    platform <- sessionInfo()$platform
   #    if(grepl("linux",platform)){
   #       x11(width=width, height=height)
   #    }
   #    else if (grepl("pc",platform)){
   #          windows(width=width, height=height)
   #    }
   #    else if (grepl("apple", platform)){
   #       quartz(width=width, height=height)
   #    }
   # }
   # 
   # 
   # # wordcloud - original
   # cols  <- colorRampPalette(c("#530000", "#D0D0D0"))(25);
   # par(mar = rep(1, 4))
   # wordcloud(words = pos_av_freq$word, freq = pos_av_freq$freq, min.freq = 0.05,
   #           max.words = 45, random.order = FALSE, rot.per=0,
   #           colors = cols, scale=c(3.5,0.25))
   # 
   # title(main="Top discriminating class I terms", family="Helvetica",
   #       cex.main=1.5, font.main=1)
   # par(mar = rep(1, 4))
   # wordcloud(words = neg_av_freq$word, freq = neg_av_freq$freq, min.freq = 0.05,
   #           max.words = 45, random.order = FALSE, rot.per=0,
   #           colors = cols, scale=c(3.5,0.25))
   # 
   # title(main="Top discriminating class II terms", family="Helvetica",
   #       cex.main=1.5, font.main=1)
   # 
   # 
   # # wordcloud2
   # cols  <- colorRampPalette(c("#530000", "#D0D0D0"))(25);
   # wordcloud2(data = pos_av_freq, color = rep_len(cols, nrow(pos_av_freq)),
   #            fontFamily = "Times New Roman")
   # wordcloud2(data = neg_av_freq, color = rep_len(cols, nrow(neg_av_freq)),
   #            fontFamily = "Times New Roman")
   # 
   # # ggwordcloud
   # par(mfrow = c(2, 1))
   # ggplot(data = pos_av_freq,
   #        aes(label = word, size = freq, color = freq)) +
   #    geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 1) +
   #    scale_size_area(max_size = 25) +
   #    theme_minimal() +
   #    scale_color_gradient(low = "darkred", high = "red")
   # 
   # p2 <- ggplot(data = neg_av_freq,
   #        aes(label = word, size = freq)) +
   #    geom_text_wordcloud_area() +
   #    #scale_size_area(max_size = 40) +
   #    theme_minimal()
   # 
   # grid.arrange(p1, p2, ncol=2)
   
}

