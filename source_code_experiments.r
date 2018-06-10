library(tm)
library(readtext)
library(SnowballC)
library(printr)
library(Matrix)
library(plyr)
library(e1071)
library(tictoc)
library(ROCR)
library(GoatBayes)
setwd("path")
firstpath <- file.path("path", "folder")
secondpath <- file.path("path", "folder")

paths <- list(firstpath, secondpath)
classes_names <- list("ok", "neg")

preprocess_data <- function(paths, classes_names, type, min=0, max=5000) {
  
  for(i in 1:length(paths)){
    path <- paths[[i]]
    docs <- VCorpus(DirSource(path))
    docs <- tm_map(docs, function(x) iconv(enc2utf8(x$content), sub = "byte"))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, tolower)
    docs <- tm_map(docs, PlainTextDocument)
    dtm <- DocumentTermMatrix(docs)
    dtm_matrix <- as.matrix(dtm)
    m_df_temp <- as.data.frame(dtm_matrix, stringsAsFactors = False, row.names=NULL)
    m_classes_df_temp <- as.list(rep(classes_names[[i]], nrow(m_df_temp)))
    rownames(m_df_temp) <- c()
    if(i==1){
      m_df <- m_df_temp
      m_classes_df <- m_classes_df_temp
    }
    else {
      m_df <- rbind.fill(m_df, m_df_temp)
      m_classes_df <- c(m_classes_df, m_classes_df_temp)
    }
  }
  m_df[is.na(m_df)] <- 0
  if(min !=0  && max !=5000) {
    m_df <- delete_most_and_least_frequent_words(m_df, min, max)
  }
  if(type == "B"){
    m_df[m_df > 1] <- 1
  }
  m_df[] <- lapply(m_df, factor)
  m_classes_df <- unlist(m_classes_df)
  
  return(list(m_df, m_classes_df))
}

kfold <- function(dataset, classes_set, k, train_func, predict_func){
  train <- match.fun(train_func)
  predict <- match.fun(predict_func)
  total <- cbind(dataset, classes_set)
  total_shuffled <- total[sample(nrow(total)),]
  range <- floor(nrow(total_shuffled) / k)
  unique_classes <- sort(unique(classes_set))
  
  for(i in 1:k) {
    before <- total_shuffled[0 : ((i-1)*range), ]
    validate_set <- total_shuffled[(range*(i-1)+1): (i*range), ]
    after <- total_shuffled[(i*range+1) : (nrow(dataset)), ]
    if(i==k){
      train_set <-before
    }
    else {
      train_set <- rbind(before, after)
    }
    bayes <- train(train_set[,1:(ncol(train_set)-1)], train_set[,ncol(train_set)])
    true_classes <- validate_set[,ncol(validate_set)]
    true_classes<-data.frame(true_classes = unlist(true_classes))
    predictions <- predict(bayes, validate_set[,1:(ncol(train_set)-1)])
    print(i)
    if(i==1){
      confusion_matrix <- create_confusion_matrix(predictions, true_classes, unique_classes)
    }
    else{
      confusion_matrix <- confusion_matrix + create_confusion_matrix(predictions, true_classes, unique_classes)
    }
  }

  return(confusion_matrix)
}

choose_max_class <- function(classes) {
  a <- apply(classes,1,which.max)
  return(colnames(classes)[a])
}

print_statistics <- function(confusion_matrix){
  print(confusion_matrix)
  tn = confusion_matrix[1, 1]
  fn = confusion_matrix[1, 2]
  fp = confusion_matrix[2, 1]
  tp = confusion_matrix[2, 2]
  error = (fp+fn)/(tp+tn+fp+fn)
  accuracy = (tp+tn)/(tp+tn+fp+fn)
  tp_rate = tp/(tp+fn)
  fp_rate = fp/(tn+fp)
  recall = tp_rate
  precision = tp/(tp+fp)
  
  print("Error")
  print(error)
  print("Accuracy")
  print(accuracy)
  print("TP Rate")
  print(tp_rate)
  print("FP Rate")
  print(fp_rate)
  print("Recall")
  print(recall)
  print("Precision")
  print(precision)
  
}

create_confusion_matrix <- function(predictions, true_classes, unique_classes){
  
  confusion_matrix <- as.data.frame(matrix(ncol=2, nrow = 2))
  confusion_matrix[,] <- 0
  p <- unique_classes[1]
  n <- unique_classes[2]
  colnames(confusion_matrix) <- c(p, n)
  rownames(confusion_matrix) <- c(p, n)
  
  
  for(j in 1:length(predictions)){
    if(true_classes[j,] == predictions[j] && true_classes[j,]==p){
      confusion_matrix[p,p] <- confusion_matrix[p,p]+ 1
    }
    
    else if(true_classes[j,] == predictions[j] && true_classes[j,]==n){
      confusion_matrix[n,n] <-  confusion_matrix[n,n]+ 1
    }
    
    else if(true_classes[j,] != predictions[j] && true_classes[j,]==n){
      confusion_matrix[p,n] <-  confusion_matrix[p,n]+ 1
    }
    
    else if(true_classes[j,] != predictions[j] && true_classes[j,]==p){
      confusion_matrix[n,p] <-  confusion_matrix[n,p]+ 1
    }
    
  }
  return(confusion_matrix)
}

delete_most_and_least_frequent_words <- function(dataset, min, max){
  indx <- sapply(dataset, is.factor)
  dataset[indx] <- lapply(dataset[indx], function(x) as.numeric(as.character(x)))
  occurences  <- ldply(dataset, function(c) sum(c))
  hist(occurences$V1)
  a <- occurences[occurences$V1 < min, ]
  b <- occurences[occurences$V1 >max, ]
  withouta <- dataset[, -which(colnames(dataset) %in% a$.id)]
  withoutb <- withouta[, -which(colnames(withouta) %in% b$.id)]
  return (withoutb)
}

create_roc_curve <- function(dataset, classes, train_func, predict_func){
  train <- match.fun(train_func)
  predict <- match.fun(predict_func)
  
  total <- cbind(dataset, classes)
  total_shuffled <- total[sample(nrow(total)),]
  dataset <- total_shuffled[,1:(ncol(total_shuffled)-1)]
  classes <- total_shuffled[,ncol(total_shuffled)]
  
  train_set_len <- floor(nrow(dataset)*2/3)
  train_set <- dataset[1:train_set_len,]
  validate_set <- dataset[(train_set_len+1):nrow(dataset), ]
  train_classes_set <- classes[1:train_set_len]
  validate_classes_set <- classes[(train_set_len+1):nrow(dataset)] 
  bayes <- train(train_set, train_classes_set)
  predictions <- predict(bayes, validate_set, type="raw")
  predictions <- unlist(predictions[,2]) - unlist(predictions[,1])
  pred <- prediction(predictions, validate_classes_set)
  roc <- performance(pred, "tpr", "fpr")
  auc <- performance(pred, "auc")
  print(auc)
  plot(roc, type="l", col="blue", lwd=5)
  
}

#Multinomial bayes - example call
data <- preprocess_data(paths, classes_names, "M", 10, 100)
dataset <- data[[1]]
classes <- data[[2]]
print_statistics(kfold(dataset, classes, 5, trainM, predictM))
create_roc_curve(dataset, classes, trainM, predictM)

#Bernoulli bayes - example call
data <- preprocess_data(paths, classes_names, "B", 10, 100)
dataset <- data[[1]]
classes <- data[[2]]
print_statistics(kfold(dataset, classes, 5, trainB, predictB))
create_roc_curve(dataset, classes, trainB, predictB)


#e1071 bayes - example call
print_statistics(kfold(dataset, classes, 5, naiveBayes, predict))
create_roc_curve(dataset, classes, naiveBayes, predict)
