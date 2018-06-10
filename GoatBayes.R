#' Oblicza warunkowe prawdopodobieństwa występowania danych słów pod warunkiem danej klasy,
#' zakładając niezależność słów od siebie.
#'
#'
#' @param train_set data frame zawierający binarne wartości odpowiadające występowaniu danej  zmiennej w danym przykładzie, wymiary [n,m]
#' @param classes_set data frame o wymiarach [n,1], zawierający klasy przykładów w dataset
#'
#' @return classifier
#' @export
#'
#' @examples
#' bayes <- trainB(dataset, classes)
trainB <- function(train_set, classes_set) {
  sorted_class_set <- sort(classes_set)
  p_class <- prop.table(table(classes_set))
  unique_classes <- unique(sorted_class_set)

  total <- cbind(train_set, classes_set)

  probabilities <- list()
  for(i in 1:length(unique_classes)){
    data_class <- subset(total, classes_set == unique_classes[i])
    data_class <- subset(data_class, select = -c(classes_set))
    freq_zero_per_column <- sapply(data_class, function(c) sum(c==0))
    freq_one_per_column <- sapply(data_class, function(c) sum(c==1))
    p_zero <- (freq_zero_per_column+1) / (freq_zero_per_column+freq_one_per_column+length(unique_classes))
    p_one <- (freq_one_per_column+1)/ (freq_zero_per_column+freq_one_per_column+length(unique_classes))
    probabilities <- cbind(probabilities, p_zero, p_one)
  }
  cols <- colnames(data_class)
  rownames(probabilities) <- cols
  result <- list(p_class=p_class, probabilities=probabilities)
  return (result)
}

#' Oblicza warunkowe prawdopodobieństwa występowania danych słów pod warunkiem danej klasy,
#' zakładając niezależność słów od siebie.
#'
#'
#' @param train_set data frame zawierający binarne wartości odpowiadające występowaniu danej  zmiennej w danym przykładzie, wymiary [n,m]
#' @param classes_set data frame o wymiarach [n,1], zawierający klasy przykładów w dataset
#'
#' @return classifier
#' @export
#'
#' @examples
#' bayes <- trainM(dataset, classes)
trainM <- function(train_set, classes_set) {
  indx <- sapply(train_set, is.factor)
  train_set[indx] <- lapply(train_set[indx], function(x) as.numeric(as.character(x)))
  p_class <- prop.table(table(classes_set))
  total <- cbind(train_set, classes_set)
  unique_classes <- sort(unique(classes_set))

  probabilities <- list()
  for(i in 1:length(unique_classes)){
    data_class <- subset(total, classes_set == unique_classes[i])
    data_class <- subset(data_class, select = -c(classes_set))
    freq_word_of_given_class <- sapply(data_class, function(c) sum(c))
    all_words_of_given_class <- sum(freq_word_of_given_class)
    p_word_of_given_class <- (freq_word_of_given_class +1)/ (all_words_of_given_class +ncol(train_set))
    probabilities <- cbind(probabilities, p_word_of_given_class)
  }
  cols <- colnames(data_class)
  rownames(probabilities) <- cols
  result <- list(p_class=p_class, probabilities=probabilities)
  return (result)
}

#' Przewiduje wartości przynależności do klas na podstawie modelu wytrenowanego metodą trainM
#'
#' @param classifier obiekt zwracany przez funkcję trainM
#' @param dataset data frame przykładów do predykcji
#' @param type jeśli podany argument “raw” to zwracane są wartości prawdopodobieństw dla każdej z klas, jeśli nie to zwracana jest tylko przewidziana klasa
#'
#' @return results
#' @export
#'
#' @examples
#' results <- predictM(classifier, dataset, type="raw")
predictM <- function(classifier, dataset, type="simple"){
  indx <- sapply(dataset, is.factor)
  dataset[indx] <- lapply(dataset[indx], function(x) as.numeric(as.character(x)))

  results <- list()
  for(j in 1:length(classifier$p_class)){
    class_probabilities = as.data.frame(matrix(ncol=1, nrow = nrow(dataset)))
    class_probabilities <- classifier$p_class[j]
    for(i in names(dataset)){
      class_probabilities <- class_probabilities * (classifier$probabilities[[i, j]] ** dataset[,i])
    }
    results <- cbind(results, class_probabilities)
  }
  colnames(results) <- names(classifier$p_class)
  if(type=="raw"){
    return(results)
  }
  else{
    return(choose_max_class(results))
  }
}

#' Przewiduje wartości przynależności do klas na podstawie modelu wytrenowanego metodą trainB
#'
#' @param classifier obiekt zwracany przez funkcję trainB
#' @param dataset data frame przykładów do predykcji
#' @param type jeśli podany argument “raw” to zwracane są wartości prawdopodobieństw dla każdej z klas, jeśli nie to zwracana jest tylko przewidziana klasa
#'
#' @return results
#' @export
#'
#' @examples
#' results <- predictB(classifier, dataset, type="raw")
predictB <- function(classifier, dataset, type="simple") {
  indx <- sapply(dataset, is.factor)
  dataset[indx] <- lapply(dataset[indx], function(x) as.numeric(as.character(x)))
  results <- list()
  for(j in 1:length(classifier$p_class)){
    idx_p_zero <- 2*j-1
    idx_p_one <- 2*j
    classes_set <- dataset
    for(i in names(dataset)){
      classes_set[,i][classes_set[,i] == 1] <- classifier$probabilities[[i, idx_p_one]]
      classes_set[,i][classes_set[,i] == 0] <- classifier$probabilities[[i, idx_p_zero]]
    }
    log_class <- apply(classes_set, 1, FUN=function(x) sum(log(x)))
    log_class <- log_class + log(classifier$p_class[j])
    results <- cbind(results, log_class)
  }
  colnames(results) <- names(classifier$p_class)
  if(type=="raw"){
    return(results)
  }
  else{
    return(choose_max_class(results))
  }
}

#'
#' Funkcja która na podstawie prawdopodobieństw klas wybiera klasę o najwyższym prawdopodobieństwie
#'
#' @param predictions obiekt zwracany przez funkcję predictB lub predictM z ustawioną flagą type="raw"
#'
#' @return classes
#' @export
#'
#' @examples
#' classes <- choose_max_class(predictions)
choose_max_class <- function(predictions) {
  a <- apply(predictions,1,which.max)
  return(colnames(predictions)[a])
}