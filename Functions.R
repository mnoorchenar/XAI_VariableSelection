#####Loading Package#####
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("Metrics", quietly = TRUE)) install.packages("Metrics")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")

if (!require("OpenML")) {install.packages("OpenML"); require("OpenML");}
if (!require("farff")) {install.packages("farff"); require("farff");}
if (!require("missForest")) {install.packages("missForest"); require("missForest");}
if (!require("caret")) {install.packages("caret"); require("caret");}
if (!require("e1071")) {install.packages("e1071"); require("e1071");}
if (!require("nnet")) {install.packages("nnet"); require("nnet");}
if (!require("fdm2id")) {install.packages("fdm2id"); require("fdm2id");}
if (!require("rpart")) {install.packages("rpart"); require("rpart");}
if (!require("pROC")) {install.packages("pROC"); require("pROC");}
if (!require("RSQLite")) {install.packages("RSQLite"); require("RSQLite");}
if (!require("outliers")) {install.packages("outliers"); require("outliers");}


my_openML <- function(openML_id) {
  # Load data from OpenML
  df <- getOMLDataSet(data.id = openML_id)$data
  
  # Change name of target variable
  target_attribute <- getOMLDataSet(data.id = openML_id)$desc$default.target.attribute
  names(df)[names(df) == target_attribute] <- 'Outcome'
  
  # Remove ignored attributes, if any
  if (!is.na(getOMLDataSet(data.id = openML_id)$desc$ignore.attribute)) {
    ignore_attributes <- unlist(strsplit(getOMLDataSet(data.id = openML_id)$desc$ignore.attribute, ", "))
    df <- df[ , !(names(df) %in% ignore_attributes)]
  }
  
  # Removing records with missing 'Outcome'
  df <- df[!is.na(df$Outcome), ]
  
  # Removing variables with 20% missing
  df <- df[, colMeans(!is.na(df)) > 0.8]
  
  # Initialize vectors to store names of numeric and factor variables
  df_numeric_names <- c()
  df_factor_names <- c()
  
  for (i in names(df)) {
    # Handling character variables
    if (is.character(df[[i]])) {
      counts <- table(df[[i]])
      less_frequent <- names(counts)[counts < round(nrow(df) * 0.05)]
      # Replace less frequent values with the most frequent value
      for (value in less_frequent) {
        df[[i]][df[[i]] == value] <- names(which.max(counts))
      }
      df[[i]] <- factor(df[[i]])
      df_factor_names <- c(df_factor_names, i)
    }
    # Handling numeric variables
    else if (is.numeric(df[[i]]) && length(unique(df[[i]])) > 10) {
      df_numeric_names <- c(df_numeric_names, i)
    }
    # Remove variables with only one unique value
    else if (length(unique(df[[i]])) == 1) {
      df[[i]] <- NULL
    }
    # Handling binary variables
    else if (length(unique(df[[i]])) == 2) {
      df[[i]] <- as.numeric(as.factor(df[[i]])) - 1
    }
    # Assuming remaining are multinomial variables
    else {
      df[[i]] <- factor(df[[i]])
      df_factor_names <- c(df_factor_names, i)
    }
  }
  
  # Impute missing data (Placeholder for missForest or other imputation methods)
  # df <- missForest(df)$ximp
  
  #return(list(data = df, NumericalVars = df_numeric_names, FactorVars = df_factor_names))
  return(df)
}

normalize_min_max <- function(train, test) {
  # Directly identify numeric columns, excluding 'Outcome'
  numeric_cols <- sapply(train, is.numeric) & names(train) != "Outcome"
  
  for(col_name in names(train)[numeric_cols]) {
    min_val <- min(train[[col_name]], na.rm = TRUE)
    max_val <- max(train[[col_name]], na.rm = TRUE)
    # Normalize only if there's variation in the data to prevent division by zero
    if(min_val < max_val) {
      train[[col_name]] <- (train[[col_name]] - min_val) / (max_val - min_val)
      test[[col_name]] <- (test[[col_name]] - min_val) / (max_val - min_val)
    }
  }
  list(train = train, test = test)
}


# Updated function to prepare data with stratified sampling for the 'Outcome' variable
my_cv <- function(df, nfolds = 10, normalize = FALSE) {
  # Ensure 'Outcome' is a factor for stratification
  #df$Outcome <- as.factor(df$Outcome)
  
  # Create stratified folds
  set.seed(123)
  folds <- createFolds(df$Outcome, k = nfolds, list = TRUE, returnTrain = FALSE)
  
  # Step 1 & 2: Collect all unique levels for each factor variable across folds and compare with overall levels
  factor_columns <- colnames(df)[sapply(df, is.factor)]
  inconsistent_vars <- character()
  
  for(col in factor_columns) {
    overall_levels <- levels(df[[col]])
    
    for (cv in 1:nfolds) {
      if(length(levels(df[,col])) != length(levels(df[folds[[cv]],col]))){
        inconsistent_vars <- c(inconsistent_vars, col)
      }
    }

    }
  
  
  # Step 3: Remove inconsistent variables from df
  df <- df[, !(colnames(df) %in% inconsistent_vars)]
  
  dummy_df <- model.matrix(~ . -1 - Outcome, data = df)
  dummy_df <- as.data.frame(dummy_df)
  dummy_df$Outcome <- df$Outcome
  dummy_df[] <- lapply(dummy_df, function(x) as.numeric(x))
  
  
  cv_train = list()
  cv_test = list()

  # Function for Min-Max normalization
  for(i in seq_along(folds)) {
    train_indices = unlist(folds[-i])
    test_indices = folds[[i]]
    
    train_data <- dummy_df[train_indices,]
    test_data <- dummy_df[test_indices,]
    
    # Normalize if requested
    if(normalize) {
      normalized_data <- normalize_min_max(train_data, test_data)
      train_data <- normalized_data$train
      test_data <- normalized_data$test
    }
    
    cv_train[[i]] <- train_data
    cv_test[[i]] <- test_data
}
  return(list('train' = cv_train, 'test' = cv_test))
}


#####Machine Learning Algorithms#####
my_LogisticRegression <- function(cv_data) {
  pred <- c()
  real <- c()
  
  for (i in 1:length(cv_data$train)) {
    # Fit the model
    Mymodel <- glm(Outcome ~ ., data = cv_data$train[[i]][,'V7581'], family = binomial())
    # Make predictions on the test set
    preds <- predict(Mymodel, newdata = cv_data$test[[i]], type = "response")[1]
    pred <- append(pred, preds)
    real <- append(real, cv_data$test[[i]]$Outcome)
  }
  
  return(list('pred' = pred, 'real' = real))
}


my_RandomForest <- function(cv_data) {
  pred <- c()
  real <- c()
  for (i in 1:length(cv_data$train)) {
    cv_data$train[[i]]$Outcome = as.factor(cv_data$train[[i]]$Outcome)
    cv_data$test[[i]]$Outcome = as.factor(cv_data$test[[i]]$Outcome)   
    
    Mymodel <- randomForest::randomForest(Outcome ~ ., data=cv_data$train[[i]])
    
    pred = append(pred, predict(object = Mymodel,newdata = cv_data$test[[i]][,-which(names(cv_data$test[[i]]) %in% c("Outcome"))], type="prob")[,2])
    real <- append(real, cv_data$test[[i]]$Outcome)
  }
  result <- cbind.data.frame('pred' = pred, 'real' = real)
  return(result)
}

my_SupportVectorMachines <- function(train, test, nfolds=10) {
  prediction_probability = c()
  for (i in 1:nfolds) {
    Mymodel <- e1071::svm(Outcome ~ ., data=train[[i]])
    prediction_probability = append(prediction_probability, predict(object = Mymodel,newdata = test[[i]][,-which(names(test[[i]]) %in% c("Outcome"))]))
  }
  return(prediction_probability)
}

my_SingleLayerPerceptron <- function(train, test, nfolds=10) {
  prediction_probability = c()
  for (i in 1:nfolds) {
    Mymodel <- nnet::nnet(Outcome ~ ., data=train[[i]],size=5, decay=5e-4, maxit=1000, trace=FALSE)
    prediction_probability = append(prediction_probability, predict(object = Mymodel,newdata = test[[i]][,-which(names(test[[i]]) %in% c("Outcome"))]))
  }
  return(prediction_probability)
}

my_NaiveBayesClassifier <- function(train, test, nfolds=10) {
  prediction_probability = c()
  for (i in 1:nfolds) {
    Mymodel <- e1071::naiveBayes(Outcome ~ ., data=train[[i]])
    prediction_probability = append(prediction_probability, predict(object = Mymodel,newdata = test[[i]][,-which(names(test[[i]]) %in% c("Outcome"))], type='raw')[,2])
  }
  return(prediction_probability)
}

my_RegressionTrees <- function(train, test, nfolds=10) {
  prediction_probability = c()
  for (i in 1:nfolds) {
    Mymodel <- rpart::rpart(Outcome ~ ., data=train[[i]], method="class")
    prediction_probability = append(prediction_probability, predict(object = Mymodel,newdata = test[[i]][,-which(names(test[[i]]) %in% c("Outcome"))])[,2])
  }
  return(prediction_probability)
}

