library(OpenML) # Load the OpenML package
# Ensure you install and load other required packages, e.g., dplyr for data manipulation

data_openML <- function(openML_id) {
  # Load the dataset from OpenML
  dataset <- getOMLDataSet(data.id = openML_id)
  df <- dataset$data
  
  # Rename the target variable to 'Outcome'
  target_var <- dataset$desc$default.target.attribute
  names(df)[names(df) == target_var] <- 'Outcome'
  
  # Remove ignored attributes, if any
  if (!is.na(dataset$desc$ignore.attribute)) {
    ignore_attrs <- unlist(strsplit(dataset$desc$ignore.attribute, ", "))
    df <- df[, !(names(df) %in% ignore_attrs)]
  }
  
  # Filter out records with missing 'Outcome'
  df <- df[!is.na(df$Outcome), ]
  
  # Convert 'Outcome' to 0 and 1 based on its unique values
  outcome_levels <- sort(unique(df$Outcome))
  if (length(outcome_levels) == 2) {
    df$Outcome <- as.integer(df$Outcome == outcome_levels[2])
  } else {
    stop("Outcome variable does not have two unique values for binary classification.")
  }
  
  # Remove variables with more than 20% missing values
  threshold <- 0.8 # Keep columns with at least 80% non-NA values
  df <- df[, colMeans(!is.na(df)) > threshold]
  
  # Process categorical and numeric variables (simplified example)
  df <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      if (length(unique(x)) > 2) {
        factor(x)
      } else {
        as.integer(as.factor(x)) - 1 # Convert binary categorical variables to 0 and 1
      }
    } else {
      x
    }
  })
  
  # Convert list back to dataframe
  df <- as.data.frame(df)
  
  # Simple imputation for missing data (mean for numeric, mode for categorical)
  # This is a placeholder - consider using more sophisticated methods like missForest as in your original code
  df <- lapply(df, function(x) {
    if (is.numeric(x)) {
      if (any(is.na(x))) { x[is.na(x)] <- mean(x, na.rm = TRUE) }
    } else if (is.factor(x)) {
      mode_val <- names(sort(table(x), decreasing = TRUE))[1]
      x[is.na(x)] <- mode_val
    }
    x
  })
  
  df <- as.data.frame(df)
  
  # Return the processed dataframe
  return(df)
}
