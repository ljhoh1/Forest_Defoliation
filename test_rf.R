# evaluate the model
evaluate <- function(models, X_test, y_test) {
  # Evaluate the accuracy, precision and recall
  #
  # Inputs:
  # - model(prediction model)   <- the model summary statistics are to be found for
  # - X_test (feature matrix)   <- test dataset, containing the feature information
  # - Y_test (response vector)  <- test dataset, containing the response information
  #
  # Outputs:
  #   - scores (data frame)       <- data frame containing accuracy, precision, recall scores
  scores <- as.data.frame(matrix(NA, 6, 1))
  colnames(scores) <- c("R-Squared")
  names <- data %>%
    distinct(tree_sp_eu) %>%
    arrange(tree_sp_eu)
  names[] <- lapply(names, as.character)

  row.names(scores) <- names %>% pull(tree_sp_eu)

  for (mod in 1:length(models)) {

    # make predictions
    y_hat <-  as.data.frame( list( as.vector( unname( predict(models[[mod]],
                                                              newdata=X_test[[mod]]) ) ) ),
                             col.names = "per")
    y_true <- y_test[[mod]]
    #con_M <- confusionMatrix(as.factor(y_hat), as.factor(y_true))$table

    # find accuracy, precision and recall
    scores[mod,] <- c(#sum(diag(con_M)) / sum(con_M),
                      #diag(con_M) / rowSums(con_M),
                      #diag(con_M) / colSums(con_M),
                      mean(models[[mod]]$rsq))


  }
  return(scores)
}

evaluate(models, X_test=splits[[3]], y_test=splits[[4]])
