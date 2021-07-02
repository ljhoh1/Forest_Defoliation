roc_auc <- function(splits = splits) {
  # store AUCs
  aucs <- as.data.frame( matrix(NA, 6, length(levels(y_true))) )
  colnames(aucs) <- c("0 - 25 %",  "25 - 45 %",  "45 - 100 %")
  names <- data %>%
    distinct(tree_sp_eu) %>%
    arrange(tree_sp_eu)
  names[] <- lapply(names, as.character)
  row.names(scores) <- names %>% pull(tree_sp_eu)

  for (spec in 1:6) {
    d_y_train = splits[[2]][[spec]] %>%

      mutate(ints = (nbv_ratio > 0.25) + (nbv_ratio > 0.45)) %>%
      pull(ints)



    d_y_test = splits[[4]][[spec]] %>%

      mutate(ints = (nbv_ratio > 0.25) + (nbv_ratio > 0.45)) %>%
      pull(ints)

    plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
         ylab="Precision",
         xlab="Recall",
         bty='n')

    colors <- c("red", "blue", "green")
    y_true = as.factor(d_y_train)




    for (i in seq_along(levels(y_true))) {
      cur.class <- levels(y_true)[i]
      binary.labels <- as.factor(d_y_train == cur.class)
      # binarize the classifier you are using (NB is arbitrary)
      model <- randomForest( as.data.frame( splits[[1]][[spec]] ), binary.labels )
      pred <- predict(model, splits[[3]][[spec]], type='response')
      score <- as.numeric( as.logical(pred) )# posterior for  positive class
      test.labels <- d_y_test == cur.class
      pred <- prediction(score, test.labels)
      perf <- performance(pred, "prec", "rec")
      roc.x <- unlist(perf@x.values)
      roc.y <- unlist(perf@y.values)
      lines(roc.y ~ roc.x, col = colors[i], lwd = 2)
      # store AUC
      auc <- performance(pred, "auc")
      auc <- unlist(slot(auc, "y.values"))
      aucs[spec, i] <- auc
    }
    lines(x=c(0,1), c(0,1))
    legend("bottomright", levels(y_true), lty=1,
           bty="n", col = colors)
  }
  return(aucs)
}

roc_auc(splits)

