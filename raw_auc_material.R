library(ROCR) # for ROC curves
library(klaR) # for NaiveBayes
data(iris) # Species variable gives the classes
response <- iris$Species
set.seed(12345)
train.idx <- sample(seq_len(nrow(iris)), 0.6 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab="Precision",
     xlab="Recall",
     bty='n')
colors <- c("red", "blue", "green")
aucs_a <- rep(NA, length(levels(response))) # store AUCs
for (i in seq_along(levels(response))) {
  cur.class_a <- levels(response)[i]
  binary.labels_a <- as.factor(iris.train$Species == cur.class_a)
  # binarize the classifier you are using (NB is arbitrary)
  model_a <- NaiveBayes(binary.labels_a ~ ., data = iris.train[, -5])
  pred_a <- predict(model_a, iris.test[,-5], type='raw')
  score_a <- pred_a$posterior[, 'TRUE'] # posterior for  positive class
  test.labels_a <- iris.test$Species == cur.class_a
  pred_a <- prediction(score_a, test.labels_a)
  perf_a <- performance(pred_a, "prec", "rec")
  roc.x_a <- unlist(perf_a@x.values)
  roc.y_a <- unlist(perf_a@y.values)
  lines(roc.y_a ~ roc.x_a, col = colors[i], lwd = 2)
  # store AUC
  auc_a <- performance(pred_a, "auc")
  auc_a <- unlist(slot(auc_a, "y.values"))
  aucs_a[i] <- auc_a
}
lines(x=c(0,1), c(0,1))
legend("bottomright", levels(response), lty=1,
       bty="n", col = colors)
