# missing obs
require(mice)
require(JointAI)

# correlation plot and data tools
require(dplyr)
require(tidyr)
require(ggplot2)
require(reshape2)

# figures
require(RColorBrewer)
require(forcats)

# gam
require(mgcv)

# random forest
require(randomForest)
require(MLmetrics)
require(caret)

raw = read.table("foresthealth.txt", sep = ";", header = T, stringsAsFactors = T)
data = raw[,!names(raw) %in% "X" ]

dtype = as.data.frame(sapply(data,class))
rownames(dtype) <- 1:nrow(dtype); colnames(dtype) = c("obs_type")
dtype["index"] = colnames(data)
meaning = read.table("variablesNHA.csv", sep = ";", header = T)[, c(1, 3, 4)]
row74 = c("Es", "Evenness Index tree species", "numeric")
row75 = c("H_bhd", "Shannon's function tree diameter", "numeric")
meaning = rbind(meaning, c(row74))
meaning = rbind(meaning, c(row75))
colnames(meaning) = c("index", "description", "real_type")

desc = merge(dtype, meaning, by = "index")

for (row in 1:dim(desc)[1]) {
  dtyp_imp = desc[row, 2]
  dtyp_tru = desc[row, 4]
  if (dtyp_imp != dtyp_tru) {
    #print("a")
    if (grepl("ordered factor", dtyp_tru)) {
      data[ , desc[row, 1]] = factor( as.vector( data[ , desc[row, 1]] ) ,
                                   ordered = is.ordered(data[ , desc[row, 1]]))
    } else if (grepl("factor", dtyp_tru)) {
      data[ , desc[row, 1]] = factor( as.vector( data[ , desc[row, 1]] ) )
    } else if (grepl("numeric", dtyp_tru)) {
      data[ , desc[row, 1]] = as.numeric( as.vector( data[ , desc[row, 1]] ) )
    }
  }
}

dtype2 = as.data.frame(sapply(data,class))
rownames(dtype2) <- 1:nrow(dtype2); colnames(dtype2) = c("obs_type")
dtype2["index"] = colnames(data)
meaning2 = read.table("variablesNHA.csv", sep = ";", header = T, stringsAsFactors = T)[, c(1, 3, 4)]
meaning2 = rbind(meaning2, c(row74))
meaning2 = rbind(meaning2, c(row75))
colnames(meaning2) = c("index", "description", "real_type")

desc2 = merge(dtype2, meaning2, by = "index")

summary(data)


# visualisation of missing values
dd= data
iter = 0
for (col in colnames(dd) ) {
  iter = iter + 1
  if (typeof(dd[1, col]) == "str") {

  }
  else if (sum( is.na( as.vector(dd[, col]) ) ) == 0) {
    dd = dd[-iter]
    iter = iter - 1
  }
  else if (sum( is.na( as.vector(dd[, col]) ) ) > 0) {
  }
}

md_pattern(dd, pattern = FALSE, color = c('#34111b', '#e30f41'), print_yaxis = F)
sum(complete.cases(data))


# correlation plot
dd = data %>%
  select(order(colnames(data)))
dd = dd[ , -which(desc2[,2] %in% c("factor"))]

cormat <- round(cor(dd[complete.cases(dd),]),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))




figures = function(data, var, colours =  c("#FF5733", " #9625FA"), xlab, ylab, title) {
  total = as.numeric( length( dim( data )[1] ) )
  data <- data %>%
    select( {{var}} ) %>%
    mutate( lev = str( get(var) ) ) %>%
    add_count( get(var) ) %>%
    unique() %>%
    mutate( get(var), dens =  n / dim(data)[1]) %>%

    filter( dens>=0.01 ) %>%
    mutate( lev = fct_explicit_na( get(var), "No Response" ) ) %>%
    arrange( desc(dens) )

  m = ggplot(data, aes(x=reorder(lev, -n), y=dens, fill=reorder(lev, -n))) +

        geom_bar(stat = "identity") +
        scale_fill_manual( values = c(brewer.pal(n=11, "Spectral"),
                                      brewer.pal(n=10, "PiYG") ),
                           name = paste0("Legend: \n", xlab ) ) +
        geom_text( aes( label=round(dens,2), y = dens + 0.01), vjust=0, color="black",
              position = position_dodge(0.9), size=2.5) +

        theme(legend.position = "right") +
        #labs( color = var ) +
        xlab(xlab) +
        ylab(ylab) +
        ggtitle(title)
  m
}

# figure 1: Distribution of Frutification Levels
figures(data, var = "fruct_lev", xlab="Frutification Level", ylab="Percentage",
         title="Distribution of Frutification Levels")

# figure 2: Distribution of Tree Species in Europe
figures(data, var = "tree_sp_eu", xlab="Tree Species", ylab="Percentage",
        title="Distribution of Tree Species in Europe")

# figure 3: Distribution of Hummus Type
figures(data, var = "humus_no", xlab="Hummus Type", ylab="Percentage",
        title="Distribution of Hummus Type")

# tree age intervals
tree_ints <- data %>%
  select(tree_age) %>%
  mutate(ints = cut(tree_age, 4 ))

# figure 4: Distribution of Tree Age
figures(tree_ints, var = "ints", xlab="Tree Age", ylab="Percentage",
        title="Distribution of Tree Age")


# split into train and test
train_test_split <- function(data, train_size=0.75) {


  # splitting data into groups
  tree_species_prep <- data %>%
    select(-c(id, x_utm, y_utm, sw, gw, fruct_lev, spat, s_veg, d_veg)) %>%
    na.omit()

  tree_species_train <- tree_species_prep %>%
    sample_frac(train_size)

  Y_train <- tree_species_train %>%
    select(c(nbv_ratio, tree_sp_eu)) %>%
    group_split(tree_sp_eu)

  X_train <- tree_species_train %>%
    select(-c(nbv_ratio)) %>%
    group_split(tree_sp_eu)

  tree_species_test <- tree_species_prep %>%
    anti_join(tree_species_train)

  Y_test <- tree_species_test %>%
    select(c(nbv_ratio, tree_sp_eu)) %>%
    group_split(tree_sp_eu)

  X_test <- tree_species_test %>%
    select(-c(nbv_ratio)) %>%
    group_split(tree_sp_eu)

  return(list(X_train, Y_train, X_test, Y_test))

}

splits = train_test_split(data, train_size=0.75)


model_rf <- function(spec_data_X, spec_data_y) {
  models = list()
  for (id in 1:length(spec_data_X)) {
    d_y = spec_data_y[[1]] %>%

      #mutate(ints = (nbv_ratio > 0.25) + (nbv_ratio > 0.45)) %>%
      pull(nbv_ratio)

    print( dim( as.data.frame( spec_data_X[[id]] ) ) )
    print( length( d_y ) )
    models[[id]] = randomForest( as.data.frame( spec_data_X[[1]] ), d_y )
  }
  return(models)
}

models <- model_rf(spec_data_X = splits[[1]], spec_data_y = splits[[2]])

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
  scores <- as.data.frame(matrix(NA, 6, 4))
  colnames(scores) <- c("Accuracy",  "Precision",  "Recall", "R-Squared")
  names <- data %>%
    distinct(tree_sp_eu) %>%
    arrange(tree_sp_eu)
  names[] <- lapply(names, as.character)

  row.names(scores) <- names %>% pull(tree_sp_eu)

  for (mod in 1:length(models)) {

    # make predictions
    y_hat <-  as.data.frame( list( as.vector( unname( predict(models[[mod]],
                                                              newdata=X_test[[mod]]) ) ) ),
                             col.names = "per") %>%
      mutate(ints = (per > 0.25) +  (per > 0.45)) %>%
      pull(ints)
    y_true <- y_test[[mod]] %>%
      mutate(ints = (nbv_ratio > 0.25) + (nbv_ratio > 0.45)) %>%
      pull(ints)
    con_M <- confusionMatrix(as.factor(y_hat), as.factor(y_true))$table

    # find accuracy, precision and recall
    scores[mod,] <- c(sum(diag(con_M)) / sum(con_M),
                      diag(con_M) / rowSums(con_M),
                      diag(con_M) / colSums(con_M),
                      models[[mod]]$rsq)


  }
  return(scores)
}

evaluate(models, X_test=splits[[3]], y_test=splits[[4]])

require(ROCR)

roc_auc <- function(splits = splits, test = 0) {
  # Evaluate the AUC scores and plot auc curve
  #
  # Inputs:
  # - splits(data frame)   <- data used for prediction
  # - test (integer)   <- values [0, 2], indicating whether test AUC is estimated
  #
  # Outputs:
  #   - aucs (data frame)       <- data frame containing AUCS


  # store AUCs
  aucs <- matrix(NA, 6, 3)
  colnames(aucs) <- c("0 - 25 %",  "25 - 45 %",  "45 - 100 %")
  names <- data %>%
    distinct(tree_sp_eu) %>%
    arrange(tree_sp_eu)
  names[] <- lapply(names, as.character)

  row.names(aucs) <- names %>% pull(tree_sp_eu)

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
      pred <- predict(model, splits[[1+test]][[spec]], type='response')
      score <- as.numeric( as.logical(pred) )# posterior for  positive class
      if (test == 2) {
        test.labels <- d_y_test == cur.class
      } else {
        test.labels <- d_y_train == cur.class
      }
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


# feature importance
feat_imp <- function(mods = models) {
  impi = as.data.frame( round(importance(mods[[1]]), 2) )

  imp_df <- as.data.frame(impi)

  for (mod in 2:6) {
    impi = as.data.frame( round(importance(mods[[mod]]), 2) )

    imp_df[,mod] <- impi
  }
  imp_df = as.data.frame(imp_df)
  names <- data %>%
    distinct(tree_sp_eu) %>%
    arrange(tree_sp_eu)
  names[] <- lapply(names, as.character)

  colnames(imp_df) <- names %>% pull(tree_sp_eu)
  imp_df <- imp_df %>%
    arrange(desc(Gfi))
  return(imp_df)
}

importance = feat_imp()


# data preperation for gamm
tree_species_mean <- function(data, feat_imp = importance) {
  # temporal-spatio model
  data$spat <- factor(paste0(data$x_utm, data$y_utm))

  # splitting data into groups
  tree_species_list <- data %>%
    group_split(tree_sp_eu)

  # number of species
  tree_species = data %>%
    distinct(tree_sp_eu)

  # feature importance prep
  feat_imp <- feat_imp %>%
    rownames_to_column("feature")

  # features extracted

  # seperate the data into tree species and find unique values (averaging)
  tree_data = list(tree_species_list, tree_species)
  for (spec in 1:dim(tree_species)[1] ) {

    feat_imp_spec <- as.data.frame( feat_imp[ , c(1, spec + 1)] ) %>%
      mutate(perc = feat_imp[,spec + 1] / sum (feat_imp[,spec + 1]) ) %>%
      filter(perc > 0.015) %>%

      arrange(desc(perc))

    tree_data[[spec]] <- tree_species_list[[spec]][,c("x_utm", "y_utm", "year", "spat", "nbv_ratio", feat_imp_spec[,1])]

    tree_data[[spec]] <- tree_data[[spec]][, !duplicated(colnames(tree_data[[spec]]))] %>%
      group_by(year, spat, x_utm, y_utm) %>%
      mutate(across(where(is.double), mean)) %>%
      distinct()
  }
  return(tree_data)
}

tree_data = tree_species_mean(data)

# gamm function
model_func <- function(data, dof = c(25,5), bs= c("tp","tp")) {
  models = list()
  i = 1
  for (tree_spec in data) {
    print(as.data.frame(tree_spec))
    models[[i]] <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                       k = dof)#, gamma=1.4)
                        + s(tree_age, bs="cr", k=10), data=as.data.frame(tree_spec),
                        correlation = corARMA(form =~ year | spat, p=1, q=1),
                        family = gaussian(link="logit"),
                        weights = as.data.frame(tree_spec)$n_trees,
                        method="REML")

    par(mfrow=c(1, 2))
    plot(models[[i]]$gam, residuals = T)
    i = i + 1
  }
  return(models)
}

model_func(tree_data)
