dat_dgl <- data.frame(tree_data[[1]])
dat_dgl <- one_hot(as.data.table(dat_dgl), cols = c("soil_no", "slope_dir"))
mod_dgl_0 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(25,10))
                  + s(tree_age, bs="cr", k=10),
                  data=dat_dgl,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_dgl$n_trees,
                  method="REML")

mod_dgl_1 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(25,20))
                  + s(tree_age, bs="cr", k=30) + slope_dir_1
                  + slope_dir_5 + ac_tot_wd
                  ,
                  data=dat_dgl,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_dgl$n_trees,
                  method="REML")

mod_dgl_2 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                k = c(25,5))
                 + s(tree_age, bs="cr", k=20) + slope_dir_1
                 + slope_dir_5 + s(H_bhd, bs="cr", k=10)
                 ,
                 data=dat_dgl,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_dgl$n_trees,
                 method="REML")

mod_dgl_3 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                k = c(25,5))
                 + s(tree_age, bs="cr", k=20) + slope_dir_1
                 + slope_dir_5
                 + s(ac_tot_wd, bs="cr", k=10) + s(prec_y, bs="cr", k=10) + s(prec_y_lag1, bs="cr", k=10)
                 ,
                 data=dat_dgl,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_dgl$n_trees,
                 method="REML")


par(mfrow=c(1, 2))
plot(mod_dgl_1, residuals = T)
anova(mod_dgl_1)

par(mfrow=c(1, 2))
plot(mod_dgl_2, residuals = T)
anova(mod_dgl_2)

e <- residuals(mod_dgl_1); fv <- fitted(mod_dgl_1)
lm(log(e^2) ~ log(fv))
mean(dat_dgl$nbv_ratio); mean(fitted(mod_dgl_1))

par(mfrow = c(2, 2))
gam.check(mod_dgl_1)

par(mfrow=c(1, 2))
plot(mod_dgl_1, residuals = T)
summary(mod_dgl_0)
summary(mod_dgl_1)
summary(mod_dgl_2)
summary(mod_dgl_3)

AIC(mod_dgl_3, mod_dgl_1, mod_dgl_2)
BIC(mod_dgl_3, mod_dgl_1, mod_dgl_2)

train_dgl <- dat_dgl %>%
  sample_frac(0.8)

test_dgl_X <- dat_dgl %>%
  anti_join(train_dgl) %>%
  select(-c(nbv_ratio))

test_dgl_Y <- dat_dgl %>%
  anti_join(train_dgl) %>%
  select(c(nbv_ratio))

mod_dgl_fin <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                  k = c(25,20))
                   + s(tree_age, bs="cr", k=30) + slope_dir_1 + slope_dir_5
                   + s(H_bhd, bs="cr", k=10) + s(H_spec, bs="cr", k=10),
                   data=train_dgl,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_dgl$n_trees,
                   method="REML")

y_pred <- predict(mod_dgl_fin, newdata=test_dgl_X, type = "response")
test_dgl_Y <- test_dgl_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]

par(mfrow = c(2, 2))
gam.check(mod_dgl_fin)
par(mfrow=c(1, 2))
plot(mod_dgl_1, residuals = T)
summary(mod_dgl_fin)
rmse(test_dgl_Y, y_pred)
