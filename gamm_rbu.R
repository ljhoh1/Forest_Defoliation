dat_rbu <- data.frame(tree_data[[4]])
dat_rbu <- one_hot(as.data.table(dat_rbu), cols = c("soil_no", "slope_dir"))

mod_rbu_0 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(25,10))
                  + s(tree_age, bs="cr", k=10) ,
                  data=dat_rbu,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_rbu$n_trees,
                  method="REML")

mod_rbu_1 <- gam(nbv_ratio^0.5 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                k = c(40,25))
                 + s(tree_age, bs="cr", k=20) + ac_tot_wd + n_tot_wd
                 + te(ac_tot_wd, globrad_y_lag1, k = 10) + te(n_tot_wd, globrad_y_lag1, k = 10)
                 + H_bhd + s(globrad_y_lag1, bs="cr", k=10)
                 + slope_dir_3 + soil_no_3
                 + soil_no_5 + soil_no_6 + soil_no_12,
                 data=dat_rbu,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_rbu$n_trees,
                 method="REML")

mod_rbu_2 <- gam(nbv_ratio^0.75 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                k = c(30,25))
                 + s(tree_age, bs="cr", k=20) + ac_tot_wd + n_tot_wd
                 + te(ac_tot_wd, globrad_y_lag1, k = 10) + te(n_tot_wd, globrad_y_lag1, k = 10)
                 + H_bhd + s(globrad_y_lag1, bs="cr", k=10)
                 + slope_dir_3 + soil_no_3
                 + soil_no_5 + soil_no_6 + soil_no_12,
                 data=dat_rbu,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_rbu$n_trees,
                 method="REML")

mod_rbu_3 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(35,25))
                  + s(tree_age, bs="cr", k=20) + ac_tot_wd + n_tot_wd
                  + te(ac_tot_wd, globrad_y_lag1, k = 10) + te(n_tot_wd, globrad_y_lag1, k = 10)
                  + H_bhd + s(globrad_y_lag1, bs="cr", k=10)
                  + slope_dir_3 + soil_no_3
                  + soil_no_5 + soil_no_6 + soil_no_12,
                  data=dat_rbu,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_rbu$n_trees,
                  method="REML")


e <- residuals(mod_rbu_0); fv <- fitted(mod_rbu_0)
lm(log(e^2) ~ log(fv))
mean(dat_rbu$nbv_ratio); mean(fitted(mod_rbu_3$gam))

par(mfrow = c(2, 2))
gam.check(mod_rbu_0)

par(mfrow = c(2, 2))
gam.check(mod_rbu_1)

par(mfrow = c(2, 2))
gam.check(mod_rbu_2)

par(mfrow = c(2, 2))
gam.check(mod_rbu_3)

par(mfrow=c(1, 2))
plot(mod_rbu_2, residuals = T)
summary(mod_rbu_0)
summary(mod_rbu_1)
summary(mod_rbu_2)
summary(mod_rbu_3)
anova(mod_rbu_1)
AIC(mod_rbu_0, mod_rbu_1, mod_rbu_3,mod_rbu_2)
BIC(mod_rbu_0,mod_rbu_3, mod_rbu_1, mod_rbu_2)

train_rbu <- dat_rbu %>%
  sample_frac(0.8)

test_rbu_X <- dat_rbu %>%
  anti_join(train_rbu) %>%
  select(-c(nbv_ratio))

test_rbu_Y <- dat_rbu %>%
  anti_join(train_rbu) %>%
  select(c(nbv_ratio))

mod_rbu_fin <- gam(nbv_ratio^0.75 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                       k = c(30,25))
                   + s(tree_age, bs="cr", k=20) + ac_tot_wd + n_tot_wd
                   + te(ac_tot_wd, globrad_y_lag1, k = 10) + te(n_tot_wd, globrad_y_lag1, k = 10)
                   + H_bhd + s(globrad_y_lag1, bs="cr", k=10)
                   + slope_dir_3 + soil_no_3
                   + soil_no_5 + soil_no_6 + soil_no_12,
                   data=train_rbu,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_rbu$n_trees,
                   method="REML")

y_pred <- predict(mod_rbu_fin, newdata=test_rbu_X, type = "response")
test_rbu_Y <- test_rbu_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]^(1/0.75)

par(mfrow = c(2, 2))
gam.check(mod_rbu_fin)
summary(mod_rbu_fin)

e <- residuals(mod_rbu_fin); fv <- fitted(mod_rbu_fin)
lm(log(e^2) ~ log(fv))

par(mfrow=c(1, 2))
plot(mod_rbu_fin, residuals = T)

rmse(test_rbu_Y, y_pred)


