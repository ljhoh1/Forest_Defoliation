dat_tei <- data.frame(tree_data[[5]])
dat_tei <- one_hot(as.data.table(dat_tei), cols = c("soil_no", "slope_dir"))
mod_tei_0 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(25,10))
                  + s(tree_age, bs="cr", k=10),
                  data=dat_tei,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_tei$n_trees,
                  method="REML")

mod_tei_1 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(35,25))
                  + s(tree_age, bs="cr", k=50) + s(ac_tot_wd, bs="cr", k=10) + s(n_tot_wd, bs="cr", k=10)
                  + s(alt_m, bs="cr", k=30) + s(depth_mm, bs="cr", k=10) + s(s_vals, bs="cr", k=10)
                  + slope_dir_2 + slope_dir_7
                  + slope_dir_8 + slope_dir_6
                  + soil_no_1
                  + soil_no_2 + soil_no_3 + soil_no_10,
                  data=dat_tei,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_tei$n_trees,
                  method="REML")

mod_tei_2 <- gam(nbv_ratio^0.5 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(30,25))
                  + s(tree_age, bs="cr", k=100) + s(alt_m, bs="cr", k=50) + s(s_vals, bs="cr", k=50)
                  + slope_dir_2 + slope_dir_7
                  + slope_dir_8 + slope_dir_6
                  + soil_no_1
                  + soil_no_2 + soil_no_3
                  + n_tot_wd + alt_m,
                  data=dat_tei,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_tei$n_trees,
                  method="REML")

mod_tei_3 <- gam(nbv_ratio^0.9 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                    k = c(25,25))
                 + s(tree_age, bs="cr", k=30) + ac_tot_wd
                 + slope_dir_2 + slope_dir_7
                 + slope_dir_8 + slope_dir_6
                 + soil_no_1
                 + soil_no_2 + soil_no_3
                 + n_tot_wd + alt_m,
                 data=dat_tei,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_tei$n_trees,
                 method="REML")

par(mfrow=c(1, 2))
plot(mod_tei_1, residuals = T)
anova(mod_tei_1)

e <- residuals(mod_tei_2); fv <- fitted(mod_tei_2)
lm(log(e^2) ~ log(fv))
mean(dat_tei$nbv_ratio); mean(fitted(mod_tei_2$gam)^2)

par(mfrow = c(2, 2))
gam.check(mod_tei_0)

par(mfrow = c(2, 2))
gam.check(mod_tei_1)

par(mfrow = c(2, 2))
gam.check(mod_tei_2)

par(mfrow=c(1, 2))
plot(mod_tei_2$gam, residuals = T)
summary(mod_tei_0)
summary(mod_tei_1)
summary(mod_tei_2)

anova(mod_rbu_1)
AIC(mod_tei_0, mod_tei_2, mod_tei_1)
BIC(mod_tei_0, mod_tei_1, mod_tei_2)#, mod_tei_3$lme)

train_tei <- dat_tei %>%
  sample_frac(0.8)

test_tei_X <- dat_tei %>%
  anti_join(train_tei) %>%
  select(-c(nbv_ratio))

test_tei_Y <- dat_tei %>%
  anti_join(train_tei) %>%
  select(c(nbv_ratio))

mod_tei_fin <- gam(nbv_ratio^0.5 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                      k = c(30,25))
                   + s(tree_age, bs="cr", k=100) + s(alt_m, bs="cr", k=50) + s(s_vals, bs="cr", k=50)
                   + slope_dir_2 + slope_dir_7
                   + slope_dir_8 + slope_dir_6
                   + soil_no_1
                   + soil_no_2 + soil_no_3
                   + n_tot_wd + alt_m,
                   data=dat_tei,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = dat_tei$n_trees,
                   method="REML")

y_pred <- predict(mod_tei_fin, newdata=test_tei_X, type = "response")
test_tei_Y <- test_tei_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]^2

par(mfrow=c(1, 2))
plot(mod_tei_2, residuals = T)

e <- residuals(mod_tei_fin); fv <- fitted(mod_tei_fin)
lm(log(e^2) ~ log(fv))
mean(dat_tei$nbv_ratio); mean(fitted(mod_tei_fin)^2)

par(mfrow = c(2, 2))
gam.check(mod_tei_fin)
summary(mod_tei_fin)

rmse(test_tei_Y, y_pred)




