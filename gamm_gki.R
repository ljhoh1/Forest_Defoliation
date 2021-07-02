dat_gki <- data.frame(tree_data[[3]])
dat_gki <- one_hot(as.data.table(dat_gki), cols = c("soil_ty_no", "slope_dir"))

mod_gki_0 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(25,10))
                  + s(tree_age, bs="cr", k=10),
                  data=dat_gki,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gki$n_trees,
                  method="REML")

mod_gki_1 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(35,25))
                  + s(tree_age, bs="cr", k=40) + slope_dir_2 + slope_dir_4
                  + slope_dir_5 + slope_dir_6 + nfk + soil_ty_no_3
                  + soil_ty_no_4 + soil_ty_no_7 + soil_ty_no_8 + soil_ty_no_9
                  + soil_ty_no_11 + depth_mm,
                  data=dat_gki,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gki$n_trees,
                  method="REML")

mod_gki_2 <- gam(nbv_ratio^0.65 ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(35,25))
                  + s(tree_age, bs="cr", k=40) + s(s_vals, bs="cr", k=40)
                 + s(globrad_y_lag1, bs="cr", k420) + s(ac_tot_wd, bs="cr", k=20)
                 + slope_dir_2 + slope_dir_4 + slope_dir_5 + slope_dir_6
                 + nfk + soil_ty_no_3
                  + soil_ty_no_4 + soil_ty_no_7 + soil_ty_no_8
                  + soil_ty_no_11 + depth_mm
                  + nfk * depth_mm,
                  data=dat_gki,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gki$n_trees,
                  method="REML")

mod_gki_3 <- gam(nbv_ratio^0.8 ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                     k = c(35,25))
                 + s(tree_age, bs="cr", k=40) + s(s_vals, bs="cr", k=20)
                 + s(globrad_y_lag1, bs="cr", k=20) + s(ac_tot_wd, bs="cr", k=20)
                 + slope_dir_2 + slope_dir_4 + slope_dir_5 + slope_dir_6
                 + nfk + soil_ty_no_3
                 + soil_ty_no_4 + soil_ty_no_7 + soil_ty_no_8
                 + soil_ty_no_11 + depth_mm
                 + nfk * depth_mm,
                 data=dat_gki,
                 correlation = corARMA(form =~ year | spat, p=1, q=1),
                 family = gaussian(link="logit"),
                 weights = dat_gki$n_trees,
                 method="REML")

par(mfrow=c(1, 2))
plot(mod_gki_2, residuals = T)
anova(mod_gki_1)

e <- residuals(mod_gki_0); fv <- fitted(mod_gki_0)
lm(log(e^2) ~ log(fv))
mean(dat_gki$nbv_ratio); mean(fitted(mod_gki_0))

par(mfrow = c(2, 2))
gam.check(mod_gki_0)

par(mfrow = c(2, 2))
gam.check(mod_gki_1)

par(mfrow = c(2, 2))
gam.check(mod_gki_2)

par(mfrow = c(2, 2))
gam.check(mod_gki_3)

par(mfrow=c(1, 2))
plot(mod_gki_2$gam, residuals = T)
summary(mod_gki_0)
summary(mod_gki_1)
summary(mod_gki_2)
summary(mod_gki_3)
anova(mod_rbu_1$gam)
AIC(mod_gki_0, mod_gki_1, mod_gki_2, mod_gki_3)
BIC(mod_gki_0, mod_gki_1, mod_gki_2, mod_gki_3)

train_gki <- dat_gki %>%
  sample_frac(0.8)

test_gki_X <- dat_gki %>%
  anti_join(train_gki) %>%
  select(-c(nbv_ratio))

test_gki_Y <- dat_gki %>%
  anti_join(train_gki) %>%
  select(c(nbv_ratio))

mod_gki_fin <- gam(nbv_ratio^0.65 ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                       k = c(35,25))
                   + s(tree_age, bs="cr", k=40) + s(s_vals, bs="cr", k=40)
                   + s(globrad_y_lag1, bs="cr", k420) + s(ac_tot_wd, bs="cr", k=20)
                   + slope_dir_2 + slope_dir_4 + slope_dir_5 + slope_dir_6
                   + nfk + soil_ty_no_3
                   + soil_ty_no_4 + soil_ty_no_7 + soil_ty_no_8
                   + soil_ty_no_11 + depth_mm
                   + nfk * depth_mm,
                   data=train_gki,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_gki$n_trees,
                   method="REML")

y_pred <- predict(mod_gki_fin, newdata=test_gki_X, type = "response")
test_gki_Y <- test_gki_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]^(1/0.65)

e <- residuals(mod_gki_fin); fv <- fitted(mod_gki_fin)
lm(log(e^2) ~ log(fv))
mean(dat_gki$nbv_ratio); mean(fitted(mod_gki_0)(1/0.65))

par(mfrow = c(2, 2))
gam.check(mod_gki_fin)
summary(mod_gki_fin)

par(mfrow=c(1, 2))
plot(mod_gki_fin, residuals = T)

rmse(test_gki_Y, y_pred)
