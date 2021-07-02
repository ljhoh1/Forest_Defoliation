dat_wta <- data.frame(tree_data[[6]])
dat_wta <- one_hot(as.data.table(dat_wta), cols = c("soil_no", "slope_dir"))
mod_wta_0 <- gam(nbv_ratio^0.75 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                       k = c(25,10))
                   + s(tree_age, bs="cr", k=10),
                   data=train_wta,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_wta$n_trees,
                   method="REML")

mod_wta_1 <- gam(nbv_ratio^0.8 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                       k = c(25,20))
                   + s(tree_age, bs="cr", k=10) + s(tpi750, bs="cr", k=20)
                   + ac_tot_wd + n_tot_wd + n_trees + alt_m + globrad_y
                   + slope_dir_0 + slope_dir_1
                   + slope_dir_3 + soil_no_6 + soil_no_2 + soil_no_7 + soil_no_12,
                   data=train_wta,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_wta$n_trees,
                   method="REML")

mod_wta_2 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                       k = c(25,20))
                   + s(tree_age, bs="cr", k=10),
                   data=train_wta,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_wta$n_trees,
                   method="REML")

par(mfrow=c(1, 2))
plot(mod_wta_1$gam, residuals = T)
anova(mod_wta_1$gam)

e <- residuals(mod_wta_0); fv <- fitted(mod_wta_0)
lm(log(e^2) ~ log(fv))

e <- residuals(mod_wta_1); fv <- fitted(mod_wta_1)
lm(log(e^2) ~ log(fv))

e <- residuals(mod_wta_2); fv <- fitted(mod_wta_2)
lm(log(e^2) ~ log(fv))

mean(dat_wta$nbv_ratio); mean(fitted(mod_wta_1)^1.25)

par(mfrow = c(2, 2))
gam.check(mod_wta_1)

par(mfrow=c(1, 2))
plot(mod_wta_1, residuals = T)
summary(mod_wta_0)
summary(mod_wta_1)
summary(mod_wta_2)

anova(mod_wta_2$gam)
AIC(mod_wta_0, mod_wta_1, mod_wta_2)
BIC(mod_wta_0, mod_wta_1, mod_wta_2)

train_wta <- dat_wta %>%
  sample_frac(0.8)

test_wta_X <- dat_wta %>%
  anti_join(train_wta) %>%
  select(-c(nbv_ratio))

test_wta_Y <- dat_wta %>%
  anti_join(train_wta) %>%
  select(c(nbv_ratio))

mod_wta_fin <- gam(nbv_ratio^0.8 ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                      k = c(25,20))
                   + s(tree_age, bs="cr", k=10) + s(tpi750, bs="cr", k=30)
                   + ac_tot_wd + n_tot_wd + n_trees + alt_m + globrad_y
                   + slope_dir_0 + slope_dir_1
                   + slope_dir_3 + soil_no_6 + soil_no_2 + soil_no_7 + soil_no_12,
                   data=train_wta,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_wta$n_trees,
                   method="REML")

e <- residuals(mod_wta_fin); fv <- fitted(mod_wta_fin)
#intervals(mod_wta_fin$lme,which="fixed")
lm(log(e^2) ~ log(fv))
mean(dat_wta$nbv_ratio); mean(fitted(mod_wta_fin))^1.33

y_pred <- predict(mod_wta_fin, newdata=test_wta_X, type = "response")
test_wta_Y <- test_wta_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]^1.33

par(mfrow = c(2, 2))
gam.check(mod_wta_fin)
summary(mod_wta_fin)

rmse(test_wta_Y, y_pred)

