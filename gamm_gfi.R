dat_gfi <- data.frame(tree_data[[2]])
dat_gfi <- one_hot(as.data.table(dat_gfi), cols = c("geol_no", "soil_no", "slope_dir"))

mod_gfi_0 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(25,10))
                  + s(tree_age, bs="cr", k=10),
                  data=dat_gfi,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gfi$n_trees,
                  method="REML")

mod_gfi_2 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                 k = c(35,25))
                  + s(tree_age, bs="cr", k=20) + slope_dir_6
                  + geol_no_20 + geol_no_50 + geol_no_60
                  + geol_no_90 + H_bhd + ac_tot_wd + spei_3_may
                  ,
                  data=dat_gfi,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gfi$n_trees,
                  method="REML")

mod_gfi_4 <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                   k = c(35,25))
                  + s(tree_age, bs="cr", k=20) + s(H_bhd, bs="cr", k=20)
                  + s(ac_tot_wd, bs="cr", k=20)
                  + slope_dir_6
                  + geol_no_20 + geol_no_50 + geol_no_60 + geol_no_90

                  ,
                  data=dat_gfi,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gfi$n_trees,
                  method="REML")

 par(mfrow=c(1, 2))
plot(mod_gfi_1, residuals = T)
anova(mod_gfi_1)

e <- residuals(mod_gfi_0); fv <- fitted(mod_gfi_0)
lm(log(e^2) ~ log(fv))
mean(dat_gfi$nbv_ratio); mean(fitted(mod_gfi_4)^2)

par(mfrow = c(2, 2))
gam.check(mod_gfi_0)

par(mfrow = c(2, 2))
gam.check(mod_gfi_2)

par(mfrow = c(2, 2))
gam.check(mod_gfi_4)

par(mfrow=c(1, 2))
plot(mod_gfi_4, residuals = T)
summary(mod_gfi_0)
summary(mod_gfi_4)
summary(mod_gfi_2)
anova(mod_gfi_4)
AIC(mod_gfi_4, mod_gfi_0, mod_gfi_2)
BIC(mod_gfi_4, mod_gfi_0, mod_gfi_2)

train_gfi <- dat_gfi %>%
  sample_frac(0.8)

test_gfi_X <- dat_gfi %>%
  anti_join(train_gfi) %>%
  select(-c(nbv_ratio))

test_gfi_Y <- dat_gfi %>%
  anti_join(train_gfi) %>%
  select(c(nbv_ratio))

mod_gfi_fin <- gam(nbv_ratio ~ te(y_utm, x_utm, year, bs = c("tp","tp"), d = c(2,1),
                                  k = c(45,25))
                   + s(tree_age, bs="cr", k=20) + s(H_bhd, bs="cr", k=20)
                   + s(ac_tot_wd, bs="cr", k=20)
                   + slope_dir_6
                   + geol_no_20 + geol_no_50 + geol_no_60 + geol_no_90

                   ,
                   data=train_gfi,
                   correlation = corARMA(form =~ year | spat, p=1, q=1),
                   family = gaussian(link="logit"),
                   weights = train_gfi$n_trees,
                   method="REML")

y_pred <- predict(mod_gfi_fin$gam, newdata=test_gfi_X, type = "response")
test_gfi_Y <- test_gfi_Y$nbv_ratio[!is.na(y_pred)]; y_pred <- y_pred[!is.na(y_pred)]

e <- residuals(mod_gfi_fin); fv <- fitted(mod_gfi_fin)
lm(log(e^2) ~ log(fv))

par(mfrow = c(2, 2))
gam.check(mod_gfi_fin$gam)
summary(mod_gfi_fin$gam)

rmse(test_gfi_Y, y_pred)
