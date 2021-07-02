# gfi
model_check <- function(model, anva = T) {
  e <- residuals(model); fv <- fitted(model)
  mod_lm <- lm(log(e^2) ~ log(fv))

  par(mfrow = c(2, 2))
  mod_plot <- gam.check(model)


  par(mfrow=c(1, 2))
  resid_plot <- plot(model, residuals = T)

  if (anva == T) {
    anov <- anova(model)
  }

  mod_eval = list(mod_lm, mod_plot, resid_plot)
  return(mod_eval)
}


dat_dgl <- data.frame(tree_data[[1]])
dat_dgl <- one_hot(as.data.table(dat_dgl), cols = c("soil_no", "slope_dir"))
mod_dgl_1 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = dof)#, gamma=1.4)
                  + s(tree_age, bs="cr", k=10) + slope_dir,
                  data=dat_dgl,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat$n_trees,
                  method="REML")

par(mfrow=c(1, 2))
plot(mod_dgl_1$gam, residuals = T)
anova(mod_dgl_1$gam)

mod_dgl_2 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(25,5))#, gamma=1.4)
                  + s(tree_age, bs="cr", k=10) + + slope_dir_6
                  + soil_no_3 + soil_no_4 + soil_no_5 + soil_no_6
                  + soil_no_11 + soil_no_12,
                  data=dat_dgl,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_dgl$n_trees,
                  method="REML")

par(mfrow=c(1, 2))
plot(mod_dgl_2$gam, residuals = T)
anova(mod_dgl_2$gam)


dat_gfi <- data.frame(tree_data[[2]])
dat_gfi <- one_hot(as.data.table(dat_gfi), cols = c("geol_no", "soil_no", "slope_dir"))
mod_gfi_1 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(25,5))#, gamma=1.4)
                  + s(tree_age, bs="cr", k=10)
                  + geol_no_20 + geol_no_50 + geol_no_60
                  + geol_no_80 + geol_no_90 + H_bhd + ac_tot_wd + spei_3_may,
                  data=dat_gfi,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat$dat_gfi,
                  method="REML")

par(mfrow=c(1, 2))
plot(mod_gfi_1$gam, residuals = T)
anova(mod_gfi_1$gam)


dat_gki <- data.frame(tree_data[[3]])
dat_gki <- one_hot(as.data.table(dat_gki), cols = c("soil_ty_no", "slope_dir"))
mod_gki_1 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(25,5))
                  + s(tree_age, bs="cr", k=10) + slope_dir_2 + slope_dir_0 + slope_dir_4
                  + slope_dir_5 + slope_dir_6 + nfk + soil_ty_no_3
                  + soil_ty_no_4 + soil_ty_no_7 + soil_ty_no_8 + soil_ty_no_9
                  + soil_ty_no_11 + depth_mm,
                  data=dat_gki,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_gki$n_trees,
                  method="REML")

par(mfrow=c(1, 2))
plot(mod_gki_1$gam, residuals = T)
anova(mod_gki_1$gam)


dat_rbu <- data.frame(tree_data[[4]])
dat_rbu <- one_hot(as.data.table(dat_rbu), cols = c("soil_no", "slope_dir"))

mod_rbu_1 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(25,5))
                  + s(tree_age, bs="cr", k=10) + ac_tot_wd + n_tot_wd
                  + ac_tot_wd * globrad_y_lag1 + n_tot_wd * globrad_y_lag1
                  + H_bhd + globrad_y_lag1
                  + slope_dir_3 + soil_no_3
                  + soil_no_5 + soil_no_6 + soil_no_12,
                  data=dat_rbu,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_rbu$n_trees,
                  method="REML")

e <- residuals(mod_rbu_1$gam); fv <- fitted(mod_rbu_1$gam)
lm(log(e^2) ~ log(fv))
mean(dat_wta$nbv_ratio); mean(fitted(mod_rbu_1$gam))

par(mfrow = c(2, 2))
gam.check(mod_rbu_1$gam)

par(mfrow=c(1, 2))
plot(mod_rbu_1$gam, residuals = T)
summary(mod_rbu_1$lme)
summary(mod_rbu_2$lme)
anova(mod_rbu_1$gam)
AIC(mod_rbu_0$lme, mod_rbu_1$lme, mod_rbu_2$lme)
BIC(mod_rbu_0$lme, mod_rbu_1$lme, mod_rbu_2$lme)


dat_tei <- data.frame(tree_data[[5]])
dat_tei <- one_hot(as.data.table(dat_tei), cols = c("soil_no", "slope_dir"))
mod_tei_1 <- gamm(nbv_ratio ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                    k = dof)#, gamma=1.4)
                    + s(tree_age, bs="cr", k=10) + ac_tot_wd
                    + slope_dir_2 + slope_dir_7 + slope_dir_10
                    + slope_dir_8 + slope_dir_6
                    + soil_no_1
                    + soil_no_2 + soil_no_3 + soil_no_10
                    + n_tot_wd + alt_m,
                     data=dat_tei,
                     correlation = corARMA(form =~ year | spat, p=1, q=1),
                     family = gaussian(link="logit"),
                     weights = dat_tei$n_trees,
                     method="REML")

par(mfrow=c(1, 2))
plot(mod_tei_1$gam, residuals = T)
anova(mod_tei_1$gam)

dof = c(25,20)
dat_wta <- data.frame(tree_data[[6]])
dat_wta <- one_hot(as.data.table(dat_wta), cols = c("soil_no", "slope_dir"))
mod_wta_1 <- gamm(nbv_ratio^0.5 ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(10,10))#, gamma=1.4)
                  + s(tree_age, bs="cr", k=10) + ac_tot_wd + n_tot_wd + n_trees
                  + spei_3_may
                  + slope_dir_0 + slope_dir_1 + slope_dir_6
                  + slope_dir_2 + slope_dir_3
                  + soil_no_6
                  + soil_no_2 + soil_no_7 + soil_no_12 + soil_no_11
                  + n_tot_wd + alt_m,
                  data=dat_wta,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_wta$n_trees,
                  method="REML")

mod_wta_2 <- gamm((nbv_ratio) ~ te(y_utm, x_utm, year, bs = bs, d = c(2,1),
                                 k = c(10,10))#, gamma=1.4)
                  + s(tree_age, bs="cr", k=7),
                  data=dat_wta,
                  correlation = corARMA(form =~ year | spat, p=1, q=1),
                  family = gaussian(link="logit"),
                  weights = dat_wta$n_trees,
                  method="REML")

par(mfrow=c(1, 2))
plot(mod_wta_1$gam, residuals = T)
anova(mod_wta_1$gam)

int1<-mod_wta_1$gam$coefficient[1]
max_age<-max(data$tree_age)
plot(mod_wta_1$gam,residuals=FALSE,shade=TRUE,shift=int1,trans=function(x)exp(x)/
       (1+exp(x)),
     xlim=c(0,max_age),ylim=c(0,1),las=1,ylab="",xlab="")

gam.check(mod_wta_1$gam)
summary(mod_wta_1$gam)

int2<-mod_wta_2$gam$coefficient[1]
plot(mod_wta_2$gam,residuals=FALSE,shade=TRUE,shift=int2,trans=function(x)exp(x)/
       (1+exp(x)),
     xlim=c(0,max_age),ylim=c(0,1),las=1,ylab="",xlab="")

gam.check(mod_wta_2$gam)
AIC(mod_wta_1$gam, mod_wta_2$gam)



mean(dat_wta$nbv_ratio); mean(fitted(mod_wta_1$gam)^2)


























