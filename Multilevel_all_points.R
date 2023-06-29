#########################################################################################
#################### multilevel model####################################################
#########################################################################################


##########################################################################################
#########################################################################################
#before use BRMS!! install STAN
#before use BRMS!! 
#Sys.getenv("BINPREF")
#Sys.which("make")
#remove.packages("rstan")
#if (file.exists(".RData")) file.remove(".RData")
#restart
#install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

#install.packages("jsonlite", type = "source")


library(rstan)
library(brms)
library(loo)
library(sjstats)
library(sjmisc)
library(mediation)
library(see)
library(dplyr)

CES_spatial_ind<-read.csv2("CES_spatial_ind.csv",
                           sep = ",", header=TRUE, stringsAsFactors = FALSE)

levels(as.factor(CES_spatial_ind$no_ipc))
prior<-c(set_prior("normal(0,320)",class="b"))
Model3<-brm(Response ~Question+(1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
            family="gaussian", iter=100000, chains=4, prior=prior,control = list(adapt_delta = 0.99))

Model3<-add_criterion(Model3,"waic")
Model3<-add_criterion(Model3,"loo")
print(Model3$criteria$loo)
print(Model3$criteria$waic)
print(Model3$criteria$bayes_R2)
summary(Model3)
#export output data in txt
sink("Model3.txt")
print(summary(Model3))
sink()


#for WAIC model comparison
#loo(Model1,Model2, Model3)

Model3_res<-equivalence_test(Model3,p_value=TRUE)
Model3_res
equi_test(Model3, out = "plot")

tidy_stan(Model3)

plot(Model3_res)


#Model3_environmental elements


#Verify and Relevel the socioeconomic factors

CES_spatial_ind$employment<-as.factor(CES_spatial_ind$employment)
summary(CES_spatial_ind$employment)
CES_spatial_ind$employment <- relevel(CES_spatial_ind$employment, ref = "employed")
str(CES_spatial_ind$employment)

CES_spatial_ind$sexe<-as.factor(CES_spatial_ind$sexe)
str(CES_spatial_ind$sexe)

CES_spatial_ind$marital_status<-as.factor(CES_spatial_ind$marital_status)
CES_spatial_ind$marital_status<-relevel(CES_spatial_ind$marital_status, ref="divorced/widowed/unmarried")
str(CES_spatial_ind$marital_status)

CES_spatial_ind$level_of_study<-as.factor(CES_spatial_ind$level_of_study)
CES_spatial_ind$level_of_study<-relevel(CES_spatial_ind$level_of_study, ref="Lower_education")
str(CES_spatial_ind$level_of_study)

CES_spatial_ind$age_class<-as.factor(CES_spatial_ind$age_class)
CES_spatial_ind$age_class<-relevel(CES_spatial_ind$age_class, ref="60-70")
str(CES_spatial_ind$age_class)


CES_spatial_ind$income_home<-as.factor(CES_spatial_ind$income_home)
str(CES_spatial_ind$income_home)

#relevel TPURP_mode
CES_spatial_ind$tpurp_mode<-ifelse(CES_spatial_ind$tpurp_mode=="","soft mobility",CES_spatial_ind$tpurp_mode)

CES_spatial_ind$tpurp_mode<-as.factor(CES_spatial_ind$tpurp_mode)
CES_spatial_ind$tpurp_mode<-relevel(CES_spatial_ind$tpurp_mode, ref="residence")
levels(CES_spatial_ind$tpurp_mode)


#adjust the model for sex, marital status and sociodemo
#boxplot for group level to see if there are differences
#library("ggpubr")
#boxplot(Response ~ sexe, data = CES_spatial_ind,
#      xlab = "sex", ylab = "Response",
#      frame = FALSE, col = c("#00AFBB", "#E7B800", "red","green", "black", "#FC4E07", "orange"))



prior_socio<-c(set_prior("normal(0,320)",class="b"))

Model3_socio<-brm(formula=Response ~ 1 + Question + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                    prev_CES8_ok+COVID+
                    (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                  family="gaussian", iter=100000, chains=4,prior= prior_socio, control = list(adapt_delta = 0.99))

Model3_socio<-add_criterion(Model3_socio,"waic")
Model3_socio<-add_criterion(Model3_socio,"loo")
print(Model3_socio$criteria$loo)
print(Model3_socio$criteria$waic)
#print(Model3_socio$criteria$bayes_R2)
summary(Model3_socio,priors=TRUE,prob=0.95)
plot(Model3_socio)

prior_summary(Model3_socio)

Model3_socio_res<-equivalence_test(Model3_socio,p_value=TRUE)
Model3_socio_res

Model3_socio<-tidy_stan(Model3_socio)

plot(Model3_socio_res)


sink("Model3_socio.txt")
print(summary(Model3_socio))
sink()

#model comparison
#for WAIC model comparison
loo(Model3,Model3_socio)



##########################################
#test for time spent outside

prior_h_out<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_h_out<-brm(formula=Response ~ 1 + Question + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                   prev_CES8_ok+COVID+
                                   hours_out+
                                   (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                                 family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))

tidy_h_out<-parameters::model_parameters(Model3_socio_h_out,ci = 0.95,
                                                  ci_method = "hdi",effects="all", digits=2)
tidy_h_out

sink("tidy_h_out.txt")
print(tidy_h_out)
sink()

posterior_h_out <- as.array(Model3_socio_h_out)
dimnames(Model3_socio_h_out)
library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_out, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                         "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                         "b_hours_out", "ar[1]","sd_no_ipc__Intercept",
                                         "sigma"))




##########################################
#add environmental variables
#green_scaled hours
prior_green<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_green_scaled_h<-brm(formula=Response ~ 1 + Question + green_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                   prev_CES8_ok+COVID+
                                   (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                                 family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))

tidy_green_scaled_h<-parameters::model_parameters(Model3_socio_green_scaled_h,ci = 0.95,
                                                  ci_method = "hdi",effects="all", digits=2)
tidy_green_scaled_h

sink("tidy_green_scaled_h.txt")
print(tidy_green_scaled_h)
sink()
save(Model3_socio_green_scaled_h,
     file = "Model3_socio_green_scaled_h.Rdata")

posterior_green <- as.array(Model3_socio_green_scaled_h)
dimnames(posterior_green)
library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_green, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                         "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                         "b_green_scaled", "ar[1]","sd_no_ipc__Intercept",
                                         "sigma"))



#water
prior_water<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_water<-brm(formula=Response ~ 1 + Question + water_scaled + sexe+ marital_status+ 
                          employment + income_home + level_of_study + age_class+
                          prev_CES8_ok+Covid+
                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),
                        data = CES_spatial_ind,
                        family="gaussian", iter=100000, chains=4, prior=prior_water,control = list(adapt_delta = 0.99))

summary(Model3_socio_water)

tidy_water<-parameters::model_parameters(Model3_socio_water,ci = 0.95,
                                         ci_method = "hdi",effects="all", digits=2)
tidy_water

sink("tidy_water.txt")
print(summary(tidy_water))
sink()

#monuments
prior_monuments<-c(set_prior("normal(0,320)",class="b"))


Model3_socio_monuments<-brm(formula=Response ~ 1 + Question + monuments_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                              prev_CES8_ok+Covid+
                              (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                            family="gaussian", iter=100000, chains=4, prior=prior_monuments,control = list(adapt_delta = 0.99))


summary(Model3_socio_monuments)

tidy_monuments<-parameters::model_parameters(Model3_socio_monuments,ci = 0.95,
                                            ci_method = "hdi",effects="all", digits=2)
tidy_monuments

sink("tidy_monuments.txt")
print(summary(tidy_monuments))
sink()

#commerce_leisure
prior_commerce_leisure<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_commerce_leisure<-brm(formula=Response ~ 1 + Question + commerce_leisure_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                     prev_CES8_ok+Covid+
                                     (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                                   family="gaussian", iter=100000, chains=4, prior=prior_commerce_leisure,control = list(adapt_delta = 0.99))

summary(Model3_socio_commerce_leisure)

tidy_commerce<-parameters::model_parameters(Model3_socio_commerce_leisure,ci = 0.95,
                                            ci_method = "hdi",effects="all", digits=2)
tidy_commerce 

sink("tidy_commerce.txt")
print(tidy_commerce)
sink()

#open
prior_open<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_open<-brm(formula=Response ~ 1 + Question + open_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                         prev_CES8_ok+Covid+
                         (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                       family="gaussian", iter=100000, chains=4, prior=prior_open,control = list(adapt_delta = 0.99))

summary(Model3_socio_open)

tidy_open<-parameters::model_parameters(Model3_socio_open,ci = 0.95,centrality = "mean",
                                        ci_method = "hdi",effects="all", digits=2)
tidy_open

sink("tidy_open.txt")
print(tidy_open)
sink()


#walkable vs street

prior_walkable_str<-c(set_prior("normal(0,320)",class="b"))


Model3_socio_walkable_str<-brm(formula=Response ~ 1 + Question + high_walkable_path_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                 prev_CES8_ok+Covid+
                                 (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                               family="gaussian", iter=100000, chains=4, prior=prior_walkable_str,control = list(adapt_delta = 0.99))

summary(Model3_socio_walkable_str)
library(parameters)

tidy_walkable_str<-parameters::model_parameters(Model3_socio_walkable_str,ci = 0.95,centrality = "mean",
                                                ci_method = "hdi",effects="all", digits=2)
tidy_walkable_str
sink("tidy_walkable_str.txt")
print((tidy_walkable_str))
sink()

library(bayestestR)
library(see)
p_direction(Model3_socio_walkable_str)


#noise
library(brms)
prior_noise<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_noise<-brm(formula=Response ~ 1 + Question + low_noise + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                          prev_CES8_ok+Covid+
                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                        family="gaussian", iter=100000, chains=4, prior=prior_noise,control = list(adapt_delta = 0.99))

summary(Model3_socio_noise)

tidy_noise<-parameters::model_parameters(Model3_socio_noise,ci = 0.95,centrality = "mean",
                                         ci_method = "hdi",effects="all", digits=2)
tidy_noise
sink("tidy_noise.txt")
print((tidy_noise))
sink()


#traffic
prior_traffic<-c(set_prior("normal(0,320)",class="b"))


Model3_socio_traffic<-brm(formula=Response ~ 1 + Question + low_speed_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                            prev_CES8_ok+Covid+
                            (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                          family="gaussian", iter=100000, chains=4, prior=prior_traffic,control = list(adapt_delta = 0.99))


summary(Model3_socio_traffic)

tidy_traffic<-parameters::model_parameters(Model3_socio_traffic,ci = 0.95,centrality = "mean",
                                           ci_method = "hdi",effects="all", digits=2)

tidy_traffic
sink("tidy_traffic.txt")
print((tidy_traffic))
sink()


#Population density
prior_pop_dens<-c(set_prior("normal(0,320)",class="b"))


Model3_socio_pop_dens<-brm(formula=Response ~ 1 + Question + high_density_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                             prev_CES8_ok+Covid+
                             (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                           family="gaussian", iter=100000, chains=4, prior=prior_pop_dens,control = list(adapt_delta = 0.99))


summary(Model3_socio_pop_dens)

tidy_pop_dens<-parameters::model_parameters(Model3_socio_pop_dens,ci = 0.95,centrality = "mean",
                                            ci_method = "hdi",effects="all", digits=2)

tidy_pop_dens
sink("tidy_pop_dens.txt")
print((tidy_pop_dens))
sink()



#Ageing Index

prior_aged<-c(set_prior("normal(0,320)",class="b"))


Model3_socio_aged<-brm(formula=Response ~ 1 + Question + high_aged_scaled + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                         prev_CES8_ok+ Covid+
                         (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                       family="gaussian", iter=100000, chains=4, prior=prior_aged,control = list(adapt_delta = 0.99))


summary(Model3_socio_aged)

tidy_aged<-parameters::model_parameters(Model3_socio_aged,ci = 0.95,centrality = "mean",
                                        ci_method = "hdi",effects="all", digits=2)

tidy_aged
sink("tidy_aged.txt")
print((tidy_aged))
sink()


#Income of the area
prior_income<-c(set_prior("normal(0,320)",class="b"))


Model3_income<-brm(formula=Response ~ 1 + Question + high_income + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                     prev_CES8_ok+Covid+
                     (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                   family="gaussian", iter=100000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))


summary(Model3_income)

tidy_income<-parameters::model_parameters(Model3_income,ci = 0.95,centrality = "mean",
                                          ci_method = "hdi",effects="all", digits=2)

tidy_income
sink("tidy_income.txt")
print((tidy_income))
sink()



#Activity and mobility

str(CES_spatial_ind$tpurp_mode)

prior_activity_mobility<-c(set_prior("normal(0,320)",class="b"))



Model3_activity_mobility<-brm(formula=Response ~ 1 + Question + tpurp_mode + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                prev_CES8_ok+ Covid+
                                (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                              family="gaussian", iter=100000, chains=4, prior=prior_activity_mobility,control = list(adapt_delta = 0.99))


Model3_activity_mobility_res<-summary(Model3_activity_mobility)
Model3_activity_mobility_res
tidy_activity_mobility<-parameters::model_parameters(Model3_activity_mobility, ci = 0.95,
                                                     ci_method = "hdi",effects="all", digits=2)
tidy_activity_mobility
sink("tidy_Model3_activity_mobility.txt")
print((tidy_activity_mobility))
sink()

summary(CES_spatial_ind$tpurp_mode)


#inside/outside
prior_in_out<-c(set_prior("normal(0,320)",class="b"))

Model3_in_out<-brm(formula=Response ~ 1 + Question + nbsat + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                     prev_CES8_ok+Covid+
                     (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                   family="gaussian", iter=10000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))

summary(Model3_in_out)

tidy_in_out<-parameters::model_parameters(Model3_in_out, ci = 0.95,
                                          ci_method = "hdi",effects="all", digits=2)
tidy_in_out
sink("tidy_in_out.txt")
print((tidy_in_out))
sink()


############################################################################################
#################################### model complete ##############################
##############################################################################################



prior_M4<-c(set_prior("normal(0,320)",class="b"))

#model with commerce,water, tpurp,nbsat
Model4_socio_environ<-brm(formula=Response ~ 1 + Question + 
                            commerce_leisure_scaled + water_scaled+ tpurp_mode+ nbsat +
                            prev_CES8_ok+Covid+
                            sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                            (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                          family="gaussian", iter=10000, chains=4, prior=prior_M4,control = list(adapt_delta = 0.99))

save(Model4_socio_environ,
     file = "Model4_socio_environ.Rdata")


summary(Model4_socio_environ)
Model4<-summary(Model4_socio_environ)
Model4

tidy_Model4<-parameters::model_parameters(Model4_socio_environ, ci = 0.95,
                                          ci_method = "hdi",effects="all", digits=2)
tidy_Model4
sink("tidy_Model4.txt")
print((tidy_Model4))
sink()

fixef(Model4_socio_environ)


#model backwards with commerce,water, tpurp
library(brms)
Model4_socio_environ_bkw<-brm(formula=Response ~ 1 + Question + 
                                commerce_leisure_scaled + water_scaled+ tpurp_mode+ 
                                prev_CES8_ok+Covid+
                                sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                              family="gaussian", iter=10000, chains=4, prior=prior_M4,control = list(adapt_delta = 0.99))

save(Model4_socio_environ_bkw,
     file = "Model4_socio_environ_bkw.Rdata")

tidy_socio_environ_bkw<-parameters::model_parameters(Model4_socio_environ_bkw, ci = 0.95,
                                                     ci_method = "hdi",effects="all", digits=2)
tidy_socio_environ_bkw
sink("tidy_socio_environ_bkw.txt")
print((tidy_socio_environ_bkw))
sink()

fixef(Model4_socio_environ_bkw)
summary(Model4_socio_environ_bkw)
library(brms)
WAIC(Model4_socio_environ_bkw)
loo(Model4_socio_environ_bkw)


#add covid variable as confounder

COVID<-read.csv2("data_all_2023_covid.csv",
                 sep = ",", header=TRUE, stringsAsFactors = FALSE)
rm(covid)
COVID<-select(COVID,participant_id,Covid)
COVID$Covid<-as.factor(COVID$Covid)

CES_spatial_ind<-merge(CES_spatial_ind, COVID,by="participant_id", all.x = TRUE)



###############################################################"
library(dplyr)
library(ggplot2)

CES_spatial_ind %>%
  ggplot(aes(x = Response, y = water_scaled)) +
  geom_abline(intercept = fixef(Model4_socio_environ_bkw)[1], 
              slope     = fixef(Model4_socio_environ_bkw)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  theme_bw() +
  theme(panel.grid = element_blank())


#verify correlation among posterior samples
test<-posterior_samples(Model4_socio_environ_bkw) %>%
  select(-lp__) %>%
  cor() %>%
  round(digits = 2)

#graphic of the areas
library(bayesplot)
mcmc_areas(as.matrix(Model4_socio_environ_bkw), regex_pars , prob = .9)

posterior <- as.array(Model4_socio_environ_bkw)
dimnames(posterior)
color_scheme_set("red")
mcmc_intervals(posterior, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                   "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                   "b_water_scaled", "b_commerce_leisure_scaled",
                                   "b_prev_CES8_ok",
                                   "b_tpurp_modefoodcommerceandservices" ,"b_tpurp_modeleisureculturalandsocialactivity",
                                   "b_tpurp_modeprivatetransport","b_tpurp_modepublictransport","b_tpurp_modesoftmobility","b_tpurp_modework",
                                   "b_sexeH", "b_marital_statuscouple","b_employmentunemployed",
                                   "b_employmentretired","b_employmentother","b_income_home2000M4000", "b_income_home>4000", 
                                   "b_level_of_studyhiger_education","b_level_of_studysecondary_education","ar[1]","sd_no_ipc__Intercept",
                                   "sigma"))




#############################################################################
###############################################################################
###########################PREDICTIONS###########################################

######################################################################################
########################PREDICTION TABLE###############################################
#######################################################################################


predictions_water<-read.csv2("predictions_all_water.csv", sep = ";", header=TRUE)

predictions_M4_water<-as.data.frame(predict(Model4_socio_environ_bkw, newdata=predictions_water))

predictions_water$prediction<-predictions_M4_water$Estimate

predictions_w<-select(predictions_water,Question,prediction,water_scaled)

predictions_w<-
  as.data.frame(predictions_w %>%
                  group_by(Question,water_scaled)%>%
                  summarise(mean = mean(prediction)))

predictions_w<-reshape(predictions_w, idvar = "Question", timevar = "water_scaled", direction = "wide")

predictions_w<-predictions_w %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

#from wide to long
library(tidyr)
predictions_w <- predictions_w %>% gather(predictions, value, -Question)

#remove lines not useful
predictions_w<-predictions_w[predictions_w$Question=="Total", ]

predictions_w$Question<-"water"

write.csv2(predictions_w,"pred_water.csv")


png(file="mygraphic.png",width=400,height=350, res=65)

library(ggplot2)
ggplot(predictions_w, aes(x=predictions, y=value)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(predictions_w, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=1))+
  stat_smooth()
dev.off()

#predictions commerce_leisure

d_pred_comm<-read.csv2("predictions_all_comm.csv", sep = ";", header=TRUE)

predictions_M4_1_comm<-as.data.frame(predict(Model4_socio_environ_bkw, newdata=d_pred_comm))

d_pred_comm$prediction<-predictions_M4_1_comm$Estimate

predictions_comm<-select(d_pred_comm,Question,prediction,commerce_leisure)

predictions_comm<-
  as.data.frame(predictions_comm %>%
                  group_by(Question,commerce_leisure)%>%
                  summarise(mean = mean(prediction)))

predictions_comm<-reshape(predictions_comm, idvar = "Question", timevar = "commerce_leisure", direction = "wide")

predictions_comm<-predictions_comm %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

#from wide to long
library(tidyr)
predictions_comm <- predictions_comm %>% gather(predictions, value, -Question)

#remove lines not useful
predictions_comm<-predictions_comm[predictions_comm$Question=="Total", ]

write.csv2(predictions_comm,"pred_comm.csv")

library(ggplot2)
ggplot(predictions_comm, aes(x=predictions, y=value)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(predictions_comm, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=1))+
  stat_smooth()

#predictions all

all_predictions<-rbind(predictions_comm,predictions_w)
all_predictions$predictions<-gsub("prediction.","", all_predictions$predictions)

all_predictions$Environmental_factor<-all_predictions$Question

all_predictions$Question<-NULL

all_predictions$predictions<-ifelse(all_predictions$predictions=="mean.0",0,
                                    ifelse(all_predictions$predictions=="mean.0.25",15,
                                           ifelse(all_predictions$predictions=="mean.0.5",30,
                                                  ifelse(all_predictions$predictions=="mean.0.75",45,
                                                         ifelse(all_predictions$predictions=="mean.1",60,
                                                                ifelse(all_predictions$predictions=="mean.1.25",75,
                                                                       ifelse(all_predictions$predictions=="mean.1.5",90,
                                                                              ifelse(all_predictions$predictions=="mean.1.75",105,
                                                                                     ifelse(all_predictions$predictions=="mean.2",120,all_predictions$predictions)))))))))
all_predictions$predictions<-as.numeric(all_predictions$predictions)
png(file="predictions_multi_all.png",width=600,height=500, res=120)
library(ggplot2)

ggplot(all_predictions, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=Environmental_factor))+
  geom_line(aes(linetype = Environmental_factor),size=0.75)+
  scale_linetype_manual(values=c("twodash","dashed"))+
  ggtitle("a) all location points") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  ylab("momentary mental well-being")+
  xlab("minutes")+
  xlim(0,61)+
  coord_cartesian(ylim=c(20,24))+
  theme(legend.position="bottom",legend.title = element_blank())

dev.off()




