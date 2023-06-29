#multilevel only open points

#
##########################################################################################
#########################################################################################
#before use BRMS!! install STAN


##if (!requireNamespace("remotes")) {
#  install.packages("remotes")
#}
#remotes::install_github("paul-buerkner/brms")

library(brms)
library(loo)
library(sjstats)
library(sjmisc)
library(mediation)
library(see)
library(sjmisc)
library(dplyr)

CES_spatial_ind_open<-read.csv2("CES_spatial_ind_open_last.csv",
                                sep = ",", header=TRUE, stringsAsFactors = FALSE)

prior<-c(set_prior("normal(0,320)",class="b"))

Model3_open<-brm(formula=Response ~Question+(1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc)+prev_CES8_ok+Covid,data = CES_spatial_ind_open,
                 family="gaussian", iter=100000, chains=4, prior=prior,control = list(adapt_delta = 0.99))

Model3_open<-add_criterion(Model3_open,"waic")
Model3_open<-add_criterion(Model3_open,"loo")
print(Model3_open$criteria$loo)
print(Model3_open$criteria$waic)
print(Model3_open$criteria$bayes_R2)
summary(Model3_open)
#export output data in txt
Model3_open_res<-tidy_stan(Model3_open)
sink("Model3_open.txt")
print((Model3_open_res))
sink()


#Verify and Relevel the socioeconomic factors

CES_spatial_ind_open$employment<-as.factor(CES_spatial_ind_open$employment)
CES_spatial_ind_open$employment <- relevel(CES_spatial_ind_open$employment, ref = "employed")
CES_spatial_ind_open$sexe<-as.factor(CES_spatial_ind_open$sexe)
CES_spatial_ind_open$marital_status<-as.factor(CES_spatial_ind_open$marital_status)
CES_spatial_ind_open$marital_status<-relevel(CES_spatial_ind_open$marital_status, ref="divorced/widowed/unmarried")
CES_spatial_ind_open$level_of_study<-as.factor(CES_spatial_ind_open$level_of_study)
CES_spatial_ind_open$level_of_study<-relevel(CES_spatial_ind_open$level_of_study, ref="Lower_education")
CES_spatial_ind_open$age_class<-as.factor(CES_spatial_ind_open$age_class)
CES_spatial_ind_open$age_class<-relevel(CES_spatial_ind_open$age_class, ref="60-70")
CES_spatial_ind_open$income_home<-as.factor(CES_spatial_ind_open$income_home)

CES_spatial_ind_open$tpurp_mode<-as.factor(CES_spatial_ind_open$tpurp_mode)
CES_spatial_ind_open$tpurp_mode<-relevel(CES_spatial_ind_open$tpurp_mode, ref="residence")



#MODEL ADJUSTED FOR SOCIOECONOMIC VARIABLES
library(brms)

prior_socio<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio<-brm(formula=Response ~ 1 + Question + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                         prev_CES8_ok+Covid+
                         (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                       family="gaussian", iter=100000, chains=4,prior= prior_socio, control = list(adapt_delta = 0.99))

Model3_open_socio<-add_criterion(Model3_open_socio,"waic")
Model3_open_socio<-add_criterion(Model3_open_socio,"loo")
print(Model3_open_socio$criteria$loo)
print(Model3_open_socio$criteria$waic)
summary(Model3_open_socio,priors=TRUE,prob=0.95)
plot(Model3_open_socio)

prior_summary(Model3_open_socio)

Model3_open_socio_res<-equivalence_test(Model3_open_socio,p_value=TRUE)

Model3_open_socio_res

Model3_open_socio_res<-tidy_stan(Model3_open_socio)

plot(Model3_open_socio_res)
Model3_open_socio_res

sink("Model3_open_socio_res.txt")
print((Model3_open_socio_res))
sink()



##########################################
#test for time spent outside

prior_h_out<-c(set_prior("normal(0,320)",class="b"))

Model3_socio_h_out<-brm(formula=Response ~ 1 + Question + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                          prev_CES8_ok+COVID+
                          hours_out+
                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
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


#MODELS ENVIRONMENTAL HOURS

#green_scaled hours
prior_green<-c(set_prior("normal(0,320)",class="b"))
Model3_open_green_scaled<-brm(formula=Response ~ 1 + Question + green + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                prev_CES8_ok+ Covid+
                                (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                              family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))

Model3_open_green_scaled

tidy_open_green_scaled_h<-parameters::model_parameters(Model3_open_green_scaled,ci = 0.89,
                                                       ci_method = "hdi",effects="all", digits=2)
tidy_open_green_scaled_h

sink("tidy_open_green_scaled_h.txt")
print(tidy_open_green_scaled_h)
sink()

str(CES_spatial_ind_open$tpurp_mode)
posterior_green_open <- as.array(Model3_open_green_scaled)
dimnames(posterior_green_open)
color_scheme_set("red")
library(bayesplot)
mcmc_intervals(posterior_green_open, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                              "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                              "b_green_scaled", "b_sexeH", "b_marital_statusdivorcedDwidowedDunmarried",
                                              "b_employmentretired","b_employmentother","b_income_home2000M4000", "b_income_home>4000", 
                                              "b_level_of_studyLower_education","b_level_of_studysecondary_education","ar[1]","sd_no_ipc__Intercept",
                                              "sigma"))

#water
prior_water<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_water<-brm(formula=Response ~ 1 + Question + water + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                               prev_CES8_ok+Covid+
                               (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                             family="gaussian", iter=100000, chains=2, prior=prior_water,control = list(adapt_delta = 0.99))

tidy_water_open<-parameters::model_parameters(Model3_open_socio_water,ci = 0.95,
                                              ci_method = "hdi",effects="all", digits=2)
tidy_water_open

#save results
sink("tidy_water_open.txt")
print((tidy_water_open))
sink()


#monuments
prior_monuments<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_monuments<-brm(formula=Response ~ 1 + Question + monuments + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                   prev_CES8_ok+ Covid+
                                   (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                 family="gaussian", iter=100000, chains=4, prior=prior_monuments,control = list(adapt_delta = 0.99))

tidy_monuments_open<-parameters::model_parameters(Model3_open_socio_monuments,ci = 0.95,
                                                  ci_method = "hdi",effects="all", digits=2)
tidy_monuments_open
sink("Model3_open_socio_monuments_prior.txt")
print((tidy_monuments_open))
sink()

#commerce_leisure
prior_commerce_leisure<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_commerce_leisure<-brm(formula=Response ~ 1 + Question + commerce_leisure + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                          prev_CES8_ok+ Covid+
                                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                        family="gaussian", iter=10000, chains=4, prior=prior_commerce_leisure,control = list(adapt_delta = 0.99))

tidy_commerce_open<-parameters::model_parameters(Model3_open_socio_commerce_leisure,ci = 0.95,
                                                 ci_method = "hdi",effects="all", digits=2)
tidy_commerce_open 

sink("tidy_commerce_open.txt")
print(tidy_commerce_open)
sink()

#open
prior_open<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_open<-brm(formula=Response ~ 1 + Question + open + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                              prev_CES8_ok+Covid+
                              (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                            family="gaussian", iter=100000, chains=4, prior=prior_open,control = list(adapt_delta = 0.99))

tidy_open_open<-parameters::model_parameters(Model3_open_socio_open,ci = 0.95,
                                             ci_method = "hdi",effects="all", digits=2)
tidy_open_open
sink("tidy_open_open.txt")
print((tidy_open_open))
sink()

#walkable vs street

prior_walkable_str<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_walkable_str<-brm(formula=Response ~ 1 + Question + high_walkable_path + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                      prev_CES8_ok+Covid+
                                      (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                    family="gaussian", iter=10000, chains=4, prior=prior_walkable_str,control = list(adapt_delta = 0.99))

tidy_walkable_str_open<-parameters::model_parameters(Model3_open_socio_walkable_str,ci = 0.95,
                                                     ci_method = "hdi",effects="all", digits=2)
tidy_walkable_str_open
sink("tidy_walkable_str_open.txt")
print((tidy_walkable_str_open))
sink()

#noise
prior_noise<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_noise<-brm(formula=Response ~ 1 + Question + low_noise + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                               prev_CES8_ok+Covid+
                               (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                             family="gaussian", iter=100000, chains=4, prior=prior_noise,control = list(adapt_delta = 0.99))

summary(Model3_open_socio_noise)

tidy_noise_open<-parameters::model_parameters(Model3_open_socio_noise,ci = 0.95,
                                              ci_method = "hdi",effects="all", digits=2)
tidy_noise_open
sink("tidy_noise_open.txt")
print((tidy_noise_open))
sink()


#traffic
prior_traffic<-c(set_prior("normal(0,320)",class="b"))

Model3_open_socio_traffic<-brm(formula=Response ~ 1 + Question + low_speed + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                 prev_CES8_ok+Covid+
                                 (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                               family="gaussian", iter=100000, chains=4, prior=prior_traffic,control = list(adapt_delta = 0.99))

summary(Model3_open_socio_traffic)

tidy_traffic_open<-parameters::model_parameters(Model3_open_socio_traffic,ci = 0.95,
                                                ci_method = "hdi",effects="all", digits=2)
tidy_traffic_open
sink("tidy_traffic_open.txt")
print((tidy_traffic_open))
sink()


#Population density
prior_pop_dens<-c(set_prior("normal(0,320)",class="b"))
Model3_open_socio_pop_dens<-brm(formula=Response ~ 1 + Question + high_density + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                  prev_CES8_ok+Covid+
                                  (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                family="gaussian", iter=100000, chains=4, prior=prior_pop_dens,control = list(adapt_delta = 0.99))

summary(Model3_open_socio_pop_dens)

tidy_pop_dens_open<-tidy_stan(Model3_open_socio_pop_dens)
tidy_pop_dens_open
sink("tidy_pop_dens_open.txt")
print((tidy_pop_dens_open))
sink()

#Ageing Index
prior_aged<-c(set_prior("normal(0,320)",class="b"))
Model3_open_socio_aged<-brm(formula=Response ~ 1 + Question + high_aged + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                              prev_CES8_ok+Covid+
                              (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                            family="gaussian", iter=100000, chains=4, prior=prior_aged,control = list(adapt_delta = 0.99))

tidy_aged_open<-tidy_stan(Model3_open_socio_aged)
tidy_aged_open
sink("tidy_aged_open.txt")
print((tidy_aged_open))
sink()


#Income of the area
prior_income<-c(set_prior("normal(0,320)",class="b"))
Model3_open_income<-brm(formula=Response ~ 1 + Question + high_income + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                          prev_CES8_ok+Covid+
                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                        family="gaussian", iter=100000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))
summary(Model3_open_income)
tidy_income_open<-tidy_stan(Model3_open_income)
tidy_income_open
sink("tidy_income_open.txt")
print((tidy_income_open))
sink()

#Activity and mobility
str(CES_spatial_ind_open$tpurp_mode)
prior_activity_mobility<-c(set_prior("normal(0,320)",class="b"))

Model3_open_activity_mobility<-brm(formula=Response ~ 1 + Question + tpurp_mode + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                     prev_CES8_ok+Covid+
                                     (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                   family="gaussian", iter=100000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))

tidy_activity_mobility_open<-tidy_stan(Model3_open_activity_mobility)
tidy_activity_mobility_open
sink("tidy_activity_mobility_open.txt")
print((tidy_activity_mobility_open))
sink()

#inside/outside
prior_in_out<-c(set_prior("normal(0,320)",class="b"))
Model3_open_in_out<-brm(formula=Response ~ 1 + Question + nbsat + sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                          prev_CES8_ok+Covid+
                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                        family="gaussian", iter=100000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))

summary(Model3_open_in_out)
tidy_in_out_open<-tidy_stan(Model3_open_in_out)
tidy_in_out_open
sink("tidy_in_out_open.txt")
print((tidy_in_out_open))
sink()


############################################################################################
####################################test model complete hours##############################
##############################################################################################



library(brms)
prior_M4<-c(set_prior("normal(0,320)",class="b"))

Model4_socio_environ_open<-brm(formula=Response ~ 1 + Question + 
                                 water+high_walkable_path+open+commerce_leisure+ low_noise +high_aged+high_income+tpurp_mode+
                                 prev_CES8_ok+Covid+
                                 sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                 (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                               family="gaussian", iter=100000, chains=4, prior=prior_M4,control = list(adapt_delta = 0.99))

summary(Model4_socio_environ_open)

tidy_Model4_open<-parameters::model_parameters(Model4_socio_environ_open, ci = 0.89,
                                               ci_method = "hdi",effects="all", digits=2)
tidy_Model4_open
sink("tidy_Model4_socio_environ_open.txt")
print((tidy_Model4_open))
sink()
fixef(Model4_socio_environ_open)

#graphic of the CI
library(bayesplot)
mcmc_areas(as.matrix(Model4_socio_environ_open), regex_pars = 'familiar|liked', prob = .9)
posterior <- as.array(Model4_socio_environ_open)
dimnames(posterior)
color_scheme_set("red")
mcmc_intervals(posterior, pars = c("b_monuments_scaled","b_nbsatout","b_tpurp_modefoodcommerceandservices" ,"b_tpurp_modeleisureculturalandsocialactivity",
                                   "b_tpurp_modeprivatetransport","b_tpurp_modepublictransport","b_tpurp_modesoftmobility","b_tpurp_modework",
                                   "b_sexeH", "b_marital_statuscouple",
                                   "b_employmentretired","b_employmentother","b_income_home2000M4000", "b_income_home>4000", 
                                   "b_level_of_studyhiger_education","b_level_of_studysecondary_education","ar[1]","sd_no_ipc__Intercept",
                                   "sigma"))

#backward regression deleting "open"; income; high aged;low noise
library(brms)
prior_M4<-c(set_prior("normal(0,320)",class="b"))
Model4_socio_environ_open_bkw<-brm(formula=Response ~ 1 + Question + prev_CES8_ok+Covid+
                                         water_scaled+ high_walkable_path+commerce_leisure+low_noise+tpurp_mode+
                                          sexe+ marital_status+ employment + income_home + level_of_study + age_class+
                                          (1|participant_id_day)+(1|no_ipc)+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                        family="gaussian", iter=100000, chains=4, prior=prior_M4,control = list(adapt_delta = 0.99))


save(Model4_socio_environ_open_bkw,
     file = "Model4_socio_environ_open_bkw.Rdata")


tidy_Model4_socio_environ_open_bkw<-parameters::model_parameters(Model4_socio_environ_open_bkw, ci = 0.95,
                                                         ci_method = "hdi",effects="all", digits=2)
tidy_Model4_socio_environ_open_bkw
sink("tidy_Model4_socio_environ_open_bkw.txt")
print((tidy_Model4_socio_environ_open_bkw))
sink()


######################################################################################
########################PREDICTION TABLE###############################################
#######################################################################################


predictions_water<-read.csv2("predictions_all_water.csv", sep = ";", header=TRUE) #read prediction table for all participants, all items and one environmental element at time


predictions_M4_1_water<-as.data.frame(predict(Model4_socio_environ_open_no1, newdata=predictions_water))



predictions_water$prediction<-predictions_M4_1_water$Estimate

library(dplyr)

predictions_w<-select(predictions_water,Question,prediction,water)

predictions_w<-
  as.data.frame(predictions_w %>%
                  group_by(Question,water)%>%
                  summarise(mean = mean(prediction)))

predictions_w<-reshape(predictions_w, idvar = "Question", timevar = "water", direction = "wide")


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


#predictions walkable path

d_pred_walk<-read.csv2("predictions_all_walk.csv", sep = ";", header=TRUE)

d_pred_walk$tpurp_mode<-"residence"

predictions_M4_1_walk<-as.data.frame(predict(Model4_socio_environ_open_no1, newdata=d_pred_walk))

d_pred_walk$prediction<-predictions_M4_1_walk$Estimate

library(dplyr)

predictions_walk<-select(d_pred_walk,Question,prediction,high_walkable_path)

predictions_walk<-
  as.data.frame(predictions_walk %>%
                  group_by(Question,high_walkable_path)%>%
                  summarise(mean = mean(prediction)))

predictions_walk<-reshape(predictions_walk, idvar = "Question", timevar = "high_walkable_path", direction = "wide")

predictions_walk<-predictions_walk %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

#from wide to long
library(tidyr)
predictions_walk <- predictions_walk %>% gather(predictions, value, -Question)

#remove lines not useful
predictions_walk<-predictions_walk[predictions_walk$Question=="Total", ]

predictions_walk$Question<-"walkable_path"


write.csv2(predictions_walk,"pred_walk.csv")

#save in png the plot
png(file="predictions_walk.png",width=800,height=350, res=65)
#plot
library(ggplot2)
ggplot(predictions_walk, aes(x=predictions, y=value)) + 
  geom_point()+
  geom_smooth(method=lm)


ggplot(predictions_walk, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=1))+
  stat_smooth()

dev.off()

#predictions commerce_leisure

d_pred_comm<-read.csv2("predictions_all_comm.csv", sep = ";", header=TRUE)

d_pred_comm$tpurp_mode<-"residence"

predictions_M4_1_comm<-as.data.frame(predict(Model4_socio_environ_open_no1, newdata=d_pred_comm))

d_pred_comm$prediction<-predictions_M4_1_comm$Estimate

library(dplyr)

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
predictions_comm$Question<-"commerce_leisure"
write.csv2(predictions_comm,"pred_comm.csv")

library(ggplot2)
ggplot(predictions_comm, aes(x=predictions, y=value)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(predictions_comm, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=1))+
  stat_smooth()


#predictions noise
d_pred_noise<-read.csv2("predictions_all_noise.csv", sep = ";", header=TRUE)
d_pred_noise$tpurp_mode<-"soft mobility"
predictions_M4_1_noise<-as.data.frame(predict(Model4_socio_environ_open_no1, newdata=d_pred_noise))
d_pred_noise$prediction<-predictions_M4_1_noise$Estimate

library(dplyr)

predictions_noise<-select(d_pred_noise,Question,prediction,low_noise)

predictions_noise<-
  as.data.frame(predictions_noise %>%
                  group_by(Question,low_noise)%>%
                  summarise(mean = mean(prediction)))

predictions_noise<-reshape(predictions_noise, idvar = "Question", timevar = "low_noise", direction = "wide")

predictions_noise<-predictions_noise %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

#from wide to long
library(tidyr)
predictions_noise <- predictions_noise %>% gather(predictions, value, -Question)

#remove lines not useful
predictions_noise<-predictions_noise[predictions_noise$Question=="Total", ]

predictions_noise$Question<-"low_noise"


write.csv2(predictions_noise,"pred_noise.csv")


####################################################################################
######################################################################################

all_predictions<-rbind(predictions_comm,predictions_w,predictions_walk,predictions_noise)
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
png(file="predictions_multi_open.png",width=600,height=500, res=120)
library(ggplot2)

ggplot(all_predictions, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=Environmental_factor))+
  geom_line(aes(linetype = Environmental_factor),size=0.75)+
  scale_linetype_manual(values=c("twodash", "dotted","solid","dashed"))+
  ggtitle("b) outdoor location point") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("momentary mental well-being")+
  xlab("minutes")+
  xlim(0,61)+
  coord_cartesian(ylim=c(20,24))+
  theme(legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())

dev.off()

#verify correlations among posterior parameters
library(dplyr)
library(brms)
posterior_samples(Model4_socio_environ_open) %>%
  select(-lp__) %>%
  cor() %>%
  round(digits = 2)






