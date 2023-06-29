#fixed effect model

library(tidyr)
library(brms)
library(loo)
library(sjstats)
library(sjmisc)
library(mediation)
library(see)
library(dplyr)

CES_spatial_ind<-read.csv2("CES_spatial_ind.csv",
                           sep = ",", header=TRUE, stringsAsFactors = FALSE)

str(CES_spatial_ind$no_ipc)
CES_spatial_ind$no_ipc<-as.factor(CES_spatial_ind$no_ipc)
CES_spatial_ind$participant_id_day<-as.factor(CES_spatial_ind$participant_id_day)

CES_spatial_ind$Question<-as.factor(CES_spatial_ind$Question)
CES_spatial_ind$Question <- relevel(CES_spatial_ind$Question, ref = "CES_Q4")

#Verify and Relevel the socioeconomic factors

CES_spatial_ind$employment<-as.factor(CES_spatial_ind$employment)
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

CES_spatial_ind$tpurp_mode<-as.factor(CES_spatial_ind$tpurp_mode)
CES_spatial_ind$tpurp_mode<-relevel(CES_spatial_ind$tpurp_mode, ref="residence")
str(CES_spatial_ind$tpurp_mode)


#add n minutes outside
#add minutes outside
min_out<-read.csv2("seconds_out_questionnaire.csv", sep = ",", header=TRUE, stringsAsFactors = FALSE)
min_out$X<-NULL
library(dplyr)
min_out<-rename(min_out, no_ipc=sampno)
min_out<-rename(min_out, Formulaire=Nom_Formulaire)

min_out$seconds_out<-NULL


CES_spatial_ind<-merge(CES_spatial_ind,min_out,by=c("no_ipc","Formulaire"),all.x = TRUE)
CES_spatial_ind[is.na(CES_spatial_ind)] = 0


CES_spatial_ind$minutes_out<-as.numeric(CES_spatial_ind$minutes_out)
CES_spatial_ind$hours_out<-CES_spatial_ind$minutes_out/60
str(CES_spatial_ind)

#test whether hours outside are associated with the outcome
prior_out<-c(set_prior("normal(0,320)",class="b"))

Fix3_out_hours<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                      hours_out + 
                      participant_id_day+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                    family="gaussian", iter=100000, chains=4, prior=prior_out,control = list(adapt_delta = 0.99))

tidy_Fix3_out_hours<-parameters::model_parameters(Fix3_out_hours,ci = 0.95,
                                                  ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_out_hours

sink("tidy_Fix3_out_hours.txt")
print(tidy_Fix3_out_hours)
sink()


posterior_h_out <- as.array(Fix3_out_hours)
dimnames(posterior_h_out)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_out, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                         "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                         "b_hours_out", "ar[1]","sd_no_ipc__Intercept",
                                         "sigma"))



#Between individual models for environmental exposures defined based on all location points 
#separate models

#green hours
library(brms)
prior_green<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_green<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                        green_scaled +
                        ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                      family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_green<-parameters::model_parameters(Fix3_socio_green,ci = 0.95,
                                                    ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_green

sink("tidy_Fix3_socio_green.txt")
print(tidy_Fix3_socio_green)
sink()

posterior_h_green <- as.array(Fix3_socio_green)
dimnames(posterior_h_green)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_green, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                         "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                         "b_green_scaled", "ar[1]","sd_no_ipc__Intercept",
                                         "sigma"))



#water hours
prior_water<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_water<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                        water_scaled + 
                        ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                      family="gaussian", iter=100000, chains=4, prior=prior_water,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_water<-parameters::model_parameters(Fix3_socio_water,ci = 0.95,
                                                    ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_water

sink("tidy_Fix3_socio_water.txt")
print(tidy_Fix3_socio_water)
sink()

posterior_h_water <- as.array(Fix3_socio_water)
dimnames(posterior_h_water)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_water, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                         "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                         "b_water_scaled", "ar[1]","sd_no_ipc__Intercept",
                                         "sigma"))


#landmarks hours
str(CES_spatial_ind$monuments_scaled)
CES_spatial_ind$monuments_scaled<-as.numeric(CES_spatial_ind$monuments_scaled)

prior_monuments<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_monuments<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                            monuments_scaled + 
                            ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                          family="gaussian", iter=100000, chains=4, prior=prior_monuments,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_monuments<-parameters::model_parameters(Fix3_socio_monuments,ci = 0.95,
                                                        ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_monuments

sink("tidy_Fix3_socio_monuments.txt")
print(tidy_Fix3_socio_monuments)
sink()


posterior_h_monuments <- as.array(Fix3_socio_monuments)
dimnames(Fix3_socio_monuments)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_monuments, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                           "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                           "b_monuments_scaled", "ar[1]","sd_no_ipc__Intercept",
                                           "sigma"))



#commerce hours
prior_commerce<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_commerce<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                           commerce_leisure_scaled +
                           ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                         family="gaussian", iter=100000, chains=4, prior=prior_commerce,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_commerce<-parameters::model_parameters(Fix3_socio_commerce,ci = 0.95,
                                                       ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_commerce

sink("tidy_Fix3_socio_commerce.txt")
print(tidy_Fix3_socio_commerce)
sink()

posterior_h_commerce <- as.array(Fix3_socio_commerce)
dimnames(Fix3_socio_commerce)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_commerce, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                               "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                               "b_commerce_leisure_scaled", "ar[1]","sd_no_ipc__Intercept",
                                               "sigma"))



#opennes hours
prior_open<-c(set_prior("normal(0,320)",class="b"))

CES_spatial_ind$open_scaled<-as.numeric(CES_spatial_ind$open_scaled)

Fix3_socio_open<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                       open_scaled +
                       ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                     family="gaussian", iter=100000, chains=4, prior=prior_open,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_open<-parameters::model_parameters(Fix3_socio_open,ci = 0.95,
                                                   ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_open

sink("tidy_Fix3_socio_open.txt")
print(tidy_Fix3_socio_open)
sink()

posterior_h_open <- as.array(Fix3_socio_open)
dimnames(Fix3_socio_open)

library(bayesplot)

color_scheme_set("red")
mcmc_intervals(posterior_h_open, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                              "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                              "b_open_scaled", "ar[1]","sd_no_ipc__Intercept",
                                              "sigma"))


#pedestrian/street hours
prior_pedestrian<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_pedestrian<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                             high_walkable_path_scaled +
                             ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                           family="gaussian", iter=100000, chains=4, prior=prior_pedestrian,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_pedestrian<-parameters::model_parameters(Fix3_socio_pedestrian,ci = 0.95,
                                                         ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_pedestrian

sink("tidy_Fix3_socio_pedestrian.txt")
print(tidy_Fix3_socio_pedestrian)
sink()

posterior_h_pedestrian <- as.array(Fix3_socio_pedestrian)
dimnames(Fix3_socio_pedestrian)

color_scheme_set("red")
mcmc_intervals(posterior_h_pedestrian, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                          "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                          "b_high_walkable_path_scaled", "ar[1]","sd_no_ipc__Intercept",
                                          "sigma"))



#noise hours
prior_noise<-c(set_prior("normal(0,320)",class="b"))

CES_spatial_ind$low_noise_scaled<-as.numeric(CES_spatial_ind$low_noise_scaled)
Fix3_socio_noise<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                        low_noise_scaled +
                        ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                      family="gaussian", iter=100000, chains=4, prior=prior_noise,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_noise<-parameters::model_parameters(Fix3_socio_noise,ci = 0.95,
                                                    ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_noise

sink("tidy_Fix3_socio_noise.txt")
print(tidy_Fix3_socio_noise)
sink()

posterior_h_noise <- as.array(Fix3_socio_noise)

color_scheme_set("red")
mcmc_intervals(posterior_h_noise, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                                "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                                "b_low_noise_scaled", "ar[1]","sd_no_ipc__Intercept",
                                                "sigma"))



#traffic hours
prior_traffic<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_traffic<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                          low_speed_scaled +
                          ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                        family="gaussian", iter=100000, chains=4, prior=prior_traffic,control = list(adapt_delta = 0.99))




tidy_Fix3_socio_traffic<-parameters::model_parameters(Fix3_socio_traffic,ci = 0.95,
                                                      ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_traffic

sink("tidy_Fix3_socio_traffic.txt")
print(tidy_Fix3_socio_traffic)
sink()

posterior_h_traffic <- as.array(Fix3_socio_traffic)

color_scheme_set("red")
mcmc_intervals(posterior_h_traffic, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                           "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                           "b_low_speed_scaled", "ar[1]","sd_no_ipc__Intercept",
                                           "sigma"))




#aged hours
prior_aged<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_aged<-brm(formula=Response ~ 1 + Question +no_ipc+prev_CES8_ok+ Covid+
                       high_aged_scaled + 
                       ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                     family="gaussian", iter=100000, chains=4, prior=prior_aged,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_aged<-parameters::model_parameters(Fix3_socio_aged,ci = 0.95,
                                               ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_aged
sink("tidy_Fix3_socio_aged.txt")
print(tidy_Fix3_socio_aged)
sink()


posterior_h_aged <- as.array(Fix3_socio_aged)

color_scheme_set("red")
mcmc_intervals(posterior_h_aged, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                             "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                             "b_high_aged_scaled", "ar[1]","sd_no_ipc__Intercept",
                                             "sigma"))



#density hours
prior_density<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_density<-brm(formula=Response ~ 1 + Question +no_ipc+prev_CES8_ok+Covid+
                          high_density_scaled +
                          ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                        family="gaussian", iter=100000, chains=4, prior=prior_density,control = list(adapt_delta = 0.99))




tidy_Fix3_socio_density<-parameters::model_parameters(Fix3_socio_density,ci = 0.95,
                                                      ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_density

sink("tidy_Fix3_socio_density.txt")
print(tidy_Fix3_socio_density)
sink()

posterior_h_density <- as.array(Fix3_socio_density)

color_scheme_set("red")
mcmc_intervals(posterior_h_density, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                          "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                          "b_high_density_scaled", "ar[1]","sd_no_ipc__Intercept",
                                          "sigma"))



#income hours
prior_income<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_income<-brm(formula=Response ~ 1 + Question +no_ipc+prev_CES8_ok+Covid+
                         high_income_scaled + 
                         ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                       family="gaussian", iter=100000, chains=4, prior=prior_density,control = list(adapt_delta = 0.99))




tidy_Fix3_socio_income<-parameters::model_parameters(Fix3_socio_density,ci = 0.95,
                                                     ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_income

sink("tidy_Fix3_socio_income.txt")
print(tidy_Fix3_socio_income)
sink()

posterior_h_income <- as.array(Fix3_socio_income)

color_scheme_set("red")
mcmc_intervals(posterior_h_income, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                             "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                             "b_high_income_scaled", "ar[1]","sd_no_ipc__Intercept",
                                             "sigma"))



#tpurp: trip purpose/mode de dtransport


prior_tpurp<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_tpurp<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                        tpurp_mode + 
                        ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                      family="gaussian", iter=100000, chains=4, prior=prior_tpurp,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_tpurp<-parameters::model_parameters(Fix3_socio_tpurp,ci = 0.95,
                                                    ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_tpurp

sink("tidy_Fix3_socio_tpurp.txt")
print(tidy_Fix3_socio_tpurp)
sink()

posterior_tpurp <- as.array(Fix3_socio_tpurp)

color_scheme_set("red")
mcmc_intervals(posterior_tpurp, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                             "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                             "b_tpurp_mode", "ar[1]","sd_no_ipc__Intercept",
                                             "sigma"))




#in/out 
prior_in_out<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_in_out<-brm(formula=Response ~ 1 + Question +no_ipc+ prev_CES8_ok+Covid+
                         nbsat +
                         ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                       family="gaussian", iter=100000, chains=4, prior=prior_in_out,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_in_out<-parameters::model_parameters(Fix3_socio_in_out,ci = 0.95,
                                                     ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_in_out

sink("tidy_Fix3_socio_in_out.txt")
print(tidy_Fix3_socio_in_out)
sink()

posterior_in_out <- as.array(Fix3_socio_in_out)

color_scheme_set("red")
mcmc_intervals(posterior_in_out, pars = c("b_Intercept" ,"b_QuestionCES_Q1","b_QuestionCES_Q2","b_QuestionCES_Q3",
                                             "b_QuestionCES_Q5","b_QuestionCES_Q6","b_QuestionCES_Q7","b_QuestionCES_Q8",
                                             "b_nbsat", "ar[1]","sd_no_ipc__Intercept",
                                             "sigma"))




###################################################
#MODEL4 ALL hours
prior_all<-c(set_prior("normal(0,320)",class="b"))

#only associate variables in previous model
Fix3_socio_all<-brm(formula=Response ~ 1 + Question +no_ipc+water_scaled+commerce_leisure_scaled+high_walkable_path_scaled+ tpurp_mode +
                      prev_CES8_ok+Covid+
                      participant_id_day+ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                    family="gaussian", iter=100000, chains=4, prior=prior_all,control = list(adapt_delta = 0.99))

save(Fix3_socio_all,file="Fix_socio_all.Rdata")

tidy_Fix3_socio_all<-parameters::model_parameters(Fix3_socio_all,ci = 0.95,
                                                  ci_method = "hdi",effects="all", digits=2)
tidy_Fix3_socio_all

sink("tidy_Fix3_socio_all.txt")
print(tidy_Fix3_socio_all)
sink()

#MODEL4 ALL hours manually backward 
prior_all<-c(set_prior("normal(0,320)",class="b"))

str(CES_spatial_ind)

#Model without nbsat 
Fix4_socio_all_bkw<-brm(formula=Response ~ 1 + Question +no_ipc+water_scaled+commerce_leisure_scaled+high_walkable_path_scaled+ tpurp_mode+
                          prev_CES8_ok+ Covid+
                          ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind,
                        family="gaussian", iter=100000, chains=4, prior=prior_all,control = list(adapt_delta = 0.99))

save(Fix4_socio_all_bkw,file="Fix_socio_all_bkw.Rdata")

tidy_Fix4_socio_all_bkw<-parameters::model_parameters(Fix4_socio_all_bkw,ci = 0.95,
                                                      ci_method = "hdi",effects="all", digits=2)
tidy_Fix4_socio_all_bkw

sink("tidy_Fix4_socio_all_bkw.txt")
print(tidy_Fix4_socio_all_bkw)
sink()
summary(Fix4_socio_all_bkw)
waic(Fix4_socio_all_bkw)
loo(Fix4_socio_all_bkw)
conditional_effects(Fix4_socio_all_bkw)

library(ggplot2)
library(dplyr)
CES_spatial_ind %>%
  ggplot(aes(x = Response, y = commerce_leisure_scaled)) +
  geom_abline(intercept = fixef(Fix4_socio_all_bkw)[1], 
              slope     = fixef(Fix4_socio_all_bkw)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") 




######################################################################################
########################PREDICTION TABLE###############################################
#######################################################################################

#read table for prediction for each variable, calculate predictions and ggplot of results
predictions_water<-read.csv2("predictions_all_water.csv", sep = ";", header=TRUE)

predictions_M4_1_water<-as.data.frame(predict(Fix4_socio_all_bkw, newdata=predictions_water))

predictions_water$prediction<-predictions_M4_1_water$Estimate

predictions_w<-select(predictions_water,Question,prediction,water_scaled)

predictions_w<-
  as.data.frame(predictions_w %>%
                  group_by(Question,water_scaled)%>%
                  summarise(mean = mean(prediction)))

predictions_w<-reshape(predictions_w, idvar = "Question", timevar = "water_scaled", direction = "wide")

#summarise predictions
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

#save plot
png(file="mygraphic.png",width=400,height=350, res=65)

#library(ggplot2)
ggplot(predictions_w, aes(x=predictions, y=value)) + 
  geom_point()+
  geom_smooth(method=lm)


ggplot(predictions_w, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=1))+
  stat_smooth()
dev.off()



#predictions walkable path

d_pred_walk<-read.csv2("predictions_all_walk.csv", sep = ";", header=TRUE)

predictions_M4_1_walk<-as.data.frame(predict(Fix4_socio_all_bkw, newdata=d_pred_walk))

predictions_walk<-select(d_pred_walk,Question,prediction,high_walkable_path_scaled)


predictions_walk<-
  as.data.frame(predictions_walk %>%
                  group_by(Question,high_walkable_path_scaled)%>%
                  summarise(mean = mean(prediction)))

predictions_walk<-reshape(predictions_walk, idvar = "Question", timevar = "high_walkable_path_scaled", direction = "wide")


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

#prediction commerce_leisure
predictions_comm<-read.csv2("predictions_all_comm.csv", sep = ";", header=TRUE)

predictions_M4_1_comm<-as.data.frame(predict(Fix4_socio_all_bkw, newdata=predictions_comm))

predictions_c<-select(predictions_comm,Question,prediction,commerce_leisure_scaled)

predictions_c<-
  as.data.frame(predictions_c %>%
                  group_by(Question,commerce_leisure_scaled)%>%
                  summarise(mean = mean(prediction)))

predictions_c<-reshape(predictions_c, idvar = "Question", timevar = "commerce_leisure_scaled", direction = "wide")

predictions_c<-predictions_c %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

#from wide to long
library(tidyr)
predictions_c <- predictions_c %>% gather(predictions, value, -Question)

#remove lines not useful
predictions_c<-predictions_c[predictions_c$Question=="Total", ]

predictions_c$Question<-"commerce"

write.csv2(predictions_c,"pred_commerce.csv")



#all predictions

all_predictions<-rbind(predictions_w,predictions_walk,predictions_c)
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
png(file="predictions_fixed.png",width=600,height=500, res=120)
library(ggplot2)

ggplot(all_predictions, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=Environmental_factor))+
  geom_line(aes(linetype = Environmental_factor),size=0.75)+
  scale_linetype_manual(values=c("twodash","solid","dashed"))+
  ggtitle("a) all location points ") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("momentary mental well-bein")+
  xlab("minutes")+
  xlim(0,61)+
  coord_cartesian(ylim=c(20,24))+
  theme(legend.position="bottom", legend.box = "orizontal",legend.title = element_blank())

dev.off()

