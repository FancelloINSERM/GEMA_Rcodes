library(tidyr)
library(brms)
library(loo)
library(sjstats)
library(sjmisc)
library(mediation)
library(see)


CES_spatial_ind_open<-read.csv2("CES_spatial_ind_open.csv",
                                sep = ";", header=TRUE, stringsAsFactors = FALSE)

CES_spatial_ind_open$no_ipc<-as.factor(CES_spatial_ind_open$no_ipc)
CES_spatial_ind_open$participant_id_day<-as.factor(CES_spatial_ind_open$participant_id_day)

CES_spatial_ind_open$Question<-as.factor(CES_spatial_ind_open$Question)
CES_spatial_ind_open$Question <- relevel(CES_spatial_ind_open$Question, ref = "CES_Q4")

str(CES_spatial_ind_open$no_ipc)
CES_spatial_ind_open$prev_CES8_ok<-as.numeric(CES_spatial_ind_open$prev_CES8_ok)

options(digits=2)
str(CES_spatial_ind_open)

#Verify and Relevel the socioeconomic factors

CES_spatial_ind_open$employment<-as.factor(CES_spatial_ind_open$employment)
summary(CES_spatial_ind_open$employment)
CES_spatial_ind_open$employment <- relevel(CES_spatial_ind_open$employment, ref = "employed")
str(CES_spatial_ind_open$employment)

CES_spatial_ind_open$sexe<-as.factor(CES_spatial_ind_open$sexe)
str(CES_spatial_ind_open$sexe)

CES_spatial_ind_open$marital_status<-as.factor(CES_spatial_ind_open$marital_status)
CES_spatial_ind_open$marital_status<-relevel(CES_spatial_ind_open$marital_status, ref="divorced/widowed/unmarried")
str(CES_spatial_ind_open$marital_status)

CES_spatial_ind_open$level_of_study<-as.factor(CES_spatial_ind_open$level_of_study)
CES_spatial_ind_open$level_of_study<-relevel(CES_spatial_ind_open$level_of_study, ref="Lower_education")
str(CES_spatial_ind_open$level_of_study)

CES_spatial_ind_open$age_class<-as.factor(CES_spatial_ind_open$age_class)
CES_spatial_ind_open$age_class<-relevel(CES_spatial_ind_open$age_class, ref="60-70")
str(CES_spatial_ind_open$age_class)


CES_spatial_ind_open$income_home<-as.factor(CES_spatial_ind_open$income_home)
str(CES_spatial_ind_open$income_home)

CES_spatial_ind_open$tpurp_mode<-as.factor(CES_spatial_ind_open$tpurp_mode)
CES_spatial_ind_open$tpurp_mode<-relevel(CES_spatial_ind_open$tpurp_mode, ref="residence")
str(CES_spatial_ind_open$tpurp_mode)



# test for time spent out
library(brms)
prior_time_out<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_time_out<-brm(formula=Response ~ 1 + Question +no_ipc+hours_out+
                           prev_CES8_ok+Covid+
                           ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                         family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_time_out<-parameters::model_parameters(Fix3_socio_time_out,ci = 0.95,
                                                       ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_time_out

sink("tidy_Fix3_socio_time_out.txt")
print(tidy_Fix3_socio_time_out)
sink()

#green hours
prior_green<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_green_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                             green +
                             ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                           family="gaussian", iter=100000, chains=4, prior=prior_green,control = list(adapt_delta = 0.99))




tidy_Fix3_socio_green_open<-parameters::model_parameters(Fix3_socio_green_open,ci = 0.95,
                                                         ci_method = "hdi",centrality="mean",effects="all", digits=2)
tidy_Fix3_socio_green_open

sink("tidy_Fix3_socio_green_open.txt")
print(tidy_Fix3_socio_green_open)
sink()


#water hours
prior_water<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_water_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                             water +
                             ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                           family="gaussian", iter=1000, chains=4, prior=prior_water,control = list(adapt_delta = 0.99))




tidy_Fix3_socio_water_open<-parameters::model_parameters(Fix3_socio_water_open,ci = 0.95,
                                                         ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_water_open

sink("tidy_Fix3_socio_water_open.txt")
print(tidy_Fix3_socio_water_open)
sink()

#landmarks hours
prior_monuments<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_monuments_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                                 monuments +
                                 ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                               family="gaussian", iter=100000, chains=4, prior=prior_monuments,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_monuments_open<-parameters::model_parameters(Fix3_socio_monuments_open,ci = 0.95,
                                                             ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_monuments_open

sink("tidy_Fix3_socio_monuments_open.txt")
print(tidy_Fix3_socio_monuments_open)
sink()

#commerce hours
prior_commerce<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_commerce_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                                commerce_leisure+
                                ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                              family="gaussian", iter=100000, chains=4, prior=prior_commerce,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_commerce_open<-parameters::model_parameters(Fix3_socio_commerce_open,ci = 0.95,
                                                            ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_commerce_open

sink("tidy_Fix3_socio_commerce_open.txt")
print(tidy_Fix3_socio_commerce_open)
sink()

#open hours
prior_open<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_open_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                            open +
                            ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                          family="gaussian", iter=100000, chains=4, prior=prior_open,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_open_open<-parameters::model_parameters(Fix3_socio_open_open,ci = 0.95,
                                                        ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_open_open

sink("tidy_Fix3_socio_open_open.txt")
print(tidy_Fix3_socio_open_open)
sink()

#pedestrian/street hours
prior_pedestrian<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_pedestrian_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                                  high_walkable_path + 
                                  ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                                family="gaussian", iter=100000, chains=4, prior=prior_pedestrian,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_pedestrian_open<-parameters::model_parameters(Fix3_socio_pedestrian_open,ci = 0.95,
                                                              ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_pedestrian_open

sink("tidy_Fix3_socio_pedestrian_open.txt")
print(tidy_Fix3_socio_pedestrian_open)
sink()


#noise hours
prior_noise<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_noise_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+ 
                             low_noise +
                             ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                           family="gaussian", iter=100000, chains=4, prior=prior_noise,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_noise_open<-parameters::model_parameters(Fix3_socio_noise_open,ci = 0.95,
                                                         ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_noise_open

sink("tidy_Fix3_socio_noise_open.txt")
print(tidy_Fix3_socio_noise_open)
sink()

#traffic hours
prior_traffic<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_traffic_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                               low_speed +
                               ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                             family="gaussian", iter=100000, chains=4, prior=prior_traffic,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_traffic_open<-parameters::model_parameters(Fix3_socio_traffic_open,ci = 0.95,
                                                           ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_traffic_open

sink("tidy_Fix3_socio_traffic_open.txt")
print(tidy_Fix3_socio_traffic_open)
sink()

#aged hours
prior_aged<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_aged_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                            high_aged +
                            ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                          family="gaussian", iter=100000, chains=4, prior=prior_aged,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_aged_open<-parameters::model_parameters(Fix3_socio_aged_open,ci = 0.95,
                                                        ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_aged_open

sink("tidy_Fix3_socio_aged_open.txt")
print(tidy_Fix3_socio_aged_open)
sink()

#density hours
prior_density<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_density_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                               high_density +
                               ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                             family="gaussian", iter=100000, chains=4, prior=prior_density,control = list(adapt_delta = 0.99))


tidy_Fix3_socio_density_open<-parameters::model_parameters(Fix3_socio_density_open,ci = 0.95,
                                                           ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_density_open

sink("tidy_Fix3_socio_density_open.txt")
print(tidy_Fix3_socio_density_open)
sink()


#income hours
prior_income<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_income_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                              high_income+
                              ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                            family="gaussian", iter=100000, chains=4, prior=prior_income,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_income_open<-parameters::model_parameters(Fix3_socio_income_open,ci = 0.95,
                                                          ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_income_open

sink("tidy_Fix3_socio_income_open.txt")
print(tidy_Fix3_socio_income_open)
sink()


#trip purpose/mode

prior_tpurp<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_tpurp_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                             tpurp_mode +
                             ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                           family="gaussian", iter=100000, chains=4, prior=prior_tpurp,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_tpurp_open<-parameters::model_parameters(Fix3_socio_tpurp_open,ci = 0.95,
                                                         ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_tpurp_open

sink("tidy_Fix3_socio_tpurp_open.txt")
print(tidy_Fix3_socio_tpurp_open)
sink()


#in/out
prior_in_out<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_in_out_open<-brm(formula=Response ~ 1 + Question +no_ipc +prev_CES8_ok+Covid+
                              nbsat + 
                              ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                            family="gaussian", iter=1000, chains=4, prior=prior_in_out,control = list(adapt_delta = 0.99))

tidy_Fix3_socio_in_out_open<-parameters::model_parameters(Fix3_socio_in_out_open,ci = 0.95,
                                                          ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_in_out_open

sink("tidy_Fix3_socio_in_out_open.txt")
print(tidy_Fix3_socio_in_out_open)
sink()


#MODEL4 ALL associated variables hours
library(brms)
prior_all<-c(set_prior("normal(0,320)",class="b"))

Fix3_socio_all_open<-brm(formula=Response ~ 1 + Question +no_ipc+prev_CES8_ok + Covid+
                           green+water+commerce_leisure+high_walkable_path +open+low_noise+high_aged+high_income+tpurp_mode + 
                           +ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                         family="gaussian", iter=1000000, chains=4, prior=prior_all,control = list(adapt_delta = 0.99))
tidy_Fix3_socio_all_open<-parameters::model_parameters(Fix3_socio_all_open,ci = 0.95,
                                                       ci_method = "hdi",effects="all", centrality="mean",digits=2)
tidy_Fix3_socio_all_open

summary(CES_spatial_ind_open$Response)

sink("tidy_Fix3_socio_all_open.txt")
print(tidy_Fix3_socio_all_open)
sink()

waic(Fix3_socio_all_open)

loo(Fix3_socio_all_open)


#backward regression: remove 1 determinant at time (the less associated to the outcome)

#removed income,green, open, high aged, low noise factor
Fix3_socio_all_open_bkw<-brm(formula=Response ~ 1 + Question +no_ipc+prev_CES8_ok+Covid+
                               water+commerce_leisure+ high_walkable_path+tpurp_mode+
                               ar(time=newTime1, gr= no_ipc),data = CES_spatial_ind_open,
                             family="gaussian", iter=100000, chains=4, prior=prior_all,control = list(adapt_delta = 0.99))
p_Fix3_socio_all_open_bkw<-parameters::model_parameters(Fix3_socio_all_open_bkw,ci = 0.95,
                                                                ci_method = "hdi",effects="all", centrality="mean",digits=2)

sink("p_Fix3_socio_all_open_bkw.txt")
print(p_Fix3_socio_all_open_bkw)
sink()



#############################################################################
#predictions water

predictions_water<-read.csv2("predictions_all_water.csv", sep = ";", header=TRUE)

predictions_M4_1_water<-as.data.frame(predict(p_Fix3_socio_all_open_bkw, newdata=predictions_water))

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


#predictions commerce_leisure

d_pred_comm<-read.csv2("predictions_all_comm.csv", sep = ";", header=TRUE)

d_pred_comm$tpurp_mode<-"residence"



predictions_M4_1_comm<-as.data.frame(predict(Fix3_socio_all_open_bkw, newdata=d_pred_comm))



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

#predictions walkable path

d_pred_walk<-read.csv2("predictions_all_walk.csv", sep = ";", header=TRUE)

d_pred_walk$tpurp_mode<-"residence"



predictions_M4_1_walk<-as.data.frame(predict(Fix3_socio_all_open_bkw, newdata=d_pred_walk))


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



######################################################################################

all_predictions<-rbind(predictions_comm,predictions_w,predictions_walk)
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
png(file="predictions_multi__fix_open.png",width=600,height=500, res=120)
library(ggplot2)

ggplot(all_predictions, aes(x=predictions, y=jitter(as.numeric(as.character(value))), group=Environmental_factor))+
  geom_line(aes(linetype = Environmental_factor),size=0.75)+
  scale_linetype_manual(values=c("twodash", "solid","dashed"))+
  ggtitle("b) outdoor location point") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("momentary mental well-being")+
  xlab("minutes")+
  xlim(0,61)+
  coord_cartesian(ylim=c(20,24))+
  theme(legend.position="bottom", legend.box = "orizontal",legend.title = element_blank())

dev.off()




#select one individual and see his predictions
d_pred_walk_sel<-d_pred_walk %>% filter(no_ipc==2080125)


d_pred_walk_sel<-
  as.data.frame(d_pred_walk_sel %>%
                  group_by(Question,high_walkable_path)%>%
                  summarise(mean = mean(prediction)))

predictions_walk<-reshape(predictions_walk, idvar = "Question", timevar = "high_walkable_path", direction = "wide")


predictions_walk<-predictions_walk %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))



