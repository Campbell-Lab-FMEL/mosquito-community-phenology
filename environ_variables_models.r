library(dplyr)
library(tidyr)
library(readr)
library(lme4)
library(performance)
library(car)
library(lmerTest)
library(sjPlot)
library(ggplot2)

setwd("C:\\Users\\linds\\IL_prelim\\rob_code_cor_temp\\code_data_github")

#add early versus late phenology as species trait
mosquitoILphenomets_2_ <- read_csv("mosquitoILphenomets.csv")
mosquitoILphenomets_2_2 <- mosquitoILphenomets_2_ %>% group_by(species) %>% summarize(mean_timing=mean(fifty))
mosquitoILphenomets_2_3 <- mosquitoILphenomets_2_2 %>% mutate(agegroup = case_when(mean_timing > 216 ~ 'late',  mean_timing < 216 ~ 'early'))
mosquitoILphenomets_21 <- left_join(mosquitoILphenomets_2_, mosquitoILphenomets_2_3, by="species")

#compile the climate data
IL_LC_clim_sites_summaries <- read_csv("IL_LC_clim_sites_summaries.csv")
# winter_vars_01_20 <- read_csv("C:/Users/linds/IL_prelim/rob_code_cor_temp/winter_vars_01_20 (1).csv")
# winter_vars_01_20$season_bin <- rep("0")
site_lc_lookup <- read_csv("site_lc_lookup3.csv")
# winter_vars_01_20_join <- left_join(winter_vars_01_20 , site_lc_lookup, by=c("Loc_ID"))
IL_LC_clim_sites_summaries <- rename(IL_LC_clim_sites_summaries, lc_class = LC_class)
IL_LC_clim_sites_all <- IL_LC_clim_sites_summaries 
IL_LC_clim_sites_all2 <- IL_LC_clim_sites_all %>% drop_na(lc_class)
loc_IDs_filter <- unique(IL_LC_clim_sites_summaries$Loc_ID)
IL_LC_clim_sites_all3 <- subset(IL_LC_clim_sites_all2, Loc_ID %in% loc_IDs_filter)

#separate into groups
IL_LC_clim_sites_all2_Seas0 <- subset(IL_LC_clim_sites_all3, season_bin=="0")
IL_LC_clim_sites_all2_Seas1 <- subset(IL_LC_clim_sites_all3, season_bin=="1")
IL_LC_clim_sites_all2_Seas2 <- subset(IL_LC_clim_sites_all3, season_bin=="2")

#calculate group level climate summaries
IL_LC_clim_sites_summaries_Seas0_mean <- IL_LC_clim_sites_all2_Seas0  %>% group_by(lc_class,year) %>% summarize(avgcum_prec_w=mean(cum_prec), avgtmin_avg_w=mean(tmin_avg), avgtmax_avg_w=mean(tmax_avg), avgrhmin_avg_w=mean(rhmin_avg), avgrhmax_avg_w=mean(rhmax_avg), avgvpd_avg_w=mean(vpd_avg))

IL_LC_clim_sites_summaries_Seas1_mean <-  IL_LC_clim_sites_all2_Seas1 %>% group_by(lc_class,year) %>% summarize(avgcum_prec_s=mean(cum_prec), avgtmin_avg_s=mean(tmin_avg), avgtmax_avg_s=mean(tmax_avg), avgrhmin_avg_s=mean(rhmin_avg), avgrhmax_avg_s=mean(rhmax_avg), avgvpd_avg_s=mean(vpd_avg))

IL_LC_clim_sites_summaries_Seas2_mean <-  IL_LC_clim_sites_all2_Seas2 %>% group_by(lc_class,year) %>% summarize(avgcum_prec_f=mean(cum_prec), avgtmin_avg_f=mean(tmin_avg), avgtmax_avg_f=mean(tmax_avg), avgrhmin_avg_f=mean(rhmin_avg), avgrhmax_avg_f=mean(rhmax_avg), avgvpd_avg_f=mean(vpd_avg))

#join the group means back to a long table
IL_LC_clim_sites_summaries_allseas <- left_join(IL_LC_clim_sites_summaries_Seas0_mean, IL_LC_clim_sites_summaries_Seas1_mean, IL_LC_clim_sites_summaries_Seas2_mean, by=c("lc_class","year"))

IL_LC_clim_sites_summaries_s0s1 <- left_join(IL_LC_clim_sites_summaries_Seas0_mean, IL_LC_clim_sites_summaries_Seas1_mean, by=c("lc_class","year"))

IL_LC_clim_sites_summaries_all <- left_join(IL_LC_clim_sites_summaries_s0s1, IL_LC_clim_sites_summaries_Seas2_mean, by=c("lc_class","year"))

#do some renaming and add some things to match up the mosquito phenology and join
IL_LC_clim_sites_summaries_all <- rename(IL_LC_clim_sites_summaries_all, YEAR = year)
IL_LC_clim_sites_summaries_all$lc_class <- sub("^", "lc", IL_LC_clim_sites_summaries_all$lc_class)
mospheno_clim_summary <- left_join( mosquitoILphenomets_21 , IL_LC_clim_sites_summaries_all, by=c("lc_class","YEAR"))
mospheno_clim_summary2 <- mospheno_clim_summary 
mospheno_clim_summary3 <- mospheno_clim_summary2 %>% mutate(tavg_avg_s = (avgtmax_avg_s + avgtmin_avg_s)/2)
mospheno_clim_summary4 <- mospheno_clim_summary3 %>% mutate(tavg_avg_f = (avgtmax_avg_f + avgtmin_avg_f)/2)  

#scale variables

#fall_vars
mospheno_clim_summary4$avgvpd_avg_f_sc <- scale(mospheno_clim_summary4$avgvpd_avg_f)
mospheno_clim_summary4$avgcum_prec_f_sc <- scale(mospheno_clim_summary4$avgcum_prec_f)
mospheno_clim_summary4$tavg_avg_f_sc <- scale(mospheno_clim_summary4$tavg_avg_f)

#spring_vars
mospheno_clim_summary4$tavg_avg_s_sc <- scale(mospheno_clim_summary4$tavg_avg_s)
mospheno_clim_summary4$avgcum_prec_s_sc <- scale(mospheno_clim_summary4$avgcum_prec_s)
mospheno_clim_summary4$avgvpd_avg_s_sc <- scale(mospheno_clim_summary3$avgvpd_avg_s)


#read in .csv with overwintering trait included -- added trait outside of R but can be assigned easily in R

mospheno_clim_summary4<-read.csv("mospheno_clim_summary4_traits.csv",stringsAsFactors = F)


#onset_model
model_on <- lmer(fifteen ~ (overwinter + agegroup + avgcum_prec_s_sc + avgvpd_avg_s_sc + lc_class + tavg_avg_s_sc )^2 + (1|species) , data=mospheno_clim_summary4)
vif(model_on)
step(model_on)


model_on <- lmer(fifteen ~ overwinter + agegroup + avgcum_prec_s_sc + avgvpd_avg_s_sc + lc_class + tavg_avg_s_sc + (1 | species) + agegroup:avgcum_prec_s_sc + agegroup:lc_class + avgcum_prec_s_sc:lc_class + avgvpd_avg_s_sc:tavg_avg_s_sc + overwinter:avgcum_prec_s_sc, data=mospheno_clim_summary4)
r2_nakagawa(model_on)
summary(model_on)


### example code for plotting model outputs -- export and perform additional edits in image editing software


p2<-plot_model(model_on, type = "pred", terms = c("tavg_avg_s_sc","avgvpd_avg_s_sc","lc_class"),colors = c("cadetblue", "gold","orangered"),  title = "")
p2+theme_classic(base_size = 18, base_family = "arial", base_line_size = 0) 



#offset_model

model_off <- lmer(eightyfive ~ (overwinter + agegroup + avgcum_prec_f_sc + avgvpd_avg_f_sc + lc_class + tavg_avg_f_sc )^2  + (1|species) , data=mospheno_clim_summary4)
step(model_off)
vif(model_off)


model_off <- lmer(eightyfive ~ agegroup + avgcum_prec_f_sc + avgvpd_avg_f_sc + tavg_avg_f_sc + (1 | species) + agegroup:tavg_avg_f_sc + avgvpd_avg_f_sc:tavg_avg_f_sc, data=mospheno_clim_summary4)
vif(model_off)
r2_nakagawa(model_off)
summary(model_off)


### example code for plotting model outputs -- export and perform additional edits in image editing software

p3<-plot_model(model_off, type = "pred", terms = c("tavg_avg_f_sc","agegroup","avgcum_prec_f_sc"),colors = c("cadetblue", "gold","orangered"),  title = "")
p3+theme_classic(base_size = 18, base_family = "arial", base_line_size = 0) 



#peak_timing model

model_peak <- lmer(peak ~ (overwinter + agegroup + avgcum_prec_s_sc + avgvpd_avg_s_sc + lc_class + tavg_avg_s_sc)^2 +  (1|species) , data=mospheno_clim_summary4)
step(model_peak)
vif(model_peak)

model_peak <- lmer(peak ~ agegroup + avgcum_prec_s_sc + avgvpd_avg_s_sc + lc_class + tavg_avg_s_sc + (1 | species) + avgcum_prec_s_sc:lc_class + avgcum_prec_s_sc:tavg_avg_s_sc, data=mospheno_clim_summary4)
vif(model_peak)
r2_nakagawa(model_peak)

summary(model_peak)


### example code for plotting model outputs -- export and perform additional edits in image editing software

p5<-plot_model(model_peak, type = "pred", terms = c("avgcum_prec_s_sc","lc_class","avgvpd_avg_s_sc"),colors = c("cadetblue", "gold","orangered"),  title = "")
p5+theme_classic(base_size = 18, base_family = "arial", base_line_size = 0) 











