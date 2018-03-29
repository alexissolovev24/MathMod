library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
library("ggplot2") 
library("dplyr")  
x = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
x = x[-1,] 
x 
glimpse(x) 
x = select(x, -(roll)) 
x<-x[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
x 
x = x %>% mutate_if(is.character, factor) 
names(x) = str_replace_all(names(x), "[!]","_emph_") 
names(x) = names(x) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(x) 
x=x[x$DOY > 151,99 & x$DOY < 244,] 
x 
sapply(x,is.numeric) 
x_numeric = x[,sapply(x,is.numeric)] 
x_numeric 
cor_x = cor(x_numeric) 
cor_x 
cor_x = cor(na.omit(x_numeric)) 
cor_x 
cor_x = cor(na.omit(x_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_x 
vars = row.names(cor_x)[cor_x$co2_flux^2 > .1] %>% na.exclude 
vars 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep = "")) 
formula 
row_numbers = 1:length(x$date) 
teach = sample(row_numbers, floor(length(x$date)*.7)) 
test = row_numbers[-teach] 
teaching_x_unq = x[teach,] 
testing_x_unq = x[test,] 
mod = lm(formula, data=x) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod) 
anova(mod) 
mod1=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+rand_err_H +LE+ rand_err_LE +co2_flux+rand_err_co2_flux+ rand_err_h2o_flux + H_strg + co2_molar_density +h2o_time_lag + sonic_temperature ++ air_temperature+air_density+air_molar_volume+es+RH+VPD+u_star_+TKE+T_star_+un_H+un_Tau+un_LE+un_co2_flux+un_h2o_flux+flowrate)^2,data=x)
mod1
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1) 
anova(mod1)
mod2=lm(co2_flux~(Tau+rand_err_Tau + H+rand_err_H +LE+ rand_err_LE + co2_flux+rand_err_co2_flux+ rand_err_h2o_flux  + air_temperature + air_density+ air_molar_volume+RH+T_star_+ un_H+un_LE+un_co2_flux+un_h2o_flux+flowrate)^2- co2_flux:co2_molar_density - co2_flux:rand_err_h2o_flux  - rand_err_LE:un_h2o_flux - rand_err_LE:un_co2_flux - rand_err_LE:un_LE - rand_err_LE:un_Tau - rand_err_LE:un_H - rand_err_LE:RH - rand_err_LE:es - co2_flux:u_star_ - rand_err_LE:air_temperature - rand_err_Tau - co2_flux:un_LE - rand_err_LE:H_strg - rand_err_LE:rand_err_h2o_flux - LE:un_h2o_flux - LE:un_co2_flux - LE:un_LE - LE:un_Tau - LE:un_H - LE:T_star_ - LE:u_star_ - rand_err_co2_flux:co2_molar_density - rand_err_co2_flux:un_H - rand_err_LE:T_star_ - co2_flux:rand_err_LE - LE:co2_molar_density - LE:rand_err_co2_flux - rand_err_H:un_co2_flux - rand_err_Tau - Tau:T_star_ - Tau:un_H - rand_err_Tau:rand_err_H - rand_err_Tau:rand_err_LE - rand_err_Tau:rand_err_co2_flux - rand_err_Tau:rand_err_h2o_flux - rand_err_Tau:H_strg- rand_err_Tau:air_temperature - rand_err_Tau:air_temperature - rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es - rand_err_Tau:RH  - rand_err_Tau:u_star_ - rand_err_Tau:un_H - rand_err_Tau:un_LE - rand_err_Tau:un_h2o_flux  - H:LE - H:u_star_ - H:un_H - H:un_Tau - H:un_LE - H:un_h2o_flux - co2_flux:rand_err_H - rand_err_h2o_flux:T_star_ - rand_err_h2o_flux:un_H -  rand_err_h2o_flux:un_co2_flux - co2_molar_density:un_co2_flux -       co2_molar_density:un_LE - co2_molar_density:un_Tau - co2_molar_density:T_star_ - co2_molar_density:RH - co2_molar_density:es - co2_molar_density:air_molar_volume - co2_molar_density:un_Tau - co2_molar_density:un_LE - co2_molar_density:un_co2_flux - h2o_time_lag:u_star_  - h2o_time_lag:RH - h2o_time_lag:es - h2o_time_lag:air_molar_volume - h2o_time_lag:air_density - h2o_time_lag:air_temperature - co2_molar_density:air_density - h2o_time_lag:un_co2_flux - h2o_time_lag:un_Tau - h2o_time_lag:un_H - h2o_time_lag:T_star_ - co2_molar_density:air_temperature - air_temperature:RH  - rand_err_h2o_flux:RH - rand_err_h2o_flux:es - rand_err_h2o_flux:air_temperature - air_density:es - air_temperature:un_h2o_flux - air_temperature:es - co2_molar_density:u_star_ - rand_err_co2_flux:un_Tau  - rand_err_H:un_Tau - rand_err_H:u_star_  - rand_err_H:RH - rand_err_H:air_molar_volume - rand_err_H:air_density - air_molar_volume:RH  - air_density:RH - co2_flux:un_co2_flux - rand_err_H:air_temperature - rand_err_H:co2_molar_density - rand_err_H:rand_err_co2_flux - es:RH - rand_err_Tau:un_Tau - rand_err_Tau:T_star_  - rand_err_Tau:co2_molar_density - rand_err_Tau:LE - rand_err_Tau:H  - Tau:un_h2o_flux - es:un_LE - rand_err_co2_flux:u_star_ - Tau:u_star_ - Tau:co2_molar_density - Tau:LE - es:un_h2o_flux - rand_err_H:T_star_ - Tau:rand_err_h2o_flux - Tau:rand_err_co2_flux - RH:un_h2o_flux - RH:un_LE - un_LE:un_h2o_flux - RH:flowrate - un_LE:un_co2_flux ,data=x)
mod2
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2) 
anova(mod2)

