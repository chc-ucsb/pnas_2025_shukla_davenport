
#-------------------Base Setup--------------------------------------------------
rm(list=ls())

SERVER<-FALSE  #use if running models on the server

#---Set Project Directories
dirBase<-'~/Library/CloudStorage/Box-Box/'



#-Base Server Directories
dirBaseServer<-'/home/chc-frank/'  #store data in here
dirFTP<-'/home/chc-data-out/people/frank/'


#-Project Directories
dirProj<-paste0(dirBase,'Shukla_WA_PNAS/')

dirRdat<-paste0(dirProj,'west_africa_rdata/')
dirOut<-paste0(dirProj,'Figures/')

dirRaw<-paste0(dirProj,'Data/')

#-Server Directories--UPDATE THIS
dirRdatServer<-paste0(dirBaseServer,'aoa_rdata/')
dirFTPOut<-paste0(dirFTP,'aoa_out/')


#---------Libraries
#library(raster)
library(R.utils)
#library(rgeos)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(haven)


library(randomForest)
library(rpart)
library(caret)
library(CAST)
#===============================================================================


setwd(dirRdat)
load('09_west_africa_folds.Rdata')

#dh has country folds

load('09_west_africa_folds_TIME.Rdata')

#dht has time folds

####---Electricity Model Set Up


# Set up Base Models ------------------------------------------------------

#Just Basic Survey Variables--
mod0<-as.formula(birthwt~urban+electrc+kidbirthyr+kidbirthmo)
mod0RE<-as.formula(birthwt~urban+el_mean+kidbirthyr+kidbirthmo) #RS electricity measure

#Pure Elec. models
mod000RE<-as.formula(birthwt~el_mean+kidbirthyr+kidbirthmo)
mod000E<-as.formula(birthwt~electrc+kidbirthyr+kidbirthmo)


# Kitchen Sink of DHS Variables
mod0a<-update(mod0,.~.+marstat+educlvl+age+kidsex+kidtwin+kidbord)

#Just EO variables
mod1<-as.formula(birthwt~p_seasonal+count_d100_pre_birth+season_mn)

#Just Temp and X month in the season
mod2<-as.formula(birthwt~p_5+count_d100_pre_birth+season_mn)  #May

mod2A<-as.formula(birthwt~p_8+count_d100_pre_birth+season_mn) #August


#Survey and EO Variables
mod3<-update(mod0RE,.~.+p_seasonal+count_d100_pre_birth+season_mn)


#Survey and First Month precip Variables
mod4<-update(mod0RE,.~.+p_5+count_d100_pre_birth+season_mn)  #MAY

mod4A<-update(mod0RE,.~.+p_8+count_d100_pre_birth+season_mn) #August

#Just Climate Means
mod5<-as.formula(birthwt~p_5+season_mn)  #Just the Seasonal Mean


#--CLIMATE PLUS PURE RELEC MODELS
#Survey and EO Variables
mod6RE<-update(mod000RE,.~.+p_seasonal+count_d100_pre_birth+season_mn)


#Survey and First Month precip Variables
mod7RE<-update(mod000RE,.~.+p_5+count_d100_pre_birth+season_mn)  #MAY

mod8RE<-update(mod000RE,.~.+p_8+count_d100_pre_birth+season_mn) #August




# Try a model -------------------------------------------------------------
fun.train<-function(df,ind,mod,mod_meth,space_time_fold=TRUE){  #train slower than regular rf
  fit_train<-caret::train(form=mod,
                          data=df,method=mod_meth,
                          tuneGrid=data.frame("mtry"=2),
                          importance=TRUE,
                          trControl=trainControl(method="cv"),index=ind$index)
  return(fit_train)
}



# Run on Countries --------------------------------------------------------


tic<-Sys.time()  #6 min for. all countries, using mtry=2, on desktop

dh<-mutate(dh,mod_test0=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod0,mod_meth='rf'))) #base dhs model
dh<-mutate(dh,mod_test1=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod1,mod_meth='rf'))) #just temp and sesonal precip.
dh<-mutate(dh,mod_test2=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod2,mod_meth='rf'))) #just temp and xth month in season precip.
dh<-mutate(dh,mod_test3=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod3,mod_meth='rf'))) #all variables
dh<-mutate(dh,mod_test4=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod4,mod_meth='rf'))) #all variables with xth month in seaons

#--Adding in for Other Months in the Season
setwd(dirRdat)

#load('10_west_africa_trained_models.Rdata')

dh<-mutate(dh,mod_test2A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod2A,mod_meth='rf'))) #just temp and xth month in season precip. #-AUGUST

dh<-mutate(dh,mod_test4A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod4A,mod_meth='rf'))) #just temp and xth month in season precip. #-AUGUST


#DHS Kitchen Sink Model  #6 minutes to run these 2 on dekstop
#tic<-Sys.time()
dh<-mutate(dh,mod_test0A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod0a,mod_meth='rf'))) #Kitchen Sink DHS Model

#Seasonal Mean Model
dh<-mutate(dh,mod_test5=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod5,mod_meth='rf'))) #Seasonal Mean Model
# 


#Pure Elec Models
#load('10_west_africa_trained_models_w_seasonal_mean_RS_ELEC.Rdata')
#dhRE<-select(dhRE,-mod_test6E,-mod_test7RE)  #need to remove cause renaming models
dhRE<-mutate(dhRE,mod_test000E=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod000E,mod_meth='rf'))) #Seasonal Mean Model

dhRE<-mutate(dhRE,mod_test000RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod000RE,mod_meth='rf'))) #Seasonal Mean Model

dhRE<-mutate(dhRE,mod_test8RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod6RE,mod_meth='rf'))) #Seasonal Mean Model

dhRE<-mutate(dhRE,mod_test9RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod7RE,mod_meth='rf'))) #Seasonal Mean Model

dhRE<-mutate(dhRE,mod_test10RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod8RE,mod_meth='rf'))) #Seasonal Mean Model




tocDHmods<-Sys.time()-tic

setwd(dirRdat)
met_dhmodsELEC<-base::date()

#dhRE<-dh
#save(dh,dhs,tocDHmods,file='10_west_africa_trained_models.Rdata')
save(dhRE,met_dhmodsELEC,tocDHmods,file='10_west_africa_trained_models_w_seasonal_mean_RS_ELEC.Rdata')  #



# Update Older File with New Models ---------------------------------------
#-Old file
# load(file='10_west_africa_trained_models.Rdata')
# dh_old<-dh
# rm(dh)
# 
# #New file
# load(file='10_west_africa_trained_models_w_seasonal_mean.Rdata')
# 
# #Diff. Collunms
# vars<-setdiff(names(dh_old),names(dh))





# Run on Survey Years----------------------------------------------
dht<-filter(dht,country!='Senegal') #only 1 year for Senegal

tic<-Sys.time()  #11 hours for. all countries, all models, using mtry=2, on desktop

dht<-mutate(dht,mod_test0=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod0,mod_meth='rf'))) #base dhs model
dht<-mutate(dht,mod_test1=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod1,mod_meth='rf'))) #just temp and sesonal precip.
dht<-mutate(dht,mod_test2=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod2,mod_meth='rf'))) #just temp and xth month in season precip.
dht<-mutate(dht,mod_test3=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod3,mod_meth='rf'))) #all variables
dht<-mutate(dht,mod_test4=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod4,mod_meth='rf'))) #all variables with xth month in seaons

dht<-mutate(dht,mod_test2A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod2A,mod_meth='rf'))) #just temp and xth month in season precip. #-AUGUST

dht<-mutate(dht,mod_test4A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod4A,mod_meth='rf'))) #just temp and xth month in season precip. #-AUGUST

dht<-mutate(dht,mod_test0A=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod0a,mod_meth='rf'))) #Kitchen Sink DHS Model

#Seasonal Mean Model
dht<-mutate(dht,mod_test5=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod5,mod_meth='rf'))) #Kitchen Sink DHS Model

#Pure Elec Models
#load('10_west_africa_trained_models_TIME_w_seasonal_mean_RS_ELEC.Rdata')
#dhtRE<-select(dhtRE,-mod_test6E,-mod_test7RE)  #need to remove cause renaming models
dhtRE<-mutate(dhtRE,mod_test000E=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod000E,mod_meth='rf'))) #Seasonal Mean Model

dhtRE<-mutate(dhtRE,mod_test000RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod000RE,mod_meth='rf'))) #Seasonal Mean Model

dhtRE<-mutate(dhtRE,mod_test8RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod6RE,mod_meth='rf'))) #Seasonal Mean Model

dhtRE<-mutate(dhtRE,mod_test9RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod7RE,mod_meth='rf'))) #Seasonal Mean Model

dhtRE<-mutate(dhtRE,mod_test10RE=map2(.x=data,.y=c_folds,~fun.train(.x,.y,mod=mod8RE,mod_meth='rf'))) #Seasonal Mean Model

tocTIMEmods<-Sys.time()-tic

setwd(dirRdat)
met_TIMEmodsELEC<-base::date()

#dhtRE<-dht
save(dhtRE,tocTIMEmods,met_TIMEmodsELEC,file='10_west_africa_trained_models_TIME_w_seasonal_mean_RS_ELEC.Rdata')

