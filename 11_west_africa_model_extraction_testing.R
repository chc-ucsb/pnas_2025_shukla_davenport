
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

#--Original Models
#load('10_west_africa_trained_models.Rdata')

load('10_west_africa_trained_models_w_seasonal_mean.Rdata')

#load('10_west_africa_trained_models_TIME.Rdata')  # dht within country survey based models

load('10_west_africa_trained_models_TIME_w_seasonal_mean.Rdata')

##--Revision Models

#-Wealth Q
#dhW
load('10_west_africa_trained_models_w_seasonal_mean_WEALTH.Rdata')
#dhtW
load('10_west_africa_trained_models_TIME_w_seasonal_mean_WEALTH.Rdata')

#-Only Electricity
#dhE
load('10_west_africa_trained_models_w_seasonal_mean_ELEC.Rdata')
#dhtE
load('10_west_africa_trained_models_TIME_w_seasonal_mean_ELEC.Rdata')

#-Remotely Sensed Electricity
#dhRE
load('10_west_africa_trained_models_w_seasonal_mean_RS_ELEC.Rdata')  #)
#dhtRE
load('10_west_africa_trained_models_TIME_w_seasonal_mean_RS_ELEC.Rdata')


#--Melt in Long form, so that can apply extraction row-wise
fun.melt<-function(df,mod_suf=FALSE,suffix){
  df<-pivot_longer(df,cols=starts_with('mod_test'),names_to='model_name',values_to='mod_test')
  
  if(mod_suf==TRUE){ #add a suffix to the model names
    df$model_name<-paste0(df$model_name,'&',suffix)
  }
  return(df)
}

dh<-fun.melt(dh)

dht<-fun.melt(dht)

#Electricity
dhE<-fun.melt(dhE,TRUE,'ElC')
dhtE<-fun.melt(dhtE,TRUE,'ElC')

#Wealth
dhW<-fun.melt(dhW,TRUE,'WLT')
dhtW<-fun.melt(dhtW,TRUE,'WLT')

#RS Elec
dhRE<-fun.melt(dhRE,TRUE,'RElC')
dhtRE<-fun.melt(dhtRE,TRUE,'RElC')


#---Bind together
dh<-rbind(dh,dhE,dhW,dhRE)
dht<-rbind(dht,dhtE,dhtW,dhtRE)

#--REVISION MODELS Melt in Long form, so that can apply extraction row-wise
#add suffix _Elec and _Wealth to model names. 
# rbind above
# in 19, extract suffix to create new model type variable (elec, wealth)
# figure out how to do re-coding of model names, maybe work model type into grouping variables?


# Extract Variable Importance ---------------------------------------------

fun.varImp<-function(mod){
  var_obj<-caret::varImp(mod)
  dvar<-var_obj$importance
  dvar$predictor<-row.names(dvar)
  return(dvar)
}

dh<-mutate(dh,mod_varimp=map(.x=mod_test,~fun.varImp(.x)))

dht<-mutate(dht,mod_varimp=map(.x=mod_test,~fun.varImp(.x)))



# Make Model Predictions --------------------------------------------------
##-Extraction of caret::train objects is broken

##-Function for Making Predictions
fun.pred<-function(.x,.y,idvars=c('dhsid','dhscc','kid','hhid','birthwt'),by_year=FALSE){
  pred<-as.data.frame(predict(.x,newdata=.y))
  .y[,'pred']<-pred
  if(by_year==FALSE){.y<-.y[,c(idvars,'pred','country_train')]}
  if(by_year==TRUE){.y<-.y[,c(idvars,'pred','year_train')]}
  
  return(.y)
}




# Set up test data for Full Country Models ----------------------------------------------

dh$data_test<-dh$data  #set up template for writing over

for(i in 1:nrow(dh)){
  country_train<-dh$country[i]
  df<-dhs
  df$country_train<-country_train
  dh$data_test[[i]]<-df
  rm(df)
}

# Set Test Data for By Year Models ----------------------------------------------

dht$data_test<-dht$data  #set up template for writing over

for(i in 1:nrow(dht)){
  country_train<-dht$country[i]
  year_train<-dht$dhsyear[i]
  df<-filter(dhs,country==country_train)
  df$year_train<-year_train
  dht$data_test[[i]]<-df
  rm(df)
}

## Predict Values
dh<-mutate(dh,mod_pred=map2(.x=mod_test,.y=data_test,~fun.pred(.x,.y)))

dht<-mutate(dht,mod_pred=map2(.x=mod_test,.y=data_test,~fun.pred(.x,.y,by_year=TRUE)))


# Extract -----------------------------------------------------------------

#-By Country
dh<-select(dh,-data_test)
dhp<-select(dh,country,model_name,mod_pred)
dhp<-unnest(dhp,cols=c(model_name,mod_pred))
dhp<-ungroup(dhp)
dhp<-select(dhp,-country)  #must do this to match on out of sample values below

#-By Year
dht<-select(dht,-data_test)
dhpt<-select(dht,country,dhsyear,model_name,mod_pred)
dhpt<-unnest(dhpt,cols=c(model_name,mod_pred))
dhpt<-ungroup(dhpt)
dhpt<-select(dhpt,-dhsyear)  #must do this to match on out of sample values below




# Match with out of sample values -----------------------------------------
dhmin<-select(dhs,dhsid,country,urban,latnum,longnum,dhscc,kid,hhid,dhsyear,livelihood,electrc,floor,drinkwtr,wealthq,kidbirthmo,kidbirthyr,count_d100_pre_birth,p_seasonal,p_5:p_9,el_max:el_mean)

dhp<-inner_join(dhmin,dhp)
dhpt<-inner_join(dhmin,dhpt)

dhp$sample<-ifelse(dhp$country==dhp$country_train,'in','out')
dhpt$sample<-ifelse(dhpt$dhsyear==dhpt$year_train,'in','out')


#--Extract Variable Importance
dhv<-select(dh,country,model_name,mod_varimp)
dhv<-unnest(dhv,cols=c(model_name,mod_varimp))

#For within country mods
dhvt<-select(dht,country,dhsyear,model_name,mod_varimp)
dhvt<-unnest(dhvt,cols=c(model_name,mod_varimp))



# Calc Accuracy Scores ----------------------------------------------------
dhp<-rename(dhp,value=birthwt)  #generic name easeir to calcuate
dhpt<-rename(dhpt,value=birthwt)  #generic name easeir to calcuate


fun.pred<-function(da){
  da$error<-da$value-da$pred  #error= observed-predicted
  da$p_error<-round(da$error/da$value,3)  #percent_error= error/observed
  da$ab_error<-abs(da$error)  #absolute error=  |error|-  #take the mean of this to get MAE
  da$abp_error<-abs(da$p_error) #absolute percent error= |percent_error| #take mean of this to get MAPE
  return(da)
}

dhp<-fun.pred(dhp)
dhpt<-fun.pred(dhpt)


#---Set Model Types
#--Seperett out the seperator
dhp<-dhp %>% separate_wider_delim(model_name,delim='&',names=c('model_name',"model_type"),too_few='align_start',cols_remove=TRUE)

dhpt<-dhpt %>% separate_wider_delim(model_name,delim='&',names=c('model_name',"model_type"),too_few='align_start',cols_remove=TRUE)

#to do here

## save 19 as differnet files and change aggregations to reflect just electricity and one for wealth quartile

## watch for model_type and model names when doing joins on dh, dht, and other nested objects


# Write Out ---------------------------------------------------------------
setwd(dirRdat)
met_dhp<-base::date()
save(met_dhp,dh,dhp,dhv,file='11_west_africa_extracted_mod_results.Rdata')

met_dhpt<-base::date()
save(met_dhpt,dht,dhpt,dhvt,file='11_west_africa_extracted_mod_results_TIME.Rdata')




