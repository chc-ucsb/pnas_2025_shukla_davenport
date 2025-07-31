
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
dirOutData<-paste0(dirProj,'Data_Out/')

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
load('02_west_africa_dhs_with_prior_season_records.Rdata')



# Select Key Variabes -----------------------------------------------------

dhs<-select(dhs,dhsid,country:hhid,livelihood,adm1name,floor:kidtwin,kidbirthyr:kidagedeath,days_post_birth:count_d105_post_birth,heat_waves_h3plus_d95_pre_birth:heat_waves_h3plus_d105_post_birth,days_exposure,days_residence,year:p_seasonal,el_max:el_mean)

dhs$birthwt<-as.numeric(dhs$birthwt)  #caret thinks it's a classfication problem if I do not do this

# Indicator if Child is Born in treatment period? -------------------------



# Custom Folds Function ---------------------------------------------------
fun.folds<-function(df,svar='livelihood',sd=1234,xcountry=FALSE){
  c_folds<-CAST::CreateSpacetimeFolds(df,spacevar=svar,timevar='year',seed=sd)
  return(c_folds)
}


# Nest By Country and/or Other Variables ----------------------------------
dh<-group_by(dhs,country) %>% nest() #later group by variable and counter
dh<-mutate(dh,c_folds=map(data,~fun.folds(.x,svar='livelihood')))


# Nest by Year ------------------------------------------------------------
dht<-group_by(dhs,country,dhsyear) %>% nest() #later group by variable and counter
dht<-mutate(dht,c_folds=map(data,~fun.folds(.x,svar='livelihood')))



# Write Out ---------------------------------------------------------------
setwd(dirRdat)
met_dh<-base::date()
save(dhs,dh,met_dh,file='09_west_africa_folds.Rdata')

met_dht<-base::date()
save(dht,met_dht,file='09_west_africa_folds_TIME.Rdata')


