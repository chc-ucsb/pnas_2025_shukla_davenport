
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
#===============================================================================


setwd(dirRaw)

#EO Data Prepped by Shrad
d<-read.csv('monthly_precip_1981_to_2022_for_0.1_deg_buffer_for_dhs_kids_in_region.csv')
d<-select(d,dhsid:last_col())

#DHS data prepped by me (with some metrics in there)
setwd(dirRdat)


# Filter it for Just WA Countries -----------------------------------------

#--Also add in Wealth Quintile Variable from dhall (for PNAS revision)

load('07_heat_health_ALL_metrics_merged_with_dhs.Rdata')

# #dhs3 all observations with EO data
# #dhs3a filtered just for children who lived there long enough
# rm(dhs3)
#
# dhs<-filter(dhs3a,dhsid %in% d$dhsid) #filter just for WA countries
# dhs<-filter(dhs,country!='Niger')
# dhs<-droplevels(dhs)
# 
# #Select id and wealth variables from dhall
# # dhal2<-select(dhall,dhsid:dhsclust,idhspsu,kid,wealthq)
# # dhs2<-inner_join(dhs,dhal2)
# 
# #---Merge in Wealth Quintile---
# ##https://www.idhsdata.org/idhs-action/variables/WEALTHQHH#description_section

# met_dhs<-base::date()
# save(met_dhs,dhs,file='01_west_africa_DHS_filtered_for_WA_countries.Rdata')
load('01_west_africa_DHS_filtered_for_WA_countries.Rdata')



# Select Just Variables Needed for Merge ----------------------------------
dh<-select(dhs,dhsid,country:dhsyear,kid,hhid,kidbdate,intdate,kidbirthyr,kidbirthmo)

minyr<-min(dh$kidbirthyr)
minyr<-minyr-2
maxyr<-max(dh$kidbirthyr)
maxyr<-maxyr+6


# Do some filters on d -------------------------------------------------
#-
#-Convert to proper date object
d$date<-parse_date_time(d$date,orders='ymd')
d$year<-year(d$date)
d$month<-month(d$date)

#---Calculate Some DHSID level Means - may need to customize the years
refyrs<-1991:2020
dref<-filter(d,year %in% refyrs)
dref<-group_by(dref,dhsid,variable_name,month)
dref_month<-summarize(dref,month_mn=mean(value),month_sd=sd(value))

#--Monthly and Seasonal Means
dref<-filter(dref,month>=5,month<=9)  #May to September for Key lean season months
dref<-group_by(dref,dhsid,variable_name,year)
dref_season<-summarize(dref,season_sum=sum(value))

dref_season<-group_by(dref_season,dhsid,variable_name)
dref_season<-summarize(dref_season,season_mn=mean(season_sum),season_sd=sd(season_sum))

dref<-inner_join(dref_season,dref_month)

#--Seasonal Sums
d<-filter(d,month>=5,month<=9)
d<-group_by(d,dhsid,variable_name,year)
d<-mutate(d,season_sum=sum(value))

#--Filter out for Key Months and Years
d<-filter(d,year>=minyr,year<=maxyr)

#--Merge Ref values back in and calculate anomalies
#dref<-select(dref,dhsid:longnum,variable_name,month:season_sd)

d<-inner_join(d,dref)

#--Join and Do year-1 matches
dh<-inner_join(dh,d)
dh<-filter(dh,year==kidbirthyr-1) #obtain precip. values for prior year


# Center the Monthly and Seasonal Values ----------------------------------
dh$value<-dh$value-dh$month_mn
dh$season_sum<-dh$season_sum-dh$season_mn



# Write out? --------------------------------------------------------------
setwd(dirRdat)
met_dh<-base::date()
save(d,dref,dh,file='01_west_africa_kids_matched_to_pre_season_vars.Rdata')


