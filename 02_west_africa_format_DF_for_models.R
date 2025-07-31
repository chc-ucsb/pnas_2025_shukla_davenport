
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
#===============================================================================


setwd(dirRdat)
load('01_west_africa_kids_matched_to_pre_season_vars.Rdata')
# d is raw EO data aggregated to clusters, filtered for relevant years, includes means, sums, anomalies

# dh is d matched to kids, and filterd for year prior to kids birth

load('01_west_africa_DHS_filtered_for_WA_countries.Rdata')

#dhs is filtered for kids that lived there long enough and WA countries


# Make DH Wide ------------------------------------------------------------
dh$variable_name<-recode(dh$variable_name,'precipitation'='p')

#---Monthly Values
dhw<-select(dh,dhsid:longnum,variable_name,value,month,year)
dhw<-pivot_wider(dhw,names_from=c(variable_name,month),values_from=c(value))


# Seasonal Values ---------------------------------------------------------
dhws<-filter(dh,month==min(dh$month)) #pick one month to reduce duplicates-CHANGE THIS IF DIFFERENT COUNTRIES USING DIFFERENT MIN/MAX MONTHS
dhws<-select(dhws,dhsid:longnum,variable_name,year,season_mn:season_sum)
dhws<-pivot_wider(dhws,names_from=c(variable_name),values_from=c(season_sum))


# Merge Seasonal and Monthly Values Back In -------------------------------
dhw<-inner_join(dhw,dhws)
dhw<-rename(dhw,'p_seasonal'='p')


# Merge with Main DHS -----------------------------------------------------

#--Trim DHS a bit
dhw<-select(dhw,-latnum,-longnum)  #some lat, long lost when joining on it, 
dhs<-inner_join(dhs,dhw)


# Select out Only BW records -------------------------------------------
dhs<-filter(dhs,birthwtref!='not_recorded',!is.na(birthwt))  #lose some surveys with this


# Add in RS Electricity Measure -------------------------------------------
setwd(dirRaw)
delc<-read.csv('electric_access_for_dhsid.csv')
test<-filter(delc,is.na(max))

write.csv(test,'electic_access_missing.csv')
delc<-dplyr::select(delc,-X)
delc<-dplyr::rename(delc,el_max=max,el_min=min,el_mean=mean)
delc$el_mean<-round(delc$el_mean,0)
delc<-delc %>% 
  mutate_at(
    vars(starts_with('el')),~ifelse(is.na(.),0,.))
#delc<-na.omit(delc)

dhs<-left_join(dhs,delc)  #missing 4 records


# Write out ---------------------------------------------------------------
setwd(dirRdat)
met_dhs<-base::date()
save(met_dhs,dhs,file='02_west_africa_dhs_with_prior_season_records.Rdata')

