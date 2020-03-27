#This script serves as a makefile for the project


##################################################
#Load preliminaries:

#Occupation codes for reference
if(!file.exists("cache/occ_codes.Rdata")){
  get_cps_occ_codes()
}
load("cache/occ_codes.Rdata")

#Industry codes for reference
if(!file.exists("cache/ind_codes.Rdata")){
  get_cps_ind_codes()
}
load("cache/ind_codes.Rdata")

#Industry selector
if(!file.exists("cache/selector_ind.Rdata")){
  build_selector(ind=T,occ=F)
}
load("cache/selector_ind.Rdata")

#Occupation selector
if(!file.exists("cache/selector_occ.Rdata")){
  build_selector(ind=F,occ=T)
}
load("cache/selector_occ.Rdata")

if(!file.exists("cache/met_codes.Rdata")){
  get_cps_metfips_codes()
}
load("cache/met_codes.Rdata")

if(!file.exists("cache/cps_data.Rdata")){
  source("code/01_import_data.R")
}
load("cache/cps_data.Rdata")

if(!file.exists("cache/child_needs.Rdata")){
  source("code/02_process_data.R")
}
load("cache/child_needs.Rdata")
  

############################################
#Summarize the data
message("Open 03_summarize_data.R to build out dataset.")