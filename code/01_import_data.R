#Build dataset

#############################
#Extract zipped data extract from ipums
if(!file.exists("inputs/cps_00006.dat")){
  gunzip("inputs/cps_00006.dat.gz",remove=F)
}


#################################
#Read in data
data_in <- read_ipums_micro(ddi="inputs/cps_00006.xml",
                            data_file = "inputs/cps_00006.dat") %>%
  rename_all(str_to_lower)

#Subset only CPS monthly data 
cps_data <- data_in %>%
  filter((asecflag!=1 | is.na(asecflag)),year>2017) %>%
  mutate_at(vars(occ,ind),~str_pad(.,4,"left",0)) %>%
  mutate_at(vars(occ,ind),~na_if(.,"0000")) %>%
  mutate(metfips=str_pad(metfips,5,"left","0"),
         earnweek=ifelse(earnweek==9999.99,NA,earnweek)) %>%
  left_join(.,selector_ind,by=c("ind"="ind_code")) %>%
  left_join(.,selector_occ,by=c("occ"="occ_code")) 
  

save(cps_data,file = "cache/cps_data.Rdata")

