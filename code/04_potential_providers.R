#Calculate numbers of potential childcare providers based on age and industry
#occupation codes then use srvyr to estimate totals

library(forcats)

to_svy <- function(df){
  #This function is a helper function to convert data frames or tibbles to an
  #object for srvyr to process
  #Args:
  #df: a dataframe or tibble coercable to a srvyr object with wtfinl
  
  #Dependencies
  require(srvyr)
  
  ###########################################################################
  #Specification comes from https://github.com/mnpopcenter/ipumsr/issues/50
  if("wtfinl" %in% colnames(df)){
    svy_out <- as_survey(df,weight = wtfinl,type = "JK1", scale = 4/60, rscales = rep(1, 160), mse = TRUE)
  }
  
}

###############

svy_frames <- nrow(distinct(cps_data,year,month))


# cps_recoded <- 
#   bind_cols(
#     cps_data %>%
#     map_dfc(special.locations,   #for intersections of industry and occupation, make this a map2_dfc and require both x==1 and y==1
#             function(x){
#               cps_data %>%
#                 transmute(parent=as.numeric((relate %in% c(101,201,202,203,1113,1114,1116,1117)) & !!as.name(x)==1)) %>%
#                 rename_all(~str_c(.,"_",x))
#             }) %>%
#       mutate_all(~ifelse(is.na(.),0,.))) %>%
#   group_by(hrhhid,hrhhid2,mish)

special.locations <- list(ind=selector_ind %>% select(-1) %>% names(),
                          occ=selector_occ %>% select(-1) %>% names())

cps_recoded <- cps_data %>%
  filter(labforce==2) %>%
  mutate(age_grp=cut(age,breaks = c(0,40,60,100))) 
  


pp_age_state <- by_age_state(cps_recoded,occ_or_ind="occ") %>%
  add_column(geography="All")



pp_age_msa <- by_age_cbsa(cps_recoded,occ_or_ind="occ") %>%
  rename(geography=description) %>%
  inner_join(.,us_msa %>% select(-geography))
  


#comma(sum(cps_data$wtfinl)/26)
 
############################################
#For power bi
to_pbi <- bind_rows(
  pp_age_state,
  pp_age_msa
) %>%
  mutate(Tier=case_when(
    Tier=="tier_1" ~ "Tier 1",
    Tier=="tier_2" ~ "Tier 2"
  ),
  Age=as.character(age_grp),
  Age=case_when(
    Age=="(0,40]" ~ "Age < 40",
    Age=="(40,60]" ~ "Age 41-60",
    Age=="(60,100]" ~ "Age > 61"
  )) %>%
  select(-age_grp)

write_csv(to_pbi,path="outputs/potential_providers_pbi.csv")



##############################################
#maps
us_st <- us_states(resolution = "low") %>%
  filter(!(stusps %in% c("HI","AK","PR")))  %>%
  st_transform(5070) %>% 
  select(state=name)

g.pp_age_state <- inner_join(us_st,
                             pp_age_state,
                             by=c("state"="description"))

mapview::mapview(g.pp_age_state,
                 #values = occ_preschool,
                 zcol = "occ_preschool")
