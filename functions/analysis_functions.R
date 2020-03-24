#This script contains functions to facilitate analysis



by_age_state <- function(df,occ_or_ind){
  if(occ_or_ind=="occ"){
    out <- df %>%
      group_by(age_grp,statecensus) %>%
      summarise_at(vars(starts_with("occ_")),~sum(.*wtfinl,na.rm = T)/svy_frames) %>%
      mutate(state=as.character(to_factor(statecensus)),
             geoid=as.character(zap_ipums_attributes(statecensus))) %>%
      select(-statecensus) %>%
      ungroup() %>%
      pivot_longer(cols = one_of(names(.)[names(.) %in% special.locations$occ]),names_to = "Occupation") 
  }
  
  if(occ_or_ind=="ind"){
    out <- df %>%
      group_by(age_grp,statecensus) %>%
      summarise_at(vars(starts_with("ind_")),~sum(.*wtfinl,na.rm = T)/svy_frames) %>%
      mutate(state=as.character(to_factor(statecensus)),
             geoid=as.character(zap_ipums_attributes(statecensus))) %>%
      select(-statecensus) %>%
      ungroup() %>%
      pivot_longer(cols = one_of(names(.)[names(.) %in% special.locations$ind]),names_to = "Industry") 
  }
  return(out)
  
}


by_age_cbsa <- function(df,occ_or_ind){
  if(occ_or_ind=="occ"){
    out <- df %>%
      group_by(age_grp,metfips) %>%
      summarise_at(vars(starts_with("occ_")),~sum(.*wtfinl,na.rm = T)/svy_frames) %>%
      inner_join(met_codes %>% rename(geoid=codes),
                 .,
                 by=c("geoid"="metfips")) %>%
      ungroup() %>%
      pivot_longer(cols = one_of(names(.)[names(.) %in% special.locations$occ]),names_to = "Occupation") 
  }
  
  if(occ_or_ind=="ind"){
    out <- df %>%
      group_by(age_grp,metfips) %>%
      summarise_at(vars(starts_with("ind_")),~sum(.*wtfinl,na.rm = T)/svy_frames) %>%
      inner_join(met_codes %>% rename(geoid=codes),
                 .,
                 by=c("geoid"="metfips")) %>%
      ungroup() %>%
      pivot_longer(cols = one_of(names(.)[names(.) %in% special.locations$ind]),names_to = "Industry") 
  }
  return(out)
  
}






